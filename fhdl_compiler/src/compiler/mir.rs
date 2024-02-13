use std::{collections::VecDeque, convert::identity, iter, ops::Deref};

use fhdl_netlist::{
    const_val::ConstVal,
    net_list::ModuleId,
    node::{Mux, Pass, Splitter},
    symbol::Symbol,
};
use itertools::Itertools as _;
use rustc_data_structures::{fx::FxHashSet, graph::WithSuccessors};
use rustc_hir::{def::DefKind, def_id::DefId, definitions::DefPathData};
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        AggregateKind, BasicBlock, BasicBlocks, Body, BorrowKind, Const, ConstOperand,
        ConstValue, Local, LocalDecl, Operand, Place, PlaceElem, Promoted, Rvalue,
        StatementKind, TerminatorKind, VarDebugInfoContents, RETURN_PLACE, START_BLOCK,
    },
    ty::{GenericArgsRef, InstanceDef, List, ParamEnv, ParamEnvAnd, Ty, TyKind},
};
use rustc_span::{def_id::LOCAL_CRATE, Span};
use rustc_target::abi::FieldIdx;
use tracing::{debug, error};

use super::{
    context::Switch,
    item::{CombineOutputs, Group, Item},
    item_ty::{ItemTy, ItemTyKind},
    Closure, Compiler, Context, MonoItem, SymIdent,
};
use crate::{
    blackbox::bin_op::BinOp,
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

#[derive(Debug, Clone, Copy)]
pub enum ClosureOrFnDid {
    Closure(DefId),
    Fn(DefId),
}

impl ClosureOrFnDid {
    fn did(&self) -> DefId {
        match self {
            Self::Closure(did) => *did,
            Self::Fn(did) => *did,
        }
    }
}

struct BranchBlocks<'b, 'tcx: 'b> {
    stop: BasicBlock,
    basic_blocks: &'b BasicBlocks<'tcx>,
    to_visit: VecDeque<BasicBlock>,
}

impl<'b, 'tcx: 'b> BranchBlocks<'b, 'tcx> {
    fn new(
        basic_blocks: &'b BasicBlocks<'tcx>,
        branch: BasicBlock,
        stop: BasicBlock,
    ) -> Self {
        Self {
            stop,
            basic_blocks,
            to_visit: {
                let mut res = VecDeque::new();
                res.push_back(branch);
                res
            },
        }
    }
}

impl<'b, 'tcx: 'b> Iterator for BranchBlocks<'b, 'tcx> {
    type Item = BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(block) = self.to_visit.pop_front() {
            if block == self.stop {
                continue;
            }

            self.to_visit.extend(self.basic_blocks.successors(block));

            return Some(block);
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefIdOrPromoted<'tcx> {
    DefId(DefId, InstanceDef<'tcx>),
    Promoted(DefId, Promoted),
}

impl<'tcx> DefIdOrPromoted<'tcx> {
    fn did(&self) -> DefId {
        match self {
            Self::DefId(did, _) => *did,
            Self::Promoted(did, _) => *did,
        }
    }
}

impl<'tcx> From<(DefId, InstanceDef<'tcx>)> for DefIdOrPromoted<'tcx> {
    fn from((def_id, instance_def): (DefId, InstanceDef<'tcx>)) -> Self {
        Self::DefId(def_id, instance_def)
    }
}

impl<'tcx> From<DefId> for DefIdOrPromoted<'tcx> {
    fn from(def_id: DefId) -> Self {
        Self::DefId(def_id, InstanceDef::Item(def_id))
    }
}

impl<'tcx> From<(DefId, Promoted)> for DefIdOrPromoted<'tcx> {
    fn from((def_id, promoted): (DefId, Promoted)) -> Self {
        Self::Promoted(def_id, promoted)
    }
}

impl<'tcx> Compiler<'tcx> {
    pub fn visit_fn(
        &mut self,
        def_id_or_promoted: impl Into<DefIdOrPromoted<'tcx>>,
        fn_generics: GenericArgsRef<'tcx>,
        top_module: bool,
    ) -> Result<ModuleId, Error> {
        let def_id_or_promoted = def_id_or_promoted.into();

        let mono_item = MonoItem::new(def_id_or_promoted, fn_generics);

        #[allow(clippy::map_entry)]
        if !self.evaluated_modules.contains_key(&mono_item) {
            let fn_did = def_id_or_promoted.did();
            let span = self
                .tcx
                .def_ident_span(fn_did)
                .unwrap_or_else(|| self.tcx.def_span(fn_did));

            let mut module_sym = if let DefKind::Closure = self.tcx.def_kind(fn_did) {
                SymIdent::Closure.into()
            } else {
                self.module_name(fn_did)
            };

            let (mir, is_inlined) = match def_id_or_promoted {
                DefIdOrPromoted::DefId(fn_did, instance_def) => {
                    let mir = self.tcx.instance_mir(instance_def);
                    (mir, self.is_inlined(fn_did))
                }
                DefIdOrPromoted::Promoted(fn_did, promoted) => {
                    let promoted_mir = self.tcx.promoted_mir(fn_did);
                    let mir = &promoted_mir[promoted];
                    module_sym =
                        Symbol::new_from_args(format_args!("{}_promoted", module_sym));
                    (mir, true)
                }
            };

            if mir.basic_blocks.is_cfg_cyclic() {
                return Err(SpanError::new(SpanErrorKind::UnsupportedLoops, span).into());
            }

            let module_id = self.netlist.add_module(module_sym, top_module);

            let mod_span = self.span_to_string(span, fn_did);
            self.netlist.add_mod_span(module_id, mod_span);

            let fn_name = self.fn_name(fn_did);
            debug!("visit_fn ({fn_name}): start, module {}", module_id);
            if is_inlined {
                self.netlist[module_id].is_inlined = true;
            }

            let mut ctx = Context::new(fn_did, fn_generics, module_id, mir);

            let inputs = mir
                .local_decls
                .iter_enumerated()
                .skip(1)
                .take(mir.arg_count);
            let inputs = self.visit_fn_inputs(inputs, &mut ctx)?;
            for var_debug_info in &mir.var_debug_info {
                if let VarDebugInfoContents::Const(ConstOperand { const_, .. }) =
                    var_debug_info.value
                {
                    if let Some(arg_idx) = var_debug_info.argument_index {
                        ctx.add_const(const_, inputs[(arg_idx - 1) as usize].clone());
                    }
                }
            }

            self.visit_blocks(mir, None, None, &mut ctx)?;

            for var_debug_info in &mir.var_debug_info {
                let name = var_debug_info.name.as_str();
                let span = var_debug_info.source_info.span;
                match var_debug_info.value {
                    VarDebugInfoContents::Place(place) => {
                        let item = self.visit_place(&place, &ctx, span)?;
                        self.assign_names_to_item(name, &item, true);
                    }
                    VarDebugInfoContents::Const(ConstOperand { const_, .. }) => {
                        if let Some(item) = ctx.find_const(&const_) {
                            self.assign_names_to_item(name, item, true);
                        }
                    }
                }
            }

            let output = ctx.locals.get_mut(RETURN_PLACE);
            self.visit_fn_output(ctx.module_id, output);

            self.evaluated_modules.insert(mono_item, module_id);

            debug!("visit_fn ({fn_name}): end, module {}", module_id);
        }

        Ok(*self.evaluated_modules.get(&mono_item).unwrap())
    }

    fn module_name(&self, def_id: DefId) -> Symbol {
        let def_path = self.tcx.def_path(def_id);
        let mut name = String::new();

        let mut has_sep = false;
        if def_path.krate != LOCAL_CRATE {
            name.push_str(self.tcx.crate_name(def_path.krate).as_str());
            has_sep = true;
        };

        for data_path in &def_path.data {
            if let DefPathData::TypeNs(s) | DefPathData::ValueNs(s) = &data_path.data {
                if has_sep {
                    name.push('_');
                } else {
                    has_sep = true;
                }
                name.push_str(s.as_str());
            }
        }

        Symbol::new(&name)
    }

    pub fn visit_fn_inputs<'a>(
        &mut self,
        inputs: impl IntoIterator<Item = (Local, &'a LocalDecl<'tcx>)>,
        ctx: &mut Context<'tcx>,
    ) -> Result<Vec<Item<'tcx>>, Error>
    where
        'tcx: 'a,
    {
        inputs
            .into_iter()
            .map(|(local, local_decl)| {
                let item_ty = self.resolve_ty(
                    local_decl.ty,
                    ctx.generic_args,
                    local_decl.source_info.span,
                )?;

                Ok(self.make_input(local, item_ty, ctx))
            })
            .collect()
    }

    pub fn visit_fn_output(&mut self, mod_id: ModuleId, output: &mut Item<'tcx>) {
        output.traverse(|node_out_id| {
            let node = &self.netlist[node_out_id.node_id()];

            // Add pass if node is input or already added as output
            let node_out_id = if node.is_input() || self.netlist.is_output(*node_out_id) {
                let ty = node.only_one_out().ty;
                let sym = node.only_one_out().sym;
                let new_node_out_id = self
                    .netlist
                    .add_and_get_out(mod_id, Pass::new(ty, *node_out_id, sym));
                *node_out_id = new_node_out_id;

                new_node_out_id
            } else {
                *node_out_id
            };

            self.netlist.add_output(node_out_id);
        });

        self.assign_names_to_item("out", output, false)
    }

    pub fn visit_blocks(
        &mut self,
        mir: &Body<'tcx>,
        start: Option<BasicBlock>,
        end: Option<BasicBlock>,
        ctx: &mut Context<'tcx>,
    ) -> Result<(), Error> {
        let mut next = Some(start.unwrap_or(START_BLOCK));
        while let Some(block) = next {
            if let Some(end) = end {
                if block == end {
                    break;
                }
            }

            next = self.visit_block(mir, block, ctx)?;
        }

        Ok(())
    }

    fn visit_block(
        &mut self,
        mir: &Body<'tcx>,
        block: BasicBlock,
        ctx: &mut Context<'tcx>,
    ) -> Result<Option<BasicBlock>, Error> {
        let basic_blocks = &mir.basic_blocks;
        let block_data = &basic_blocks[block];

        for statement in &block_data.statements {
            let span = statement.source_info.span;

            match &statement.kind {
                StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => {}
                StatementKind::Assign(assign) => {
                    let rvalue = &assign.1;

                    let item: Option<Item> = match rvalue {
                        Rvalue::Ref(_, BorrowKind::Shared, place)
                        | Rvalue::Discriminant(place)
                        | Rvalue::CopyForDeref(place) => {
                            Some(self.visit_place(place, ctx, span)?)
                        }
                        Rvalue::Use(operand) => {
                            Some(self.visit_operand(operand, ctx, span)?)
                        }
                        Rvalue::BinaryOp(bin_op, operands)
                        | Rvalue::CheckedBinaryOp(bin_op, operands) => {
                            let lhs = self.visit_operand(&operands.0, ctx, span)?;
                            let rhs = self.visit_operand(&operands.1, ctx, span)?;

                            let ty = bin_op.ty(
                                self.tcx,
                                operands.0.ty(&mir.local_decls, self.tcx),
                                operands.1.ty(&mir.local_decls, self.tcx),
                            );
                            let output_ty =
                                self.resolve_ty(ty, ctx.generic_args, span)?;

                            let bin_op = BinOp::try_from_op(*bin_op, span)?;

                            if let Rvalue::CheckedBinaryOp(_, _) = rvalue {
                                ctx.add_checked(assign.0.local);
                            }
                            Some(bin_op.bin_op(self, &lhs, &rhs, output_ty, ctx, span)?)
                        }
                        Rvalue::Aggregate(aggregate_kind, fields) => match aggregate_kind
                            .deref()
                        {
                            AggregateKind::Tuple => {
                                let ty = rvalue.ty(&mir.local_decls, self.tcx);
                                let ty = self.resolve_ty(ty, ctx.generic_args, span)?;

                                Some(self.mk_struct(ty, fields, ctx, span)?)
                            }
                            AggregateKind::Adt(
                                variant_did,
                                variant_idx,
                                generic_args,
                                _,
                                field_idx,
                            ) if field_idx.is_none() => {
                                let generic_args =
                                    ctx.instantiate(self.tcx, *generic_args);
                                let ty = self.type_of(*variant_did, generic_args);

                                let variant_idx = *variant_idx;

                                let ty = self.resolve_ty(ty, ctx.generic_args, span)?;

                                match ty.kind() {
                                    ItemTyKind::Struct(_) => {
                                        Some(self.mk_struct(ty, fields, ctx, span)?)
                                    }
                                    ItemTyKind::Enum(enum_ty) => {
                                        let data_part = if fields.is_empty() {
                                            None
                                        } else {
                                            let ty =
                                                *enum_ty.by_variant_idx(variant_idx).ty;

                                            Some(self.mk_struct(ty, fields, ctx, span)?)
                                        };

                                        Some(self.enum_variant_to_bitvec(
                                            ctx.module_id,
                                            data_part,
                                            ty,
                                            variant_idx,
                                        ))
                                    }

                                    _ => None,
                                }
                            }
                            AggregateKind::Closure(closure_did, closure_generics) => {
                                Some(self.visit_closure(
                                    ClosureOrFnDid::Closure(*closure_did),
                                    closure_generics,
                                    fields,
                                    ctx,
                                    span,
                                )?)
                            }
                            _ => None,
                        },
                        _ => None,
                    };

                    let item = item.ok_or_else(|| {
                        error!("assign ({}): {rvalue:#?}", dump_rvalue_kind(rvalue));
                        SpanError::new(SpanErrorKind::NotSynthExpr, span)
                    })?;

                    self.assign(assign.0.local, &item, ctx)?;
                }
                _ => {
                    error!("statement: {statement:#?}");
                    return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
                }
            }
        }

        let terminator = block_data.terminator();
        let span = terminator.source_info.span;

        let next_block = match &terminator.kind {
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                fn_span,
                ..
            } => {
                let span = *fn_span;
                let inputs = args.iter().map(|arg| &arg.node);

                let item = match func {
                    Operand::Move(place) | Operand::Copy(place) => {
                        let fn_item = self.visit_place(place, ctx, span)?;

                        if fn_item.ty.is_closure_ty() {
                            let inputs = self.visit_operands(inputs, ctx, span)?;
                            let item =
                                self.instantiate_closure(&fn_item, &inputs, ctx, span)?;

                            Some(item)
                        } else {
                            None
                        }
                    }
                    Operand::Constant(const_) => {
                        let ty = ctx.instantiate(self.tcx, const_.ty());

                        if let TyKind::FnDef(fn_did, fn_generics) = ty.kind() {
                            let item = self.resolve_fn_call(
                                *fn_did,
                                fn_generics,
                                inputs,
                                ctx,
                                span,
                            )?;

                            Some(item)
                        } else {
                            None
                        }
                    }
                };

                match item {
                    Some(item) => {
                        self.assign(destination.local, &item, ctx)?;
                    }
                    None => {
                        error!(
                            "terminator ({}): {terminator:#?}",
                            dump_terminator_kind(&terminator.kind)
                        );
                        return Err(
                            SpanError::new(SpanErrorKind::NotSynthExpr, span).into()
                        );
                    }
                }

                *target
            }
            TerminatorKind::Return | TerminatorKind::Unreachable => None,
            TerminatorKind::Drop { target, .. }
            | TerminatorKind::Goto { target }
            | TerminatorKind::Assert { target, .. } => Some(*target),
            TerminatorKind::SwitchInt { discr, targets } => {
                let discr = self.visit_operand(discr, ctx, span)?;
                let target_blocks = targets.all_targets();

                let convergent_block = ctx
                    .immediate_post_dominator(basic_blocks, block)
                    .expect("Cannot find convergent block for switchInt targets");

                let (ignore, target_count, common_locals) = self.find_common_locals(
                    basic_blocks,
                    target_blocks,
                    convergent_block,
                );
                if !ignore {
                    if common_locals.is_empty() {
                        return Err(
                            SpanError::new(SpanErrorKind::NotSynthExpr, span).into()
                        );
                    }

                    ctx.push_switch(Switch::new(common_locals, discr, target_count));

                    for (value, target) in targets
                        .iter()
                        .map(|(value, target)| (Some(value), target))
                        .chain(iter::once((None, targets.otherwise())))
                    {
                        if basic_blocks[target].is_empty_unreachable() {
                            continue;
                        }
                        let switch = ctx.last_switch().unwrap();
                        switch.add_variant(value);

                        self.visit_blocks(
                            mir,
                            Some(target),
                            Some(convergent_block),
                            ctx,
                        )?;
                    }

                    let switch = ctx.pop_switch().unwrap();
                    let mod_id = ctx.module_id;

                    let (discr, discr_width) = match switch.discr.ty.kind() {
                        ItemTyKind::Enum(enum_ty) => {
                            let discr_ty = enum_ty.discr_ty();
                            let discr = self.to_bitvec(mod_id, &switch.discr);

                            (
                                Item::new(discr_ty, {
                                    let splitter = self.netlist.add_and_get_out(
                                        mod_id,
                                        Splitter::new(
                                            discr.node_out_id(),
                                            [(discr_ty.node_ty(), SymIdent::Discr)],
                                            None,
                                            true,
                                        ),
                                    );

                                    let span = self.span_to_string(span, ctx.fn_did);
                                    self.netlist.add_span(splitter, span);

                                    splitter
                                }),
                                enum_ty.discr_width(),
                            )
                        }
                        _ => (
                            self.to_bitvec(mod_id, &switch.discr),
                            switch.discr.ty.width(),
                        ),
                    };
                    let discr = discr.node_out_id();

                    let output_ty = Ty::new_tup_from_iter(
                        self.tcx,
                        switch.locals.iter().map(|local| mir.local_decls[*local].ty),
                    );
                    let output_ty = self.resolve_ty(output_ty, ctx.generic_args, span)?;

                    let default = switch.otherwise().map(|otherwise| {
                        let item =
                            Item::new(output_ty, Group::new(otherwise.values().cloned()));

                        self.to_bitvec(mod_id, &item).node_out_id()
                    });

                    let inputs = switch.variants().map(|variant| {
                        let case = ConstVal::new(variant.value, discr_width);

                        let item = Item::new(
                            output_ty,
                            Group::new(variant.locals.values().cloned()),
                        );
                        let input = self.to_bitvec(mod_id, &item).node_out_id();

                        (case, input)
                    });

                    let mux = Mux::new(
                        output_ty.to_bitvec(),
                        discr,
                        inputs,
                        default,
                        SymIdent::Mux,
                    );
                    let mux = self.netlist.add_and_get_out(mod_id, mux);

                    let node_span = self.span_to_string(span, ctx.fn_did);
                    self.netlist.add_span(mux, node_span);

                    let mux = self.from_bitvec(mod_id, mux, output_ty);

                    for (local, item) in switch.locals.iter().zip(&*mux.group().items()) {
                        self.assign(*local, item, ctx)?;
                    }
                }

                Some(convergent_block)
            }
            _ => {
                error!(
                    "terminator ({}): {terminator:#?}",
                    dump_terminator_kind(&terminator.kind)
                );
                return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
            }
        };

        Ok(next_block)
    }

    fn assign(
        &mut self,
        local: Local,
        item: &Item<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Result<(), Error> {
        match ctx.last_switch() {
            Some(switch) if switch.contains(local) => {
                switch.add_variant_item(local, item.clone());
            }
            _ => {
                ctx.locals.place(local, item.clone());
            }
        }
        Ok(())
    }

    fn mk_struct(
        &mut self,
        item_ty: ItemTy<'tcx>,
        fields: &IndexVec<FieldIdx, Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        Ok(Item::new(
            item_ty,
            Group::try_new(
                fields
                    .iter()
                    .map(|field| self.visit_operand(field, ctx, span)),
            )?,
        ))
    }

    fn mk_const(
        &mut self,
        ty: Ty<'tcx>,
        value: u128,
        ctx: &Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let ty = self.resolve_ty(ty, ctx.generic_args, span)?;

        Ok(Item::new(
            ty,
            self.netlist.const_val(ctx.module_id, ty.to_bitvec(), value),
        ))
    }

    fn visit_operands<'a>(
        &mut self,
        operands: impl IntoIterator<Item = &'a Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Vec<Item<'tcx>>, Error>
    where
        'tcx: 'a,
    {
        operands
            .into_iter()
            .map(|operand| self.visit_operand(operand, ctx, span))
            .collect()
    }

    fn visit_operand(
        &mut self,
        operand: &Operand<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                self.visit_place(place, ctx, span)
            }
            Operand::Constant(value) => {
                let span = value.span;

                match value.const_ {
                    Const::Ty(const_) => {
                        if let Some(value) = ctx
                            .instantiate(self.tcx, const_)
                            .try_eval_scalar_int(self.tcx, ParamEnv::reveal_all())
                            .and_then(utils::eval_scalar_int)
                        {
                            return self.mk_const(const_.ty(), value, ctx, span);
                        }
                    }
                    Const::Val(const_value, ty) => match const_value {
                        ConstValue::Scalar(scalar) => {
                            if let Some(value) = utils::eval_scalar(scalar) {
                                return self.mk_const(ty, value, ctx, span);
                            }
                        }
                        ConstValue::ZeroSized => {
                            if let Some(item) = ctx.find_const(&value.const_) {
                                return Ok(item.clone());
                            }

                            if let TyKind::Closure(closure_did, closure_generics) =
                                ty.kind()
                            {
                                return self.visit_closure(
                                    ClosureOrFnDid::Closure(*closure_did),
                                    closure_generics,
                                    &IndexVec::new(),
                                    ctx,
                                    span,
                                );
                            }

                            if let TyKind::FnDef(fn_did, fn_generics) = ty.kind() {
                                return self.visit_closure(
                                    ClosureOrFnDid::Fn(*fn_did),
                                    fn_generics,
                                    &IndexVec::new(),
                                    ctx,
                                    span,
                                );
                            }

                            if let Ok(ty) = self.resolve_ty(ty, ctx.generic_args, span) {
                                if let Some(item) = self.mk_zero_sized_val(ty, ctx) {
                                    return Ok(item);
                                }
                            }
                        }
                        _ => {}
                    },
                    Const::Unevaluated(unevaluated, ty) => {
                        let ty = ctx.instantiate(self.tcx, ty);
                        if let Some(value) =
                            utils::resolve_unevaluated(self.tcx, unevaluated)
                        {
                            return self.mk_const(ty, value, ctx, span);
                        }

                        if let Some(promoted) = unevaluated.promoted {
                            let output_ty =
                                self.resolve_ty(ty, ctx.generic_args, span)?;
                            let fn_args = ctx.instantiate(self.tcx, unevaluated.args);

                            let module_id =
                                self.visit_fn((ctx.fn_did, promoted), fn_args, false)?;

                            let module =
                                self.instantiate_module(module_id, iter::empty(), ctx);

                            return Ok(self.combine_outputs(module, output_ty));
                        }
                    }
                }

                error!("operand value: {:#?}", value.const_);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    pub fn visit_place(
        &mut self,
        place: &Place<'tcx>,
        ctx: &Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let mut item = ctx.locals.get(place.local).clone();

        for place_elem in place.projection {
            item = match place_elem {
                PlaceElem::ConstantIndex {
                    offset, from_end, ..
                } => {
                    let array_ty = item.ty.array_ty();
                    let count = array_ty.count() as u64;
                    let offset = if from_end { count - offset } else { offset };

                    item.by_idx(offset as usize).clone()
                }
                PlaceElem::Deref => item.clone(),
                PlaceElem::Subtype(_) => item,
                PlaceElem::Field(idx, _) => {
                    if ctx.is_checked(place.local) {
                        item
                    } else {
                        item.by_field(idx).clone()
                    }
                }
                PlaceElem::Downcast(_, variant_idx) => {
                    let mod_id = ctx.module_id;

                    let enum_ty = item.ty.enum_ty();
                    let variant = self.to_bitvec(mod_id, &item);

                    self.enum_variant_from_bitvec(
                        mod_id,
                        variant.node_out_id(),
                        enum_ty,
                        variant_idx,
                    )
                }
                _ => {
                    error!("place elem: {place_elem:?} (local: {:?})", place.local);
                    return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
                }
            }
        }

        Ok(item)
    }

    fn find_common_locals(
        &self,
        basic_blocks: &BasicBlocks<'tcx>,
        targets: &[BasicBlock],
        convergent_block: BasicBlock,
    ) -> (bool, usize, FxHashSet<Local>) {
        fn search_along_branch(
            basic_blocks: &BasicBlocks<'_>,
            start: BasicBlock,
            convergent_block: BasicBlock,
        ) -> FxHashSet<Local> {
            let mut locals: FxHashSet<Local> = Default::default();

            for block in BranchBlocks::new(basic_blocks, start, convergent_block) {
                let block_data = &basic_blocks[block];
                for stmt in &block_data.statements {
                    if let Some((place, _)) = stmt.kind.as_assign() {
                        locals.insert(place.local);
                    }
                }
                if let TerminatorKind::Call { destination, .. } =
                    block_data.terminator().kind
                {
                    locals.insert(destination.local);
                }
            }

            locals
        }

        let mut common_locals =
            search_along_branch(basic_blocks, targets[0], convergent_block);
        let mut has_assign = !common_locals.is_empty();
        let mut count = 1;

        for target in &targets[1 ..] {
            if basic_blocks[*target].is_empty_unreachable() {
                continue;
            }

            let locals = search_along_branch(basic_blocks, *target, convergent_block);

            common_locals = common_locals
                .intersection(&locals)
                .copied()
                .collect::<FxHashSet<Local>>();
            has_assign |= !locals.is_empty();
            count += 1;
        }

        (!has_assign, count, common_locals)
    }

    pub fn resolve_fn_call<'a>(
        &mut self,
        fn_did: impl Into<DefId>,
        fn_generics: GenericArgsRef<'tcx>,
        inputs: impl IntoIterator<Item = &'a Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error>
    where
        'tcx: 'a,
    {
        let fn_did = fn_did.into();
        let fn_generics = ctx.instantiate(self.tcx, fn_generics);

        let fn_name = self.fn_name(fn_did);
        debug!("resolve_fn_call ({fn_name})");

        let (instance_did, instance_def, instance_generics) = self
            .tcx
            .resolve_instance(ParamEnvAnd {
                param_env: ParamEnv::reveal_all(),
                value: (fn_did, fn_generics),
            })
            .ok()
            .and_then(identity)
            .and_then(|instance| match instance.def {
                InstanceDef::Item(fn_did) => Some((fn_did, instance.def, instance.args)),
                InstanceDef::FnPtrShim(fn_did, _) => {
                    Some((fn_did, instance.def, instance.args))
                }
                _ => None,
            })
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthCall, span))?;

        let is_std_call = self.is_std_call(fn_did);

        if instance_did.is_local()
            || self.is_synth(instance_did)
            || self.is_synth(fn_did)
            || is_std_call
        {
            let inputs = self.visit_operands(inputs, ctx, span)?;
            let output_ty = self.fn_output(instance_did, instance_generics, span)?;

            let module_id =
                self.visit_fn((instance_did, instance_def), instance_generics, false)?;
            if is_std_call {
                self.netlist[module_id].is_inlined = true;
            }
            let module = self.instantiate_module(module_id, inputs.iter(), ctx);
            let span = self.span_to_string(span, ctx.fn_did);
            self.netlist.add_span(module, span);

            Ok(self.combine_outputs(module, output_ty))
        } else {
            let blackbox = self
                .find_blackbox(instance_did, span)
                .or_else(|e| self.find_blackbox(fn_did, span).map_err(|_| e))?;

            let old_fn_did = ctx.fn_did;
            let old_generics = ctx.generic_args;

            // Inputs are type folded with ctx.generic_args, not with instance_generics,
            // because they are defined in caller function scope.
            let inputs = self.visit_operands(inputs, ctx, span)?;

            // But for resolving output ty instance_generics are used as output ty related to
            // callee function scope.
            let output_ty = self.fn_output(instance_did, instance_generics, span)?;

            ctx.fn_did = instance_did;
            ctx.generic_args = instance_generics;

            let blackbox = blackbox.eval(self, &inputs, output_ty, ctx, span)?;

            ctx.fn_did = old_fn_did;
            ctx.generic_args = old_generics;

            Ok(blackbox)
        }
    }

    pub fn visit_closure(
        &mut self,
        closure_did: ClosureOrFnDid,
        closure_generics: GenericArgsRef<'tcx>,
        captures: &IndexVec<FieldIdx, Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let closure_generics = ctx.instantiate(self.tcx, closure_generics);
        let closure_ty = self.type_of(closure_did.did(), closure_generics);
        let closure_ty = self.resolve_ty(closure_ty, List::empty(), span)?;

        match self.find_closure_opt(closure_ty) {
            Some(closure) => Ok(closure.item.clone()),
            None => {
                let closure = self.mk_struct(closure_ty, captures, ctx, span)?;

                let closure_id =
                    self.visit_fn(closure_did.did(), closure_generics, false)?;

                let (input_tys, output_ty) = match closure_did {
                    ClosureOrFnDid::Closure(did) => {
                        self.netlist[closure_id].is_inlined = true;

                        let sig = self.fn_sig(did, closure_generics);

                        // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/struct.ClosureArgs.html
                        // ```
                        // CS represents the closure signature, representing as a fn() type.
                        // For example, fn(u32, u32) -> u32 would mean that the closure implements CK<(u32, u32), Output = u32>,
                        // where CK is the trait specified above
                        // ```
                        let input_tys = sig
                            .inputs()
                            .iter()
                            .map(|input_ty| {
                                self.resolve_ty(*input_ty, List::empty(), span)
                                    .map(|item_ty| item_ty.struct_ty().tys())
                            })
                            .flatten_ok()
                            .collect::<Result<Vec<_>, Error>>()?;

                        let output_ty =
                            self.resolve_ty(sig.output(), List::empty(), span)?;

                        (input_tys, output_ty)
                    }
                    ClosureOrFnDid::Fn(did) => {
                        let sig = self.fn_sig(did, closure_generics);

                        let input_tys = sig
                            .inputs()
                            .iter()
                            .map(|input_ty| {
                                self.resolve_ty(*input_ty, List::empty(), span)
                            })
                            .collect::<Result<Vec<_>, Error>>()?;

                        let output_ty =
                            self.resolve_ty(sig.output(), List::empty(), span)?;

                        (input_tys, output_ty)
                    }
                };

                self.add_closure(closure_ty, Closure {
                    closure_id,
                    input_tys,
                    output_ty,
                    item: closure.clone(),
                });

                Ok(closure)
            }
        }
    }

    pub fn instantiate_closure(
        &mut self,
        closure: &Item<'tcx>,
        inputs: &[Item<'tcx>],
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let Closure {
            closure_id,
            output_ty,
            ..
        } = self.find_closure(closure.ty, span)?;
        let closure_id = *closure_id;
        let output_ty = *output_ty;

        let module = self.instantiate_module(
            closure_id,
            iter::once(closure).chain(inputs.iter()),
            ctx,
        );

        let span = self.span_to_string(span, ctx.fn_did);
        self.netlist.add_span(module, span);

        let mut outputs = CombineOutputs::new(self, module);
        Ok(outputs.next_output(output_ty))
    }
}

fn dump_rvalue_kind(rvalue: &Rvalue) -> &'static str {
    match rvalue {
        Rvalue::Use(_) => "use",
        Rvalue::Repeat(_, _) => "repeat",
        Rvalue::Ref(_, _, _) => "ref",
        Rvalue::ThreadLocalRef(_) => "thread local ref",
        Rvalue::AddressOf(_, _) => "address_of",
        Rvalue::Len(_) => "len",
        Rvalue::Cast(_, _, _) => "cast",
        Rvalue::BinaryOp(_, _) => "binary_op",
        Rvalue::CheckedBinaryOp(_, _) => "checked binary_op",

        Rvalue::NullaryOp(_, _) => "nullary_op",
        Rvalue::UnaryOp(_, _) => "unary_op",
        Rvalue::Discriminant(_) => "discriminant",
        Rvalue::ShallowInitBox(_, _) => "shallow init box",
        Rvalue::CopyForDeref(_) => "copy_for_deref",
        Rvalue::Aggregate(_, _) => "aggregate",
    }
}

fn dump_terminator_kind(terminator: &TerminatorKind) -> &'static str {
    match terminator {
        TerminatorKind::Goto { .. } => "goto",
        TerminatorKind::SwitchInt { .. } => "switch int",
        TerminatorKind::UnwindResume => "unwind resume",
        TerminatorKind::UnwindTerminate(_) => "unwind terminate",
        TerminatorKind::Return => "return",
        TerminatorKind::Unreachable => "unreachable",
        TerminatorKind::Drop { .. } => "drop",
        TerminatorKind::Call { .. } => "call",
        TerminatorKind::Assert { .. } => "assert",
        TerminatorKind::Yield { .. } => "yield",
        TerminatorKind::CoroutineDrop => "coroutine drop",
        TerminatorKind::FalseEdge { .. } => "false edge",
        TerminatorKind::FalseUnwind { .. } => "false unwind",
        TerminatorKind::InlineAsm { .. } => "inline asm",
    }
}
