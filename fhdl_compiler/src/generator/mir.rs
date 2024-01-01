use std::{collections::VecDeque, convert::identity, iter, ops::Deref};

use fhdl_netlist::{
    bvm::BitVecMask,
    group::ItemId,
    net_list::ModuleId,
    node::{Case, Splitter},
    sig_ty::{NodeTy, SignalTy, SignalTyKind},
    symbol::Symbol,
};
use rustc_data_structures::{fx::FxHashSet, graph::WithSuccessors};
use rustc_hir::def_id::DefId;
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        AggregateKind, BasicBlock, BasicBlocks, Body, BorrowKind, Const, ConstOperand,
        ConstValue, Local, LocalDecl, Operand, Place, PlaceElem, Promoted, Rvalue,
        StatementKind, TerminatorKind, VarDebugInfoContents, RETURN_PLACE, START_BLOCK,
    },
    ty::{ClosureArgs, GenericArgsRef, InstanceDef, ParamEnv, ParamEnvAnd, TyKind},
};
use rustc_span::Span;
use rustc_target::abi::FieldIdx;

use super::{Generator, MonoItem};
use crate::{
    blackbox::bin_op::BinOp,
    error::{Error, SpanError, SpanErrorKind},
    eval_context::{Closure, EvalContext, ModuleOrItem, Switch},
    generator::bitvec::CombineOutputs,
    scopes::SymIdent,
    utils,
};

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

pub enum DefIdOrPromoted {
    DefId(DefId),
    Promoted(DefId, Promoted),
}

impl From<DefId> for DefIdOrPromoted {
    fn from(def_id: DefId) -> Self {
        Self::DefId(def_id)
    }
}

impl From<(DefId, Promoted)> for DefIdOrPromoted {
    fn from((def_id, promoted): (DefId, Promoted)) -> Self {
        Self::Promoted(def_id, promoted)
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn visit_fn_mir(
        &mut self,
        fn_did: impl Into<DefIdOrPromoted>,
        fn_generics: GenericArgsRef<'tcx>,
        top_module: bool,
    ) -> Result<ModuleId, Error> {
        let fn_did = fn_did.into();

        let (fn_did, mir, module_sym, is_inlined) = match fn_did {
            DefIdOrPromoted::DefId(fn_did) => {
                let mir = self.tcx.instance_mir(InstanceDef::Item(fn_did));
                let module_sym = if !self.tcx.is_closure(fn_did) {
                    self.module_name(fn_did)
                } else {
                    SymIdent::Closure.into()
                };
                (fn_did, mir, module_sym, self.is_inlined(fn_did))
            }
            DefIdOrPromoted::Promoted(fn_did, promoted) => {
                let promoted_mir = self.tcx.promoted_mir(fn_did);
                let mir = &promoted_mir[promoted];
                let module_sym = if !self.tcx.is_closure(fn_did) {
                    self.module_name(fn_did)
                } else {
                    SymIdent::Closure.into()
                };
                let module_sym =
                    Symbol::new_from_args(format_args!("{}_promoted", module_sym));
                (fn_did, mir, module_sym, true)
            }
        };

        if mir.basic_blocks.is_cfg_cyclic() {
            return Err(SpanError::new(
                SpanErrorKind::UnsupportedLoops,
                self.tcx
                    .def_ident_span(fn_did)
                    .unwrap_or_else(|| self.tcx.def_span(fn_did)),
            )
            .into());
        }

        let module_id = self.netlist.add_module(module_sym, top_module);
        if is_inlined {
            self.netlist[module_id].is_inlined = true;
        }

        let mut ctx =
            EvalContext::new(self.is_primary, fn_did, fn_generics, module_id, mir);

        let inputs = mir
            .local_decls
            .iter_enumerated()
            .skip(1)
            .take(mir.arg_count);
        let inputs = self.visit_fn_mir_inputs(inputs, &mut ctx)?;
        for var_debug_info in &mir.var_debug_info {
            if let VarDebugInfoContents::Const(ConstOperand { const_, .. }) =
                var_debug_info.value
            {
                if let Some(arg_idx) = var_debug_info.argument_index {
                    ctx.add_const(const_, inputs[(arg_idx - 1) as usize]);
                }
            }
        }

        self.visit_blocks(mir, None, None, &mut ctx)?;

        for var_debug_info in &mir.var_debug_info {
            let name = var_debug_info.name.as_str();
            let span = var_debug_info.source_info.span;
            match var_debug_info.value {
                VarDebugInfoContents::Place(place) => {
                    if let ModuleOrItem::Item(item_id) =
                        ctx.find_local(place.local, span)?
                    {
                        self.assign_names_to_item(name, item_id);
                    }
                }
                VarDebugInfoContents::Const(ConstOperand { const_, .. }) => {
                    if let Some(item_id) = ctx.find_const(&const_) {
                        self.assign_names_to_item(name, item_id);
                    }
                }
            }
        }

        let span = mir.local_decls[RETURN_PLACE].source_info.span;
        let output_id = ctx.find_local(RETURN_PLACE, span)?.item_id();
        self.eval_outputs(None, output_id);

        Ok(module_id)
    }

    fn module_name(&self, def_id: DefId) -> Symbol {
        let name = self
            .tcx
            .def_path_str(def_id)
            .replace("::", "_")
            .replace(['<', '>'], "");

        Symbol::new(&name)
    }

    pub fn visit_fn_mir_inputs<'a>(
        &mut self,
        inputs: impl IntoIterator<Item = (Local, &'a LocalDecl<'tcx>)>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<Vec<ItemId>, Error>
    where
        'tcx: 'a,
    {
        inputs
            .into_iter()
            .map(|(local, local_decl)| {
                let sig_ty =
                    self.find_sig_ty(local_decl.ty, ctx, local_decl.source_info.span)?;
                let item_id = self.make_input_with_sig_ty(sig_ty, ctx.module_id);
                ctx.add_local(local, item_id);

                Ok(item_id)
            })
            .collect()
    }

    pub fn visit_blocks(
        &mut self,
        mir: &Body<'tcx>,
        start: Option<BasicBlock>,
        end: Option<BasicBlock>,
        ctx: &mut EvalContext<'tcx>,
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
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<Option<BasicBlock>, Error> {
        let basic_blocks = &mir.basic_blocks;
        let block_data = &basic_blocks[block];

        for statement in &block_data.statements {
            let span = statement.source_info.span;

            match &statement.kind {
                StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => {}
                StatementKind::Assign(assign) => {
                    let rvalue = &assign.1;

                    let mod_or_item: Option<ModuleOrItem> = match rvalue {
                        Rvalue::Ref(_, BorrowKind::Shared, place)
                        | Rvalue::Discriminant(place) => {
                            let mod_or_item = ctx.find_local(place.local, span)?;
                            Some(match mod_or_item {
                                ModuleOrItem::Module(mod_id) => mod_id.into(),
                                ModuleOrItem::Item(item_id) => {
                                    self.visit_place(place, item_id, ctx, span)?.into()
                                }
                            })
                        }
                        Rvalue::Use(operand) => {
                            Some(self.visit_operand(operand, ctx, span)?)
                        }
                        Rvalue::BinaryOp(bin_op, operands)
                        | Rvalue::CheckedBinaryOp(bin_op, operands) => {
                            let lhs =
                                self.visit_operand(&operands.0, ctx, span)?.item_id();
                            let rhs =
                                self.visit_operand(&operands.1, ctx, span)?.item_id();

                            let ty = ctx.instantiate(
                                self.tcx,
                                bin_op.ty(
                                    self.tcx,
                                    operands.0.ty(&mir.local_decls, self.tcx),
                                    operands.1.ty(&mir.local_decls, self.tcx),
                                ),
                            );
                            let output_ty = self.find_sig_ty(ty, ctx, span)?;

                            let bin_op = BinOp::try_from_op(*bin_op, span)?;

                            if let Rvalue::CheckedBinaryOp(_, _) = rvalue {
                                ctx.add_checked(assign.0.local);
                            }
                            Some(
                                bin_op
                                    .bin_op(self, lhs, rhs, output_ty, ctx, span)?
                                    .into(),
                            )
                        }
                        Rvalue::Aggregate(aggregate_kind, fields) => match aggregate_kind
                            .deref()
                        {
                            AggregateKind::Tuple => {
                                let ty = ctx.instantiate(
                                    self.tcx,
                                    rvalue.ty(&mir.local_decls, self.tcx),
                                );
                                let sig_ty = self.find_sig_ty(ty, ctx, span)?;
                                let struct_ty =
                                    sig_ty.opt_struct_ty().ok_or_else(|| {
                                        SpanError::new(
                                            SpanErrorKind::ExpectedStructType,
                                            span,
                                        )
                                    })?;

                                Some(
                                    self.make_struct_group(
                                        struct_ty,
                                        fields,
                                        |generator, field| {
                                            Ok(generator
                                                .visit_operand(field, ctx, span)?
                                                .item_id())
                                        },
                                    )?
                                    .into(),
                                )
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
                                let ty = self
                                    .tcx
                                    .type_of(variant_did)
                                    .instantiate(self.tcx, generic_args);

                                let sig_ty = self.find_sig_ty(ty, ctx, span)?;
                                match sig_ty.kind {
                                    SignalTyKind::Struct(struct_ty) => Some(
                                        self.make_struct_group(
                                            struct_ty,
                                            fields,
                                            |generator, field| {
                                                Ok(generator
                                                    .visit_operand(field, ctx, span)?
                                                    .item_id())
                                            },
                                        )?
                                        .into(),
                                    ),
                                    SignalTyKind::Enum(enum_ty) => {
                                        let data_part = if fields.is_empty() {
                                            None
                                        } else {
                                            let sig_ty =
                                                enum_ty.variant(variant_idx.as_usize());
                                            let struct_ty = sig_ty
                                                .opt_struct_ty()
                                                .ok_or_else(|| {
                                                    SpanError::new(
                                                        SpanErrorKind::ExpectedStructType,
                                                        span,
                                                    )
                                                })?;

                                            Some(self.make_struct_group(
                                                struct_ty,
                                                fields,
                                                |generator, field| {
                                                    Ok(generator
                                                        .visit_operand(field, ctx, span)?
                                                        .item_id())
                                                },
                                            )?)
                                        };

                                        Some(
                                            self.enum_variant_to_bitvec(
                                                ctx.module_id,
                                                enum_ty,
                                                variant_idx.as_usize(),
                                                data_part,
                                            )
                                            .into(),
                                        )
                                    }
                                    _ => None,
                                }
                            }
                            AggregateKind::Closure(closure_did, closure_generics) => {
                                Some(
                                    self.visit_closure(
                                        *closure_did,
                                        closure_generics,
                                        fields,
                                        ctx,
                                        span,
                                    )?
                                    .into(),
                                )
                            }
                            _ => None,
                        },
                        _ => None,
                    };

                    let mod_or_item = mod_or_item.ok_or_else(|| {
                        println!("assign ({}): {rvalue:#?}", dump_rvalue_kind(rvalue));
                        SpanError::new(SpanErrorKind::NotSynthExpr, span)
                    })?;

                    self.assign(assign.0.local, mod_or_item, ctx)?;
                }
                _ => {
                    println!("statement: {statement:#?}");
                    return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
                }
            }
        }

        let terminator = block_data.terminator();
        let span = terminator.source_info.span;

        let next_block = match &terminator.kind {
            TerminatorKind::Call {
                func: Operand::Constant(const_),
                args,
                destination,
                target,
                fn_span,
                ..
            } => {
                let ty = ctx.instantiate(self.tcx, const_.ty());

                if let TyKind::FnDef(fn_did, fn_generics) = ty.kind() {
                    let item_id =
                        self.resolve_fn_call(*fn_did, fn_generics, args, ctx, *fn_span)?;

                    self.assign(destination.local, item_id.into(), ctx)?;
                } else {
                    println!("terminator: {terminator:#?}");
                    return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
                }

                *target
            }
            TerminatorKind::Return | TerminatorKind::Unreachable => None,
            TerminatorKind::Drop { target, .. }
            | TerminatorKind::Goto { target }
            | TerminatorKind::Assert { target, .. } => Some(*target),
            TerminatorKind::SwitchInt { discr, targets } => {
                let discr = self.visit_operand(discr, ctx, span)?.item_id();
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

                    let width = switch.width;

                    let discr = switch.discr;
                    let discr = match self.item_ty(switch.discr).kind {
                        SignalTyKind::Enum(enum_ty) => {
                            let discr_width = enum_ty.discr_width();
                            let ty = NodeTy::BitVec(discr_width.into());
                            let discr = self.to_bitvec(mod_id, discr);

                            self.netlist.add_and_get_out(
                                mod_id,
                                Splitter::new(discr, [(ty, None)], None, true),
                            )
                        }
                        _ => self.to_bitvec(mod_id, discr),
                    };

                    let output_ty =
                        self.make_tuple_ty(&switch.locals, |generator, local| {
                            let ty = mir.local_decls[*local].ty;
                            let span = mir.local_decls[*local].source_info.span;
                            let sig_ty = generator.find_sig_ty(ty, ctx, span)?;

                            Ok(sig_ty)
                        })?;

                    let default = switch.otherwise().map(|otherwise| {
                        let otherwise = otherwise.values();
                        let item_id = self
                            .make_struct_group(output_ty, otherwise, |_, item_id| {
                                Ok(*item_id)
                            })
                            .unwrap();

                        self.to_bitvec(mod_id, item_id)
                    });

                    let inputs = switch.variants().map(|variant| {
                        let mut mask = BitVecMask::default();
                        mask.set_val(variant.value, width);

                        let item_id = self
                            .make_struct_group(
                                output_ty,
                                variant.locals.values(),
                                |_, item_id| Ok(*item_id),
                            )
                            .unwrap();
                        let node_out_id = self.to_bitvec(mod_id, item_id);

                        (mask, node_out_id)
                    });

                    let output_ty = SignalTy::new(output_ty.into());
                    let case = Case::new(
                        output_ty.to_bitvec(),
                        discr,
                        inputs,
                        default,
                        SymIdent::Mux,
                    );
                    let case = self.netlist.add_and_get_out(mod_id, case);
                    let case = self.from_bitvec(mod_id, case, output_ty);

                    for (local, item_id) in switch.locals.iter().zip(case.item_ids()) {
                        self.assign(*local, item_id.into(), ctx)?;
                    }
                }

                Some(convergent_block)
            }
            _ => {
                println!("terminator: {terminator:#?}");
                return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
            }
        };

        Ok(next_block)
    }

    fn assign(
        &mut self,
        local: Local,
        mod_or_item: ModuleOrItem,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<(), Error> {
        match ctx.last_switch() {
            Some(switch) if switch.contains(local) => {
                switch.add_variant_item_id(local, mod_or_item.item_id());
            }
            _ => {
                ctx.add_local(local, mod_or_item);
            }
        }
        Ok(())
    }

    fn visit_operand(
        &mut self,
        operand: &Operand<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ModuleOrItem, Error> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let local = ctx.find_local(place.local, span)?;
                match local {
                    ModuleOrItem::Module(mod_id) => Ok(mod_id.into()),
                    ModuleOrItem::Item(item_id) => {
                        self.visit_place(place, item_id, ctx, span).map(Into::into)
                    }
                }
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
                            let sig_ty =
                                self.find_sig_ty(const_.ty(), ctx, span)?.node_ty();

                            return Ok(ItemId::from(self.netlist.const_val(
                                ctx.module_id,
                                sig_ty,
                                value,
                            ))
                            .into());
                        }
                    }
                    Const::Val(const_value, ty) => match const_value {
                        ConstValue::Scalar(scalar) => {
                            if let Some(value) = utils::eval_scalar(scalar) {
                                let ty = ctx.instantiate(self.tcx, ty);
                                let sig_ty = self.find_sig_ty(ty, ctx, span)?.node_ty();

                                return Ok(ItemId::from(self.netlist.const_val(
                                    ctx.module_id,
                                    sig_ty,
                                    value,
                                ))
                                .into());
                            }
                        }
                        ConstValue::ZeroSized => {
                            if let Some(item_id) = ctx.find_const(&value.const_) {
                                return Ok(item_id.into());
                            }

                            if let TyKind::Closure(closure_did, closure_generics) =
                                ty.kind()
                            {
                                let closure_args = ClosureArgs {
                                    args: closure_generics,
                                };

                                if closure_args.upvar_tys().is_empty() {
                                    return Ok(self
                                        .visit_closure(
                                            *closure_did,
                                            closure_generics,
                                            &IndexVec::new(),
                                            ctx,
                                            span,
                                        )?
                                        .into());
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
                            let ty = ctx.instantiate(self.tcx, ty);
                            let sig_ty = self.find_sig_ty(ty, ctx, span)?.to_bitvec();

                            return Ok(ItemId::from(self.netlist.const_val(
                                ctx.module_id,
                                sig_ty,
                                value,
                            ))
                            .into());
                        }

                        if let Some(promoted) = unevaluated.promoted {
                            let output_ty = self.find_sig_ty(ty, ctx, span)?;
                            let fn_args = ctx.instantiate(self.tcx, unevaluated.args);

                            let module_id = self.visit_fn_mir(
                                (ctx.fn_did, promoted),
                                fn_args,
                                false,
                            )?;

                            let module =
                                self.instantiate_module(module_id, iter::empty(), ctx);

                            return Ok(self.combine_outputs(module, output_ty).into());
                        }
                    }
                }

                println!("operand value: {:#?}", value.const_);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    pub fn visit_place(
        &mut self,
        place: &Place,
        mut item_id: ItemId,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        for place_elem in place.projection {
            item_id = match place_elem {
                PlaceElem::ConstantIndex {
                    offset, from_end, ..
                } => {
                    let array_ty = self.item_ty(item_id).array_ty();
                    let count = array_ty.count() as u64;
                    let offset = if from_end { count - offset } else { offset };

                    item_id.group().by_idx(offset as usize)
                }
                PlaceElem::Deref | PlaceElem::Subtype(_) => item_id,
                PlaceElem::Field(idx, _) => {
                    if ctx.is_checked(place.local) {
                        item_id
                    } else {
                        item_id.group().by_idx(idx.as_usize())
                    }
                }
                PlaceElem::Downcast(_, variant_idx) => {
                    let mod_id = ctx.module_id;

                    let enum_ty = self.item_ty(item_id).enum_ty();
                    let variant = self.to_bitvec(mod_id, item_id);

                    self.enum_variant_from_bitvec(
                        mod_id,
                        variant,
                        enum_ty,
                        variant_idx.as_usize(),
                    )
                }
                _ => {
                    println!("place elem: {place_elem:?} (local: {:?})", place.local);
                    return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
                }
            }
        }

        Ok(item_id)
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

    pub fn resolve_fn_call(
        &mut self,
        fn_did: impl Into<DefId>,
        fn_generics: GenericArgsRef<'tcx>,
        args: &[Operand<'tcx>],
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let fn_did = fn_did.into();
        let fn_generics = ctx.instantiate(self.tcx, fn_generics);

        let (instance_did, instance_generics) = self
            .tcx
            .resolve_instance(ParamEnvAnd {
                param_env: ParamEnv::reveal_all(),
                value: (fn_did, fn_generics),
            })
            .ok()
            .and_then(identity)
            .and_then(|instance| {
                if let InstanceDef::Item(fn_did) = instance.def {
                    Some((fn_did, instance.args))
                } else {
                    None
                }
            })
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthCall, span))?;

        if !self.tcx.is_mir_available(instance_did) {
            return Err(SpanError::new(SpanErrorKind::NotSynthCall, span).into());
        }

        let args = args
            .iter()
            .map(|arg| self.visit_operand(arg, ctx, span))
            .collect::<Result<Vec<_>, _>>()?;

        let output_ty =
            self.fn_output(fn_did, &ctx.with_generic_args(fn_generics), span)?;

        if self.is_local_def_id(instance_did)
            || self.is_synth(instance_did)
            || self.is_synth(fn_did)
        {
            let mono_item = MonoItem::new(fn_did, fn_generics);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                let module_id =
                    self.visit_fn_mir(instance_did, instance_generics, false)?;

                self.evaluated_modules.insert(mono_item, module_id);
            }
            let module_id = *self.evaluated_modules.get(&mono_item).unwrap();

            let module = self.instantiate_module(
                module_id,
                args.into_iter().map(ModuleOrItem::item_id),
                ctx,
            );

            Ok(self.combine_outputs(module, output_ty))
        } else {
            let blackbox = self
                .find_blackbox(instance_did, span)
                .or_else(|e| self.find_blackbox(fn_did, span).map_err(|_| e))?;
            let old_fn_did = ctx.fn_did;
            let old_generics = ctx.generic_args;

            ctx.fn_did = instance_did;
            ctx.generic_args = instance_generics;

            let blackbox = blackbox.eval(self, &args, output_ty, ctx, span)?;

            ctx.fn_did = old_fn_did;
            ctx.generic_args = old_generics;

            Ok(blackbox)
        }
    }

    pub fn visit_closure(
        &mut self,
        closure_did: DefId,
        closure_generics: GenericArgsRef<'tcx>,
        captures: &IndexVec<FieldIdx, Operand<'tcx>>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ModuleId, Error> {
        let closure_generics = ctx.instantiate(self.tcx, closure_generics);
        let closure_args = ClosureArgs {
            args: closure_generics,
        };
        let output_ty =
            self.find_sig_ty(closure_args.sig().skip_binder().output(), ctx, span)?;
        let closure_id = self.visit_fn_mir(closure_did, closure_generics, false)?;
        self.netlist[closure_id].is_inlined = true;
        let closure = Closure {
            upvars: captures.clone(),
            output_ty,
        };

        ctx.add_closure(closure_id, closure);

        Ok(closure_id)
    }

    pub fn instantiate_closure_(
        &mut self,
        closure_id: ModuleId,
        inputs: &[ItemId],
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let closure = ctx.find_closure(closure_id, span)?;
        let upvars = closure
            .upvars
            .iter()
            .map(|upvar| {
                self.visit_operand(upvar, ctx, span)
                    .map(ModuleOrItem::item_id)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let upvars_ty = self
            .make_tuple_ty(upvars.iter(), |generator, upvar| {
                Ok(generator.item_ty(*upvar))
            })
            .unwrap();

        let upvars = self
            .make_struct_group(upvars_ty, upvars, |_, upvar| Ok(upvar))
            .unwrap();

        let module = self.instantiate_module(
            closure_id,
            iter::once(upvars).chain(inputs.iter().copied()),
            ctx,
        );
        let mut outputs = CombineOutputs::new(self, module);
        Ok(outputs.next_output(self, closure.output_ty))
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
