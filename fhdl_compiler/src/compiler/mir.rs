use std::{convert::identity, fmt::Debug, iter, ops::Deref};

use fhdl_netlist::{
    netlist::{Module, ModuleId},
    node::{Pass, PassArgs},
    symbol::Symbol,
};
use rustc_hir::{def::DefKind, def_id::DefId, definitions::DefPathData};
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        AggregateKind, BasicBlock, BorrowKind, Const, ConstOperand, ConstValue, Local,
        LocalDecl, Operand, Place, PlaceElem, Promoted, Rvalue, StatementKind,
        TerminatorKind, UnOp, VarDebugInfoContents, RETURN_PLACE, START_BLOCK,
    },
    ty::{GenericArgsRef, Instance, InstanceDef, List, ParamEnv, ParamEnvAnd, TyKind},
};
use rustc_span::{def_id::LOCAL_CRATE, Span};
use rustc_target::abi::FieldIdx;
use smallvec::SmallVec;
use tracing::{debug, error, instrument};

use super::{
    item::{CombineOutputs, Group, Item},
    item_ty::{ItemTy, ItemTyKind},
    Compiler, Context, MonoItem,
};
use crate::{
    blackbox::{bin_op::BinOp, un_op::BitNot},
    compiler::{cons_::scalar_to_u128, item::ModuleExt, pins::Idents},
    error::{Error, SpanError, SpanErrorKind},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefIdOrPromoted<'tcx> {
    DefId(DefId, InstanceDef<'tcx>),
    Promoted(DefId, Promoted),
}

impl<'tcx> Debug for DefIdOrPromoted<'tcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.did().fmt(f)
    }
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
    #[instrument(parent = None, level = "debug", skip(self, def_id_or_promoted, fn_generics, top_module), fields(def_id = self.fn_name(def_id_or_promoted.did())))]
    pub fn visit_fn(
        &mut self,
        def_id_or_promoted: DefIdOrPromoted<'tcx>,
        fn_generics: GenericArgsRef<'tcx>,
        top_module: bool,
    ) -> Result<ModuleId, Error> {
        let mono_item = MonoItem::new(def_id_or_promoted, fn_generics);

        #[allow(clippy::map_entry)]
        if !self.evaluated_modules.contains_key(&mono_item) {
            debug!("start");
            let fn_did = def_id_or_promoted.did();
            let span = self
                .tcx
                .def_ident_span(fn_did)
                .unwrap_or_else(|| self.tcx.def_span(fn_did));

            let mut module_sym = self.module_name(fn_did);

            let mut synth_attrs = None;
            let (mir, inline) = match def_id_or_promoted {
                DefIdOrPromoted::DefId(fn_did, instance_def) => {
                    let mir = self.tcx.instance_mir(instance_def);
                    synth_attrs = self.find_synth(fn_did);
                    let inline = synth_attrs
                        .as_ref()
                        .map(|synth_attrs| synth_attrs.inline)
                        .unwrap_or_default();

                    (mir, inline)
                }
                DefIdOrPromoted::Promoted(fn_did, promoted) => {
                    let promoted_mir = self.tcx.promoted_mir(fn_did);
                    let mir = &promoted_mir[promoted];
                    module_sym =
                        Symbol::intern_args(format_args!("{}_promoted", module_sym));
                    (mir, true)
                }
            };

            if mir.basic_blocks.is_cfg_cyclic() {
                return Err(SpanError::new(SpanErrorKind::UnsupportedLoops, span).into());
            }

            if self.args.dump_mir {
                debug!("mir: {mir:#?}");
            }

            let mut module = Module::new(module_sym, top_module);
            let mod_span = self.span_to_string(span, fn_did);
            module.set_span(mod_span);

            if !top_module && inline {
                module.inline = true;
            }

            let mut ctx = Context::new(fn_did, module, fn_generics, mir);

            let inputs = mir
                .local_decls
                .iter_enumerated()
                .skip(1)
                .take(mir.arg_count);
            let inputs = self.visit_fn_inputs(inputs, &mut ctx)?;
            for var_debug_info in &mir.var_debug_info {
                if let Some(arg_idx) = var_debug_info.argument_index {
                    let input = &inputs[(arg_idx - 1) as usize];

                    match var_debug_info.value {
                        VarDebugInfoContents::Place(place)
                            if place.projection.is_empty() =>
                        {
                            let name = var_debug_info.name.as_str();
                            ctx.module.assign_names_to_item(name, input, true);
                        }
                        VarDebugInfoContents::Const(ConstOperand { const_, .. }) => {
                            ctx.add_const(const_, input.clone());
                        }
                        _ => {}
                    }
                }
            }

            self.visit_blocks(None, None, &mut ctx)?;

            for var_debug_info in &mir.var_debug_info {
                let name = var_debug_info.name.as_str();
                let span = var_debug_info.source_info.span;
                match var_debug_info.value {
                    VarDebugInfoContents::Place(place) => {
                        let item = self.visit_rhs_place(&place, &mut ctx, span)?;
                        ctx.module.assign_names_to_item(name, &item, true);
                    }
                    VarDebugInfoContents::Const(ConstOperand { const_, .. }) => {
                        if let Some(item) = ctx.find_const(&const_) {
                            ctx.module.assign_names_to_item(name, &item, true);
                        }
                    }
                }
            }

            self.visit_fn_output(&mut ctx);

            if top_module {
                if let Some(synth_attrs) = &synth_attrs {
                    let output = ctx.locals.get(RETURN_PLACE);

                    if !synth_attrs.constr.is_empty() {
                        let idents = Idents::from_debug_info(mir);
                        let inputs_and_outputs =
                            inputs.iter().map(|input| input.nodes()).sum::<usize>()
                                + output.nodes();

                        self.add_pin_constraints(
                            &synth_attrs.constr,
                            &idents,
                            inputs_and_outputs,
                            &ctx,
                        )?;
                    }
                }
            }

            let module_id = self.netlist.add_module(ctx.module);

            self.evaluated_modules.insert(mono_item, module_id);

            debug!("end");
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
            let s = match &data_path.data {
                DefPathData::TypeNs(s) | DefPathData::ValueNs(s) => Some(s.as_str()),
                DefPathData::Closure => Some("closure"),
                _ => None,
            };

            if let Some(s) = s {
                if has_sep {
                    name.push('_');
                } else {
                    has_sep = true;
                }
                name.push_str(s);
            }
        }

        Symbol::intern(&name)
    }

    pub fn visit_fn_inputs<'a>(
        &mut self,
        inputs: impl IntoIterator<Item = (Local, &'a LocalDecl<'tcx>)>,
        ctx: &mut Context<'tcx>,
    ) -> Result<SmallVec<[Item<'tcx>; 1]>, Error>
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

    pub fn visit_fn_output(&self, ctx: &mut Context<'tcx>) {
        let module = &mut ctx.module;
        let output = ctx.locals.get(RETURN_PLACE);

        // TODO: optimize
        let outputs = output
            .iter()
            .map(|port| {
                let node = module.node(port.node);
                let port = if node.is_input() || module.is_mod_output(port) {
                    let sym = module[port].sym;
                    module.add_and_get_port::<_, Pass>(PassArgs {
                        input: port,
                        sym,
                        ty: None,
                    })
                } else {
                    port
                };
                module.add_mod_output(port);

                port
            })
            .collect::<SmallVec<[_; 1]>>();

        let output = module.combine(outputs.into_iter(), output.ty);
        ctx.locals.place(RETURN_PLACE, output.clone());

        ctx.module.assign_names_to_item("out", &output, false);
    }

    pub fn visit_blocks(
        &mut self,
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

            next = self.visit_block(block, ctx)?;
        }

        Ok(())
    }

    fn visit_block(
        &mut self,
        block: BasicBlock,
        ctx: &mut Context<'tcx>,
    ) -> Result<Option<BasicBlock>, Error> {
        let mir = ctx.mir;
        let basic_blocks = &mir.basic_blocks;
        let block_data = &basic_blocks[block];

        for statement in &block_data.statements {
            let span = statement.source_info.span;

            match &statement.kind {
                StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => {}
                StatementKind::Assign(assign) => {
                    let rvalue = &assign.1;
                    let rvalue_ty = rvalue.ty(&mir.local_decls, self.tcx);

                    let item: Option<Item> = match rvalue {
                        Rvalue::Ref(_, BorrowKind::Shared, place)
                        | Rvalue::Discriminant(place)
                        | Rvalue::CopyForDeref(place) => {
                            Some(self.visit_rhs_place(place, ctx, span)?)
                        }
                        Rvalue::Use(operand) => {
                            Some(self.visit_operand(operand, ctx, span)?)
                        }
                        Rvalue::BinaryOp(bin_op, operands) => {
                            let lhs = self.visit_operand(&operands.0, ctx, span)?;
                            let rhs = self.visit_operand(&operands.1, ctx, span)?;

                            let lhs_ty = operands.0.ty(&mir.local_decls, self.tcx);
                            let ty = bin_op.ty(
                                self.tcx,
                                lhs_ty,
                                operands.1.ty(&mir.local_decls, self.tcx),
                            );
                            let output_ty =
                                self.resolve_ty(ty, ctx.generic_args, span)?;

                            let lhs_ty =
                                self.resolve_ty(lhs_ty, ctx.generic_args, span)?;
                            let bin_op = BinOp::try_from_op(lhs_ty, *bin_op, span)?;

                            Some(bin_op.bin_op(&lhs, &rhs, output_ty, ctx, span)?)
                        }
                        Rvalue::UnaryOp(UnOp::Not, operand) => {
                            let expr = self.visit_operand(operand, ctx, span)?;

                            Some(BitNot::not(self, &expr, ctx)?)
                        }
                        Rvalue::Repeat(op, const_) => {
                            let rvalue_ty =
                                self.resolve_ty(rvalue_ty, ctx.generic_args, span)?;
                            let count = self.eval_const(*const_, span)? as usize;
                            let op = self.visit_operand(op, ctx, span)?;

                            Some(Item::new(
                                rvalue_ty,
                                Group::new(
                                    iter::repeat(op)
                                        .take(count)
                                        .map(|item| item.deep_clone()),
                                ),
                            ))
                        }
                        Rvalue::Aggregate(aggregate_kind, fields) => match aggregate_kind
                            .deref()
                        {
                            AggregateKind::Array(_) => {
                                let rvalue_ty =
                                    self.resolve_ty(rvalue_ty, ctx.generic_args, span)?;

                                Some(self.mk_item_group(rvalue_ty, fields, ctx, span)?)
                            }
                            AggregateKind::Tuple => {
                                let ty = rvalue.ty(&mir.local_decls, self.tcx);
                                let ty = self.resolve_ty(ty, ctx.generic_args, span)?;

                                Some(self.mk_item_group(ty, fields, ctx, span)?)
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
                                        Some(self.mk_item_group(ty, fields, ctx, span)?)
                                    }
                                    ItemTyKind::Enum(enum_ty) => {
                                        let data_part = if fields.is_empty() {
                                            None
                                        } else {
                                            let ty =
                                                *enum_ty.by_variant_idx(variant_idx).ty;

                                            Some(
                                                self.mk_item_group(
                                                    ty, fields, ctx, span,
                                                )?,
                                            )
                                        };

                                        Some(ctx.module.enum_variant_to_bitvec(
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
                                    *closure_did,
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

                    self.assign(assign.0, item, ctx)?;
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
                        let fn_item = self.visit_rhs_place(place, ctx, span)?;

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
                            let item = self.visit_fn_call(
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
                        self.assign(*destination, item, ctx)?;
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
                self.visit_switch(block, discr, targets, ctx, span)?
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

    pub fn assign(
        &mut self,
        place: Place<'tcx>,
        rhs: Item<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Result<(), Error> {
        let local = place.local;
        let span = ctx.mir.local_decls[local].source_info.span;
        let lhs = ctx.locals.get_opt(local);

        match ctx.last_switch() {
            Some(switch) if switch.contains(local) => {
                if place.projection.is_empty() {
                    switch.add_branch_local(local, rhs.clone());
                } else {
                    // Write rhs into a nested part of lhs
                    let mut lhs = match switch.branch_local(local) {
                        Some(lhs) => lhs.clone(),
                        None => lhs.expect("cannot find local").deep_clone(),
                    };

                    self.visit_lhs_place(&place, &mut lhs, rhs, span)?;

                    switch.add_branch_local(local, lhs);
                }
            }
            _ => {
                if place.projection.is_empty() {
                    ctx.locals.place(local, rhs.clone());
                } else {
                    let mut lhs = lhs.expect("cannot find local");
                    // Write rhs into a nested part of lhs
                    self.visit_lhs_place(&place, &mut lhs, rhs, span)?;

                    ctx.locals.place(local, lhs);
                }
            }
        }
        Ok(())
    }

    pub fn assign_local(
        &mut self,
        local: Local,
        item: &Item<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Result<(), Error> {
        match ctx.last_switch() {
            Some(switch) if switch.contains(local) => {
                switch.add_branch_local(local, item.clone());
            }
            _ => {
                ctx.locals.place(local, item.clone());
            }
        }
        Ok(())
    }

    fn mk_item_group(
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

    fn visit_operands<'a>(
        &mut self,
        operands: impl IntoIterator<Item = &'a Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<SmallVec<[Item<'tcx>; 1]>, Error>
    where
        'tcx: 'a,
    {
        operands
            .into_iter()
            .map(|operand| self.visit_operand(operand, ctx, span))
            .collect()
    }

    pub fn visit_operand(
        &mut self,
        operand: &Operand<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                self.visit_rhs_place(place, ctx, span)
            }
            Operand::Constant(value) => {
                let span = value.span;

                match value.const_ {
                    Const::Ty(const_) => {
                        if let Ok(value) =
                            self.eval_const(ctx.instantiate(self.tcx, const_), span)
                        {
                            return self.mk_const(const_.ty(), value, ctx, span);
                        }
                    }
                    Const::Val(const_value, ty) => match const_value {
                        ConstValue::Scalar(scalar) => {
                            if let Some(value) = scalar_to_u128(scalar) {
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
                                    *closure_did,
                                    closure_generics,
                                    &IndexVec::new(),
                                    ctx,
                                    span,
                                );
                            }

                            if let TyKind::FnDef(fn_did, fn_generics) = ty.kind() {
                                let fn_generics = ctx.instantiate(self.tcx, *fn_generics);

                                let (instance_did, instance) =
                                    self.resolve_instance(*fn_did, fn_generics, span)?;

                                return self.visit_closure(
                                    instance_did,
                                    instance.args,
                                    &IndexVec::new(),
                                    ctx,
                                    span,
                                );
                            }

                            if let Ok(ty) = self.resolve_ty(ty, ctx.generic_args, span) {
                                if let Some(item) = ctx.module.mk_zero_sized_val(ty) {
                                    return Ok(item);
                                }
                            }
                        }
                        _ => {}
                    },
                    Const::Unevaluated(unevaluated, ty) => {
                        let ty = ctx.instantiate(self.tcx, ty);

                        if let Some(promoted) = unevaluated.promoted {
                            let output_ty =
                                self.resolve_ty(ty, ctx.generic_args, span)?;
                            let fn_args = ctx.instantiate(self.tcx, unevaluated.args);

                            let module_id = self.visit_fn(
                                (ctx.fn_did, promoted).into(),
                                fn_args,
                                false,
                            )?;

                            let mod_inst_id = self.instantiate_module(
                                &mut ctx.module,
                                module_id,
                                iter::empty(),
                            );

                            return Ok(ctx
                                .module
                                .combine_from_node(mod_inst_id, output_ty));
                        }

                        if let Some(item) =
                            self.resolve_unevaluated(unevaluated, ty, ctx, span)
                        {
                            return Ok(item);
                        }
                    }
                }

                error!("operand value: {:#?}", value.const_);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    fn visit_lhs_place(
        &self,
        place: &Place<'tcx>,
        mut lhs: &mut Item<'tcx>,
        rhs: Item<'tcx>,
        span: Span,
    ) -> Result<(), Error> {
        for place_elem in place.projection {
            lhs = match place_elem {
                PlaceElem::Field(idx, _) => lhs.by_field_mut(idx),
                _ => {
                    return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
                }
            }
        }

        *lhs = rhs;

        Ok(())
    }

    pub fn visit_rhs_place(
        &mut self,
        place: &Place<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let mut item = match ctx.last_switch() {
            Some(switch) if switch.has_branch_local(place.local) => {
                switch.branch_local(place.local).unwrap().clone()
            }
            _ => ctx.locals.get(place.local).clone(),
        };

        for place_elem in place.projection {
            if item.is_unsigned() {
                return Ok(item);
            }
            item = match place_elem {
                PlaceElem::ConstantIndex {
                    offset, from_end, ..
                } => {
                    let array_ty = item.ty.array_ty();
                    let count = array_ty.count() as u64;
                    let offset = if from_end { count - offset } else { offset };

                    item.by_idx(offset as usize)
                }
                PlaceElem::Deref => item,
                PlaceElem::Subtype(_) => item,
                PlaceElem::Field(idx, _) => item.by_field(idx),
                PlaceElem::Downcast(_, variant_idx) => {
                    let enum_ty = item.ty.enum_ty();
                    let variant = ctx.module.to_bitvec(&item);

                    ctx.module.enum_variant_from_bitvec(
                        variant.port(),
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

    pub fn visit_fn_call<'a>(
        &mut self,
        fn_did: DefId,
        fn_generics: GenericArgsRef<'tcx>,
        inputs: impl IntoIterator<Item = &'a Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error>
    where
        'tcx: 'a,
    {
        let fn_generics = ctx.instantiate(self.tcx, fn_generics);

        let (instance_did, instance) =
            self.resolve_instance(fn_did, fn_generics, span)?;

        let is_std_call = self.is_std_call(fn_did);

        if ((instance_did.is_local() || is_std_call)
            && !self.has_blackbox(fn_did)
            && !self.has_blackbox(instance_did))
            || self.is_synth(instance_did)
            || self.is_synth(fn_did)
        {
            let inputs = self.visit_operands(inputs, ctx, span)?;
            let output_ty = self.fn_output(instance_did, instance.args);
            let output_ty = self.resolve_ty(output_ty, List::empty(), span)?;

            let module_id =
                self.visit_fn((instance_did, instance.def).into(), instance.args, false)?;
            if is_std_call {
                self.netlist[module_id].borrow_mut().inline = true;
            }
            let mod_inst_id =
                self.instantiate_module(&mut ctx.module, module_id, inputs.iter());
            let span = self.span_to_string(span, ctx.fn_did);
            ctx.module.add_span(mod_inst_id, span);

            Ok(ctx.module.combine_from_node(mod_inst_id, output_ty))
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
            let output_ty = self.fn_output(instance_did, instance.args);
            let output_ty = self.resolve_ty(output_ty, List::empty(), span)?;

            ctx.fn_did = instance_did;
            ctx.generic_args = instance.args;

            let blackbox = blackbox.eval(self, &inputs, output_ty, ctx, span)?;

            ctx.fn_did = old_fn_did;
            ctx.generic_args = old_generics;

            Ok(blackbox)
        }
    }

    pub fn resolve_instance(
        &self,
        fn_did: DefId,
        fn_generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<(DefId, Instance<'tcx>), Error> {
        self.tcx
            .resolve_instance(ParamEnvAnd {
                param_env: ParamEnv::reveal_all(),
                value: (fn_did, fn_generics),
            })
            .ok()
            .and_then(identity)
            .and_then(|instance| match instance.def {
                InstanceDef::Item(fn_did) => Some((fn_did, instance)),
                InstanceDef::FnPtrShim(fn_did, _) => Some((fn_did, instance)),
                _ => None,
            })
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthCall, span).into())
    }

    pub fn visit_closure(
        &mut self,
        closure_did: DefId,
        closure_generics: GenericArgsRef<'tcx>,
        captures: &IndexVec<FieldIdx, Operand<'tcx>>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let closure_generics = ctx.instantiate(self.tcx, closure_generics);
        let closure_ty = self.type_of(closure_did, closure_generics);
        let closure_ty = self.resolve_ty(closure_ty, List::empty(), span)?;

        self.mk_item_group(closure_ty, captures, ctx, span)
    }

    pub fn instantiate_closure(
        &mut self,
        closure: &Item<'tcx>,
        inputs: &[Item<'tcx>],
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let closure_ty = closure.ty.closure_ty();
        let fn_did = closure_ty.fn_did;
        let fn_generics = closure_ty.fn_generics;

        let module_id = self.visit_fn(fn_did.into(), fn_generics, false)?;

        let mod_inst_id = if let DefKind::Closure = self.tcx.def_kind(fn_did) {
            self.netlist[module_id].borrow_mut().inline = true;
            self.instantiate_module(
                &mut ctx.module,
                module_id,
                iter::once(closure).chain(inputs.iter()),
            )
        } else {
            self.instantiate_module(&mut ctx.module, module_id, inputs)
        };

        let node_span = self.span_to_string(span, ctx.fn_did);
        ctx.module.add_span(mod_inst_id, node_span);

        let output_ty = self.fn_output(fn_did, fn_generics);
        let output_ty = self.resolve_ty(output_ty, List::empty(), span)?;

        let mut outputs = CombineOutputs::from_node(&mut ctx.module, mod_inst_id);

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
