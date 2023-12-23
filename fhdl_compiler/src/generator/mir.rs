use std::{convert::identity, fmt::Write, iter, ops::Deref};

use fhdl_netlist::{group::ItemId, net_list::ModuleId, symbol::Symbol};
use rustc_hir::def_id::DefId;
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        traversal, AggregateKind, Body, BorrowKind, Const, ConstOperand, ConstValue,
        Local, LocalDecl, Operand, PlaceElem, Rvalue, StatementKind, TerminatorKind,
        VarDebugInfoContents, RETURN_PLACE,
    },
    ty::{ClosureArgs, GenericArgsRef, InstanceDef, List, ParamEnv, ParamEnvAnd, TyKind},
};
use rustc_span::Span;
use rustc_target::abi::FieldIdx;

use super::Generator;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::{Closure, EvalContext, ModuleOrItem},
    generator::bitvec::CombineOutputs,
    utils,
};

impl<'tcx> Generator<'tcx> {
    pub fn eval_fn_mir(
        &mut self,
        fn_did: impl Into<DefId>,
        fn_generics: GenericArgsRef<'tcx>,
        top_module: bool,
    ) -> Result<ModuleId, Error> {
        let fn_did = fn_did.into();

        let mir = self.tcx.instance_mir(InstanceDef::Item(fn_did));
        mir.basic_blocks.predecessors();
        mir.basic_blocks.switch_sources();
        mir.basic_blocks.dominators();
        mir.basic_blocks.reverse_postorder();
        println!("{mir:#?}");

        let module_sym = if !self.tcx.is_closure(fn_did) {
            self.module_name(fn_did)
        } else {
            Symbol::new("closure")
        };
        let module_id = self.netlist.add_module(module_sym, top_module);
        Ok(module_id)
        // if self.is_inlined(fn_did) {
        //     self.netlist[module_id].is_inlined = true;
        // }

        // let mut ctx = EvalContext::new(self.is_primary, fn_generics, module_id);

        // let inputs = mir
        //     .local_decls
        //     .iter_enumerated()
        //     .skip(1)
        //     .take(mir.arg_count);
        // let inputs = self.eval_fn_mir_inputs(inputs, &mut ctx)?;
        // for var_debug_info in &mir.var_debug_info {
        //     if let VarDebugInfoContents::Const(ConstOperand { const_, .. }) =
        //         var_debug_info.value
        //     {
        //         if let Some(arg_idx) = var_debug_info.argument_index {
        //             ctx.add_const(const_, inputs[(arg_idx - 1) as usize]);
        //         }
        //     }
        // }

        // self.eval_blocks(mir, &mut ctx)?;

        // for var_debug_info in &mir.var_debug_info {
        //     let name = var_debug_info.name.as_str();
        //     let span = var_debug_info.source_info.span;
        //     match var_debug_info.value {
        //         VarDebugInfoContents::Place(place) => {
        //             if let ModuleOrItem::Item(item_id) =
        //                 ctx.find_local(place.local, span)?
        //             {
        //                 self.assign_names_to_item(name, item_id);
        //             }
        //         }
        //         VarDebugInfoContents::Const(ConstOperand { const_, .. }) => {
        //             if let Some(item_id) = ctx.find_const(&const_) {
        //                 self.assign_names_to_item(name, item_id);
        //             }
        //         }
        //     }
        // }

        // let span = mir.local_decls[RETURN_PLACE].source_info.span;
        // let output_id = ctx.find_local(RETURN_PLACE, span)?.item_id();
        // self.eval_outputs(None, output_id);

        // Ok(module_id)
    }

    fn module_name(&self, def_id: DefId) -> Symbol {
        let def_path = self.tcx.def_path(def_id);

        let mut s = String::with_capacity(def_path.data.len() << 4);
        for component in def_path.data {
            write!(s, "_{component}").unwrap();
        }

        Symbol::new(&s)
    }

    pub fn eval_fn_mir_inputs<'a>(
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

    pub fn eval_blocks(
        &mut self,
        mir: &Body<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<(), Error> {
        let order = traversal::reverse_postorder(mir);
        for (_, block) in order {
            if block.is_cleanup {
                continue;
            }

            for statement in &block.statements {
                let span = statement.source_info.span;

                match &statement.kind {
                    StatementKind::Assign(assign) => {
                        let local = assign.0.local;
                        let rvalue = &assign.1;

                        match rvalue {
                            Rvalue::Ref(_, BorrowKind::Shared, place) => {
                                let mod_or_item = ctx.find_local(place.local, span)?;
                                match mod_or_item {
                                    ModuleOrItem::Module(mod_id) => {
                                        ctx.add_local(local, mod_id)
                                    }
                                    ModuleOrItem::Item(item_id) => ctx.add_local(
                                        local,
                                        self.eval_projection(
                                            item_id,
                                            place.projection,
                                            span,
                                        )?,
                                    ),
                                };
                            }
                            Rvalue::Use(operand) => {
                                let mod_or_item =
                                    self.eval_operand(operand, ctx, span)?;
                                ctx.add_local(local, mod_or_item);
                            }
                            Rvalue::Aggregate(aggregate_kind, fields) => {
                                if let AggregateKind::Closure(
                                    closure_did,
                                    closure_generics,
                                ) = aggregate_kind.deref()
                                {
                                    let mod_id = self.eval_closure_(
                                        *closure_did,
                                        closure_generics,
                                        fields,
                                        ctx,
                                        span,
                                    )?;
                                    ctx.add_local(local, mod_id);
                                }
                            }
                            _ => {
                                match rvalue {
                                    Rvalue::Use(_) => {
                                        println!("use");
                                    }
                                    Rvalue::Repeat(_, _) => {
                                        println!("repeat");
                                    }
                                    Rvalue::Ref(_, _, _) => {
                                        println!("ref");
                                    }
                                    Rvalue::ThreadLocalRef(_) => {
                                        println!("thread local ref");
                                    }
                                    Rvalue::AddressOf(_, _) => {
                                        println!("address_of");
                                    }
                                    Rvalue::Len(_) => {
                                        println!("len");
                                    }
                                    Rvalue::Cast(_, _, _) => {
                                        println!("cast");
                                    }
                                    Rvalue::BinaryOp(_, _) => {
                                        println!("binary_op");
                                    }
                                    Rvalue::CheckedBinaryOp(_, _) => {
                                        println!("checked binary_op");
                                    }

                                    Rvalue::NullaryOp(_, _) => {
                                        println!("nullary_op");
                                    }
                                    Rvalue::UnaryOp(_, _) => {
                                        println!("unary_op");
                                    }
                                    Rvalue::Discriminant(_) => {
                                        println!("discriminant");
                                    }
                                    Rvalue::ShallowInitBox(_, _) => {
                                        println!("shallow init box");
                                    }
                                    Rvalue::CopyForDeref(_) => {
                                        println!("copy_for_deref");
                                    }
                                    Rvalue::Aggregate(_, _) => {
                                        println!("aggregate");
                                    }
                                };
                                println!("assign: {rvalue:#?}");
                                return Err(SpanError::new(
                                    SpanErrorKind::NotSynthExpr,
                                    span,
                                )
                                .into());
                            }
                        }
                    }
                    _ => {
                        println!("statement: {statement:#?}");
                        return Err(
                            SpanError::new(SpanErrorKind::NotSynthExpr, span).into()
                        );
                    }
                }
            }

            if let Some(terminator) = &block.terminator {
                let span = terminator.source_info.span;

                match &terminator.kind {
                    TerminatorKind::Call {
                        func: Operand::Constant(const_),
                        args,
                        destination,
                        fn_span,
                        ..
                    } => {
                        let ty = const_.ty();

                        if let TyKind::FnDef(fn_did, fn_generics) = ty.kind() {
                            let item_id = self.resolve_fn_call(
                                *fn_did,
                                fn_generics,
                                args,
                                ctx,
                                *fn_span,
                            )?;
                            ctx.add_local(destination.local, item_id);
                        } else {
                            println!("terminator: {terminator:#?}");
                            return Err(SpanError::new(
                                SpanErrorKind::NotSynthExpr,
                                span,
                            )
                            .into());
                        }
                    }
                    TerminatorKind::Return | TerminatorKind::Drop { .. } => {}
                    _ => {
                        println!("terminator: {terminator:#?}");
                        return Err(
                            SpanError::new(SpanErrorKind::NotSynthExpr, span).into()
                        );
                    }
                }
            }
        }

        Ok(())
    }

    fn eval_operand(
        &mut self,
        operand: &Operand<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ModuleOrItem, Error> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let local = ctx.find_local(place.local, span)?;
                match local {
                    ModuleOrItem::Module(mod_id) => Ok(mod_id.into()),
                    ModuleOrItem::Item(item_id) => self
                        .eval_projection(item_id, place.projection, span)
                        .map(Into::into),
                }
            }
            Operand::Constant(value) => {
                let span = value.span;

                match value.const_ {
                    Const::Ty(const_) => {
                        println!("Const::Ty({const_:?})");
                    }
                    Const::Unevaluated(value, ty) => {
                        println!("Const::Unevaluated({value:?}, {ty:?})");
                    }
                    Const::Val(const_value, ty) => {
                        println!("Const::Val({const_value:?}, {ty:?})");
                        match const_value {
                            ConstValue::Scalar(scalar) => {
                                if let Some(value) = utils::eval_scalar(scalar) {
                                    let sig_ty =
                                        self.find_sig_ty(ty, ctx, span)?.node_ty();

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

                                if let TyKind::Closure(closure_id, closure_generics) =
                                    ty.kind()
                                {
                                    println!("closure_generics {closure_generics:?}");
                                    println!(
                                        "mir: {:#?}",
                                        self.tcx.optimized_mir(closure_id)
                                    );
                                }
                            }
                            _ => {}
                        }
                    }
                }

                println!("operand value: {value:#?}");
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    pub fn eval_projection(
        &self,
        mut item_id: ItemId,
        projection: &'tcx List<PlaceElem<'tcx>>,
        span: Span,
    ) -> Result<ItemId, Error> {
        for place_elem in projection {
            item_id = self.eval_place(item_id, place_elem, span)?;
        }

        Ok(item_id)
    }

    fn eval_place(
        &self,
        item_id: ItemId,
        place_elem: PlaceElem<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        match place_elem {
            PlaceElem::Deref => Ok(item_id),
            PlaceElem::Field(idx, _) => Ok(item_id.group().by_idx(idx.as_usize())),
            _ => {
                println!("place elem: {place_elem:?}");
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    pub fn resolve_fn_call(
        &mut self,
        fn_did: impl Into<DefId>,
        fn_generics: GenericArgsRef<'tcx>,
        args: &[Operand<'tcx>],
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let fn_did = fn_did.into();
        let fn_generics = ctx.instantiate(self.tcx, fn_generics);
        println!(
            "resolve {} ({:?})",
            self.tcx.def_path_str(fn_did),
            fn_generics
        );

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
            .map(|arg| self.eval_operand(arg, ctx, span))
            .collect::<Result<Vec<_>, _>>()?;

        let output_ty =
            self.fn_output(fn_did, &ctx.with_generic_args(fn_generics), span)?;

        if self.is_local_def_id(instance_did)
            || self.is_synth(instance_did)
            || self.is_synth(fn_did)
        {
            let module_id = self.eval_fn_mir(instance_did, instance_generics, false)?;
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
            blackbox.eval(self, &args, output_ty, ctx, span)
        }
    }

    pub fn eval_closure_(
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
        let closure_id = self.eval_fn_mir(closure_did, closure_generics, false)?;
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
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let closure = ctx.find_closure(closure_id, span)?;
        let upvars = closure
            .upvars
            .iter()
            .map(|upvar| {
                self.eval_operand(upvar, ctx, span)
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
