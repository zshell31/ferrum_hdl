use std::{cell::Cell, rc::Rc};

use fhdl_netlist::{group::ItemId, net_list::ModuleId, sig_ty::SignalTy, symbol::Symbol};
use rustc_data_structures::fx::FxIndexMap;
use rustc_hir::{Expr, ExprKind};
use rustc_span::symbol::Ident;

use super::{EvalContext, Generator};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::bitvec::CombineOutputs,
};

#[derive(Debug, Clone)]
struct Captured {
    ident: Ident,
    outer_mod_id: ModuleId,
    outer_item_id: Rc<Cell<ItemId>>,
    local_item_id: Rc<Cell<ItemId>>,
}

impl Captured {
    fn new(
        ident: Ident,
        outer_mod_id: ModuleId,
        outer_item_id: Rc<Cell<ItemId>>,
        local_item_id: Rc<Cell<ItemId>>,
    ) -> Self {
        Self {
            ident,
            outer_mod_id,
            outer_item_id,
            local_item_id,
        }
    }

    #[inline]
    fn is_in_module(&self, module_id: ModuleId) -> bool {
        self.outer_mod_id == module_id
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub inputs: Vec<ItemId>,
    outer_mod_id: ModuleId,
    captured: FxIndexMap<Ident, Captured>,
}

impl Closure {
    pub fn new(outer_mod_id: ModuleId) -> Self {
        Self {
            outer_mod_id,
            inputs: Default::default(),
            captured: Default::default(),
        }
    }

    #[inline]
    fn add_captured(&mut self, captured: Captured) {
        if !self.is_captured(captured.ident) {
            self.captured.insert(captured.ident, captured);
        }
    }

    #[inline]
    fn is_captured(&self, ident: Ident) -> bool {
        self.captured.contains_key(&ident)
    }

    #[inline]
    fn captured(&self, ident: Ident) -> Option<&Captured> {
        self.captured.get(&ident)
    }

    fn captured_iter(&self) -> impl Iterator<Item = &Captured> + '_ {
        self.captured.values()
    }
}

impl<'tcx> Generator<'tcx> {
    // pub fn eval_closure_fn_without_params(
    //     &mut self,
    //     fn_did: DefId,
    //     expr: &Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     let arg = match self.tcx.hir().find_parent(expr.hir_id) {
    //         Some(HirNode::Expr(Expr {
    //             kind: ExprKind::MethodCall(_, _, args, _),
    //             ..
    //         })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
    //         Some(HirNode::Expr(Expr {
    //             kind: ExprKind::Call(_, args),
    //             ..
    //         })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
    //         _ => None,
    //     };

    //     if let Some(arg) = arg {
    //         let span = arg.span;
    //         let ty = self.node_type(arg.hir_id, ctx);
    //         let generic_args = ctx.instantiate(self.tcx, utils::subst(ty).unwrap());
    //         let new_ctx = ctx.with_generic_args(generic_args);

    //         let fn_sig = self.fn_sig(fn_did, &new_ctx);
    //         let input_ty = self.find_sig_ty(fn_sig.inputs()[0], &new_ctx, span)?;
    //         let output_ty = self.find_sig_ty(fn_sig.output(), &new_ctx, span)?;

    //         if self.is_local_def_id(fn_did) {
    //             let input = self.get_closure_inputs_for_sig_ty(input_ty, ctx);

    //             let closure = match self.find_local_impl_id(fn_did, generic_args) {
    //                 Some((impl_id, generic_args)) => self.eval_impl_fn_call(
    //                     impl_id,
    //                     generic_args,
    //                     Some(input.into()),
    //                     [],
    //                     ctx,
    //                     expr.span,
    //                 )?,
    //                 None => self.eval_fn_call(
    //                     fn_did.expect_local(),
    //                     generic_args,
    //                     [input.into()],
    //                     ctx,
    //                     expr.span,
    //                 )?,
    //             };

    //             return Ok(closure);
    //         } else {
    //             let blackbox = self.find_blackbox(fn_did, span)?;

    //             if let Some(from) = blackbox.kind.is_cast() {
    //                 let conversion = Conversion::new(from);
    //                 let from = self.get_closure_inputs_for_sig_ty(input_ty, ctx);

    //                 return match conversion.eval_cast_as_closure(
    //                     self,
    //                     ctx,
    //                     generic_args,
    //                     from,
    //                     expr.span,
    //                 )? {
    //                     Some(closure) => Ok(closure),
    //                     None => conversion.convert(self, expr, ctx, from, output_ty),
    //                 };
    //             };
    //         }
    //     }

    //     Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
    // }

    // fn get_closure_inputs_for_sig_ty(
    //     &mut self,
    //     sig_ty: SignalTy,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> ItemId {
    //     match sig_ty.kind {
    //         SignalTyKind::Node(_) | SignalTyKind::Enum(_) => {
    //             ctx.next_closure_input().into()
    //         }
    //         SignalTyKind::Array(ty) => self
    //             .make_array_group(ty, ty.tys(), |generator, sig_ty| {
    //                 Ok(generator.get_closure_inputs_for_sig_ty(sig_ty, ctx))
    //             })
    //             .unwrap(),
    //         SignalTyKind::Struct(ty) => self
    //             .make_struct_group(
    //                 ty,
    //                 ty.tys().iter().map(|ty| ty.inner),
    //                 |generator, sig_ty| {
    //                     Ok(generator.get_closure_inputs_for_sig_ty(sig_ty, ctx))
    //                 },
    //             )
    //             .unwrap(),
    //     }
    // }

    pub fn closure(&self, module_id: ModuleId) -> &Closure {
        self.closures
            .get(&module_id)
            .expect("No closure for specified module_id")
    }

    pub fn closure_mut(&mut self, module_id: ModuleId) -> &mut Closure {
        self.closures
            .get_mut(&module_id)
            .expect("No closure for specified module_id")
    }

    pub fn is_closure(&self, module_id: ModuleId) -> bool {
        self.closures.get(&module_id).is_some()
    }

    pub fn eval_closure(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        name: Symbol,
        is_inlined: bool,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ModuleId, Error> {
        let expr = if let ExprKind::Closure(expr) = expr.kind {
            expr
        } else {
            return Err(SpanError::new(SpanErrorKind::ExpectedClosure, expr.span).into());
        };

        let closure_id = self.netlist.add_module(name, false);
        self.netlist[closure_id].is_inlined = is_inlined;
        self.closures
            .insert(closure_id, Closure::new(ctx.module_id));

        self.idents.for_module(closure_id).push_scope();

        let mut ctx = ctx.with_module_id(closure_id);

        let body = self.tcx.hir().body(expr.body);
        let inputs = expr.fn_decl.inputs.iter().zip(body.params.iter());
        let inputs = self.eval_inputs(inputs, &mut ctx)?;
        self.closure_mut(closure_id).inputs = inputs;

        let body = self.eval_expr(body.value, &mut ctx)?;

        self.eval_outputs(None, body);

        let captured_outputs = self
            .closure(closure_id)
            .captured_iter()
            .cloned()
            .collect::<Vec<_>>();
        for cap_output in captured_outputs {
            self.eval_outputs(None, cap_output.local_item_id.get());
        }

        self.idents.for_module(closure_id).pop_scope();

        Ok(closure_id)
    }

    pub fn instantiate_closure(
        &mut self,
        closure_id: ModuleId,
        inputs: impl IntoIterator<Item = ItemId>,
        output_ty: SignalTy,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let module_id = ctx.module_id;
        let closure = self.closure(closure_id).clone();

        let mut inputs = inputs.into_iter().collect::<Vec<_>>();
        inputs.reserve(closure.captured.len());

        for cap_input in closure.captured_iter() {
            if cap_input.is_in_module(module_id) {
                inputs.push(cap_input.outer_item_id.get());
            } else if let Some(closure) = self.closures.get(&module_id) {
                match closure.captured(cap_input.ident) {
                    Some(captured) => {
                        inputs.push(captured.local_item_id.get());
                    }
                    None => {
                        let local_item_id = self.add_closure_input(
                            module_id,
                            cap_input.ident,
                            cap_input.outer_mod_id,
                            cap_input.outer_item_id.clone(),
                        );
                        inputs.push(local_item_id);
                    }
                }
            } else {
                return Err(SpanError::missing_item_id(cap_input.ident).into());
            }
        }

        let inst = self.instantiate_module(closure_id, inputs, ctx);
        let mut outputs = CombineOutputs::new(self, inst);
        let res = outputs.next_output(self, output_ty);

        for cap_output in closure.captured_iter() {
            let output_ty = self.item_ty(cap_output.outer_item_id.get());
            let output = outputs.next_output(self, output_ty);

            if cap_output.is_in_module(module_id) {
                cap_output.outer_item_id.replace(output);
            } else if let Some(closure) = self.closures.get(&module_id) {
                match closure.captured(cap_output.ident) {
                    Some(captured) => {
                        captured.local_item_id.replace(output);
                    }
                    None => {
                        self.add_closure_output(
                            module_id,
                            cap_output.ident,
                            cap_output.outer_mod_id,
                            cap_output.outer_item_id.clone(),
                            output,
                        );
                    }
                }
            } else {
                return Err(SpanError::missing_item_id(cap_output.ident).into());
            }
        }

        assert!(!outputs.has_outputs());

        Ok(res)
    }

    pub fn find_item_id_for_ident(
        &mut self,
        module_id: ModuleId,
        ident: Ident,
    ) -> Result<ItemId, Error> {
        match self.idents.for_module(module_id).item_id_opt(ident) {
            Some(item_id) => Ok(item_id.get()),
            None => {
                if self.is_closure(module_id) {
                    let (outer_mod_id, outer_item_id) = self.capture(module_id, ident)?;
                    let local_item_id = self.add_closure_input(
                        module_id,
                        ident,
                        outer_mod_id,
                        outer_item_id,
                    );

                    Ok(local_item_id)
                } else {
                    Err(SpanError::missing_item_id(ident).into())
                }
            }
        }
    }

    pub fn replace_item_id_for_ident(
        &mut self,
        module_id: ModuleId,
        ident: Ident,
        item_id: ItemId,
    ) -> Result<(), Error> {
        match self.idents.for_module(module_id).item_id_opt(ident) {
            Some(_) => {
                self.idents
                    .for_module(module_id)
                    .replace_local_ident(ident, item_id);

                Ok(())
            }
            None => {
                if self.is_closure(module_id) {
                    let (outer_mod_id, outer_item_id) = self.capture(module_id, ident)?;
                    self.add_closure_output(
                        module_id,
                        ident,
                        outer_mod_id,
                        outer_item_id,
                        item_id,
                    );

                    Ok(())
                } else {
                    Err(SpanError::missing_item_id(ident).into())
                }
            }
        }
    }

    fn capture(
        &mut self,
        module_id: ModuleId,
        ident: Ident,
    ) -> Result<(ModuleId, Rc<Cell<ItemId>>), Error> {
        if let Some(outer_mod_id) = self
            .closures
            .get(&module_id)
            .map(|closure| closure.outer_mod_id)
        {
            match self.idents.for_module(outer_mod_id).item_id_opt(ident) {
                Some(item_id) => Ok((outer_mod_id, item_id)),
                None => self.capture(outer_mod_id, ident),
            }
        } else {
            let item_id = self.idents.for_module(module_id).item_id(ident)?;
            Ok((module_id, item_id))
        }
    }

    fn add_closure_input(
        &mut self,
        closure_id: ModuleId,
        ident: Ident,
        outer_mod_id: ModuleId,
        outer_item_id: Rc<Cell<ItemId>>,
    ) -> ItemId {
        let input_ty = self.item_ty(outer_item_id.get());
        let input = self.make_input_with_sig_ty(input_ty, closure_id);
        self.assign_names_to_item(ident.as_str(), input);
        let local_item_id = self
            .idents
            .for_module(closure_id)
            .add_local_ident(ident, input)
            .unwrap();

        let captured =
            Captured::new(ident, outer_mod_id, outer_item_id, local_item_id.clone());
        self.closure_mut(closure_id).add_captured(captured);

        local_item_id.get()
    }

    fn add_closure_output(
        &mut self,
        closure_id: ModuleId,
        ident: Ident,
        outer_mod_id: ModuleId,
        outer_item_id: Rc<Cell<ItemId>>,
        local_item_id: ItemId,
    ) {
        let local_item_id = self
            .idents
            .for_module(closure_id)
            .add_local_ident(ident, local_item_id)
            .unwrap();

        let captured = Captured::new(ident, outer_mod_id, outer_item_id, local_item_id);
        self.closure_mut(closure_id).add_captured(captured);
    }
}
