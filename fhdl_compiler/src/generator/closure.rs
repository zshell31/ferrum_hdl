use fhdl_netlist::{
    group::ItemId,
    sig_ty::{SignalTy, SignalTyKind},
};
use rustc_hir::{def_id::DefId, Expr, ExprKind, Node as HirNode};

use super::{EvalContext, Generator};
use crate::{
    blackbox::cast::Conversion,
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

impl<'tcx> Generator<'tcx> {
    pub fn eval_closure_fn_without_params(
        &mut self,
        fn_id: DefId,
        expr: &Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let arg = match self.tcx.hir().find_parent(expr.hir_id) {
            Some(HirNode::Expr(Expr {
                kind: ExprKind::MethodCall(_, _, args, _),
                ..
            })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
            Some(HirNode::Expr(Expr {
                kind: ExprKind::Call(_, args),
                ..
            })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
            _ => None,
        };

        if let Some(arg) = arg {
            let span = arg.span;
            let ty = self.node_type(arg.hir_id, ctx);
            let generic_args = ctx.instantiate(self.tcx, utils::subst(ty).unwrap());

            let fn_sig = self.fn_sig(fn_id, Some(generic_args));
            let input_ty = self.find_sig_ty(fn_sig.inputs()[0], generic_args, span)?;
            let output_ty = self.find_sig_ty(fn_sig.output(), generic_args, span)?;

            if fn_id.is_local() {
                let input = self.get_closure_inputs_for_sig_ty(input_ty, ctx);

                let closure = match self.find_local_impl_id(fn_id, generic_args) {
                    Some((impl_id, generic_args)) => self.eval_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some(input.into()),
                        [],
                        ctx,
                        expr.span,
                    )?,
                    None => self.eval_fn_call(
                        fn_id.expect_local(),
                        generic_args,
                        [input.into()],
                        ctx,
                        expr.span,
                    )?,
                };

                return Ok(closure);
            } else {
                let blackbox = self.find_blackbox(fn_id, ctx.generic_args, span)?;
                if let Some(from) = blackbox.kind.is_cast() {
                    let conversion = Conversion::new(from);
                    let from = self.get_closure_inputs_for_sig_ty(input_ty, ctx);

                    return match conversion.eval_cast_as_closure(
                        self,
                        ctx,
                        generic_args,
                        from,
                        expr.span,
                    )? {
                        Some(closure) => Ok(closure),
                        None => conversion
                            .convert(&blackbox, self, expr, ctx, from, output_ty),
                    };
                }
            }
        }
        Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
    }

    fn get_closure_inputs_for_sig_ty(
        &mut self,
        sig_ty: SignalTy,
        ctx: &mut EvalContext<'tcx>,
    ) -> ItemId {
        match sig_ty.kind {
            SignalTyKind::Node(_) | SignalTyKind::Enum(_) => {
                ctx.next_closure_input().into()
            }
            SignalTyKind::Array(ty) => self
                .make_array_group(ty, ty.tys(), |generator, sig_ty| {
                    Ok(generator.get_closure_inputs_for_sig_ty(sig_ty, ctx))
                })
                .unwrap(),
            SignalTyKind::Struct(ty) => self
                .make_struct_group(
                    ty,
                    ty.tys().iter().map(|ty| ty.inner),
                    |generator, sig_ty| {
                        Ok(generator.get_closure_inputs_for_sig_ty(sig_ty, ctx))
                    },
                )
                .unwrap(),
        }
    }
}
