use ferrum::{bit::Bit, unsigned::Unsigned};
use ferrum_blackbox::Blackbox;
use ferrum_netlist::{
    group::ItemId,
    node::{IsNode, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_hir::{Expr, ExprKind, QPath};
use rustc_span::Span;

use super::EvaluateExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::{EvalContext, Generator},
    utils,
};

pub struct Cast;

impl<'tcx> EvaluateExpr<'tcx> for Cast {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let from = generator.find_sig_ty(
            generator.node_type(rec.hir_id, ctx),
            ctx.generic_args,
            rec.span,
        )?;
        let target = generator.find_sig_ty(
            generator.node_type(expr.hir_id, ctx),
            ctx.generic_args,
            expr.span,
        )?;

        StdConversion::convert_for_expr(from, target, generator, rec, ctx)
    }
}

#[allow(dead_code)]
pub struct StdConversion {
    pub from: bool,
}

impl StdConversion {
    fn make_err(&self, span: Span) -> Error {
        SpanError::new(
            SpanErrorKind::NotSynthBlackboxExpr(if self.from {
                Blackbox::StdFrom
            } else {
                Blackbox::StdInto
            }),
            span,
        )
        .into()
    }

    pub fn convert(
        from: SignalTy,
        target: SignalTy,
        generator: &mut Generator<'_>,
        item_id: ItemId,
        span: Span,
    ) -> Result<ItemId, Error> {
        if from == target {
            return Ok(item_id);
        }

        let node_id = item_id.node_id();
        let module_id = node_id.module_id();
        let node_out = generator.net_list[node_id]
            .kind
            .outputs_mut()
            .only_one_mut();
        let node_out_id = node_out.node_out_id(node_id);
        let node_out = node_out.out;

        match (from, target) {
            (SignalTy::Prim(PrimTy::Bool), SignalTy::Prim(PrimTy::Bit)) => {
                assert_convert::<bool, Bit>();
                node_out.ty = PrimTy::Bit;
                Ok(item_id)
            }
            (SignalTy::Prim(PrimTy::Bit), SignalTy::Prim(PrimTy::Bool)) => {
                assert_convert::<Bit, bool>();
                node_out.ty = PrimTy::Bool;
                Ok(item_id)
            }
            (SignalTy::Prim(PrimTy::U128), SignalTy::Prim(PrimTy::Unsigned(n))) => {
                assert_convert::<u128, Unsigned<1>>();
                Ok(generator
                    .net_list
                    .add_node(
                        module_id,
                        Splitter::new(
                            node_out_id,
                            [(
                                PrimTy::Unsigned(n),
                                generator.idents.for_module(module_id).tmp(),
                            )],
                            None,
                            false,
                        ),
                    )
                    .into())
            }
            _ => {
                println!("from: {:?}", from);
                println!("to: {:?}", target);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    fn convert_for_expr<'tcx>(
        from: SignalTy,
        target: SignalTy,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let item_id = generator.evaluate_expr(expr, ctx)?;
        Self::convert(from, target, generator, item_id, expr.span)
    }
}

fn assert_convert<F, T: From<F>>() {}

impl<'tcx> EvaluateExpr<'tcx> for StdConversion {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match expr.kind {
            ExprKind::Call(rec, args) => {
                let from = generator.find_sig_ty(
                    generator.node_type(args[0].hir_id, ctx),
                    ctx.generic_args,
                    args[0].span,
                )?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator.find_sig_ty(
                        generator.node_type(ty.hir_id, ctx),
                        ctx.generic_args,
                        rec.span,
                    )?,
                    _ => {
                        return Err(self.make_err(rec.span));
                    }
                };

                Self::convert_for_expr(from, target, generator, &args[0], ctx)
            }
            ExprKind::MethodCall(_, rec, _, span) => {
                let from = generator.find_sig_ty(
                    generator.node_type(rec.hir_id, ctx),
                    ctx.generic_args,
                    rec.span,
                )?;
                let target = generator.find_sig_ty(
                    generator.node_type(expr.hir_id, ctx),
                    ctx.generic_args,
                    span,
                )?;

                Self::convert_for_expr(from, target, generator, rec, ctx)
            }
            _ => Err(self.make_err(expr.span)),
        }
    }
}
