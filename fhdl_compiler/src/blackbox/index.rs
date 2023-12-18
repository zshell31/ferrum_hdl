use fhdl_netlist::{group::ItemId, node::Const, sig_ty::NodeTy};
use rustc_hir::Expr;

use super::EvalExpr;
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct UnsignedBit;

impl<'tcx> EvalExpr<'tcx> for UnsignedBit {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec_ty = generator.node_type(rec.hir_id, ctx);
        let rec_ty = generator.find_sig_ty(rec_ty, ctx, rec.span)?;
        let width = rec_ty.width();

        let generics = generator.method_call_generics(expr, ctx)?;
        let idx = generics.as_const(1, rec.span)?;
        let idx = generator
            .netlist
            .add_and_get_out(ctx.module_id, Const::new(NodeTy::BitVec(width), idx, None))
            .into();

        generator.index(rec, idx, ctx)
    }
}

pub struct Index;

impl<'tcx> EvalExpr<'tcx> for Index {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, idx);

        let idx = generator.eval_expr(idx, ctx)?;

        generator.index(rec, idx, ctx)
    }
}
