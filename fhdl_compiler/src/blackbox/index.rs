use super::EvalExpr;

pub struct UnsignedBit;

impl<'tcx> EvalExpr<'tcx> for UnsignedBit {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as rec);

    //     let rec_ty = compiler.node_type(rec.hir_id, ctx);
    //     let rec_ty = compiler.find_sig_ty(rec_ty, ctx, rec.span)?;
    //     let width = rec_ty.width();

    //     let generics = compiler.method_call_generics(expr, ctx)?;
    //     let idx = generics.as_const(1, rec.span)?;
    //     let idx = compiler
    //         .netlist
    //         .add_and_get_out(ctx.module_id, Const::new(NodeTy::BitVec(width), idx, None))
    //         .into();

    //     compiler.index(rec, idx, ctx)
    // }
}

pub struct Index;

impl<'tcx> EvalExpr<'tcx> for Index {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as rec, idx);

    //     let idx = compiler.eval_expr(idx, ctx)?;

    //     compiler.index(rec, idx, ctx)
    // }
}
