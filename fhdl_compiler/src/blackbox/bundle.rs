use super::EvalExpr;

pub struct Unbundle;

impl<'tcx> EvalExpr<'tcx> for Unbundle {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as rec);

    //     compiler.eval_expr(rec, ctx)
    // }
}

pub struct Bundle;

impl<'tcx> EvalExpr<'tcx> for Bundle {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as rec);

    //     compiler.eval_expr(rec, ctx)
    // }
}
