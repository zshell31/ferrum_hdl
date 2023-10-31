use fhdl_netlist::group::ItemId;
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct StdClone;

impl<'tcx> EvaluateExpr<'tcx> for StdClone {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}
