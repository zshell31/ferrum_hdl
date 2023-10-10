use fhdl_netlist::group::ItemId;
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{
    error::Error,
    generator::{EvalContext, Generator},
    utils,
};

pub struct StdClone;

impl<'tcx> EvaluateExpr<'tcx> for StdClone {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.evaluate_expr(rec, ctx)
    }
}
