use fhdl_netlist::group::ItemId;
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct Unbundle;

impl<'tcx> EvaluateExpr<'tcx> for Unbundle {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.evaluate_expr(rec, ctx)
    }
}

pub struct Bundle;

impl<'tcx> EvaluateExpr<'tcx> for Bundle {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.evaluate_expr(rec, ctx)
    }
}
