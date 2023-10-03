use ferrum_netlist::group::ItemId;
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{
    error::Error,
    generator::{EvalContext, Generator},
    utils,
};

pub struct Unbundle;

impl<'tcx> EvaluateExpr<'tcx> for Unbundle {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        generator.evaluate_expr(rec, ctx)
    }
}

pub struct Bundle;

impl<'tcx> EvaluateExpr<'tcx> for Bundle {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        generator.evaluate_expr(rec, ctx)
    }
}
