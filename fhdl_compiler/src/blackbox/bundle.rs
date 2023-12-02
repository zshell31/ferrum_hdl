use fhdl_netlist::group::ItemId;
use rustc_hir::Expr;

use super::{Blackbox, EvaluateExpr};
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct Unbundle;

impl<'tcx> EvaluateExpr<'tcx> for Unbundle {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct Bundle;

impl<'tcx> EvaluateExpr<'tcx> for Bundle {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}
