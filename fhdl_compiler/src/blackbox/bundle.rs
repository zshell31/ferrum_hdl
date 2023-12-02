use fhdl_netlist::group::ItemId;
use rustc_hir::Expr;

use super::{Blackbox, EvalExpr};
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct Unbundle;

impl<'tcx> EvalExpr<'tcx> for Unbundle {
    fn eval_expr(
        &self,
        _: &Blackbox,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct Bundle;

impl<'tcx> EvalExpr<'tcx> for Bundle {
    fn eval_expr(
        &self,
        _: &Blackbox,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}
