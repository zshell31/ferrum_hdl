use ferrum_netlist::group::ItemId;
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{
    error::Error,
    generator::{EvalContext, Generator},
    utils,
};

pub struct UnsignedIndex;

impl<'tcx> EvaluateExpr<'tcx> for UnsignedIndex {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let generics = generator.method_call_generics(expr, ctx)?;
        let ind: u128 = generics.as_const(1).unwrap();

        generator.index(rec, ind, ctx)
    }
}
