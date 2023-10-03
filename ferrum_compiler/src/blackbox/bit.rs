use ferrum_netlist::{group::ItemId, node::Const, sig_ty::PrimTy};
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{
    error::Error,
    generator::{EvalContext, Generator},
};

pub struct BitVal(pub bool);

impl BitVal {
    fn create_bit_value<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let value = match self.0 {
            true => 1,
            false => 0,
        };

        let cons = Const::new(
            PrimTy::Bit,
            value,
            generator.idents.for_module(ctx.module_id).tmp(),
        );
        Ok(generator.net_list.add_node(ctx.module_id, cons).into())
    }
}

impl<'tcx> EvaluateExpr<'tcx> for BitVal {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        _: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        self.create_bit_value(generator, ctx)
    }
}
