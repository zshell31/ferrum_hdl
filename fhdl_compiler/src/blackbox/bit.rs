use fhdl_netlist::{group::ItemId, node::Const, sig_ty::NodeTy};
use rustc_hir::Expr;

use super::{Blackbox, EvalExpr};
use crate::{error::Error, eval_context::EvalContext, generator::Generator};

pub struct BitVal(pub bool);

impl BitVal {
    pub fn bit_value(&self) -> u128 {
        match self.0 {
            true => 1,
            false => 0,
        }
    }

    fn add_const<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let value = match self.0 {
            true => 1,
            false => 0,
        };

        let cons = Const::new(NodeTy::Bit, value, None);
        Ok(generator
            .net_list
            .add_and_get_out(ctx.module_id, cons)
            .into())
    }
}

impl<'tcx> EvalExpr<'tcx> for BitVal {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        _: &Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        self.add_const(generator, ctx)
    }
}
