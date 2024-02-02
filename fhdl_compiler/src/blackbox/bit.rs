use super::EvalExpr;

pub struct BitVal(pub bool);

impl BitVal {
    // pub fn bit_value(&self) -> u128 {
    //     match self.0 {
    //         true => 1,
    //         false => 0,
    //     }
    // }

    // fn add_const<'tcx>(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     ctx: &EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     let value = match self.0 {
    //         true => 1,
    //         false => 0,
    //     };

    //     let cons = Const::new(NodeTy::Bit, value.into(), None);
    //     Ok(compiler
    //         .netlist
    //         .add_and_get_out(ctx.module_id, cons)
    //         .into())
    // }
}

impl<'tcx> EvalExpr<'tcx> for BitVal {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     _: &Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     self.add_const(compiler, ctx)
    // }
}
