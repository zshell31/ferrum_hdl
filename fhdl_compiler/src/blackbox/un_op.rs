use fhdl_netlist::node::BitNot as BitNotNode;
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context},
    error::Error,
};

pub struct BitNot;

impl BitNot {
    pub fn not<'tcx>(
        compiler: &mut Compiler<'tcx>,
        expr: &Item<'tcx>,
        ctx: &Context<'tcx>,
    ) -> Result<Item<'tcx>, Error> {
        let ty = expr.ty;

        Ok(Item::new(
            ty,
            compiler.netlist.add_and_get_out(
                ctx.module_id,
                BitNotNode::new(ty.node_ty(), expr.node_out_id(), None),
            ),
        ))
    }
}

impl<'tcx> EvalExpr<'tcx> for BitNot {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as expr);

        Self::not(compiler, expr, ctx)
    }
}
