use fhdl_netlist::node::{BitNot as BitNotNode, BitNotArgs};
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context},
    error::Error,
};

pub struct BitNot;

impl BitNot {
    pub fn not<'tcx>(
        _: &mut Compiler<'tcx>,
        expr: &Item<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Result<Item<'tcx>, Error> {
        let ty = expr.ty;

        Ok(Item::new(
            ty,
            ctx.module.add_and_get_port::<_, BitNotNode>(BitNotArgs {
                ty: ty.node_ty(),
                input: expr.port(),
                sym: None,
            }),
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
