use fhdl_netlist::{
    group::ItemId,
    net_list::NodeOutId,
    node::{BinOp as NodeBinOp, BinOpNode},
    sig_ty::{NodeTy, SignalTy},
};
use rustc_span::{def_id::DefId, Span};

use super::{cast::Conversion, EvalExpr};
use crate::{
    error::Error,
    eval_context::{EvalContext, ModuleOrItem},
    generator::Generator,
    utils,
};

pub struct BinOp(pub NodeBinOp);

impl BinOp {
    pub fn bin_op<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        lhs: ItemId,
        rhs: ItemId,
        output_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let lhs_ty = generator.item_ty(lhs).node_ty();
        let rhs_ty = generator.item_ty(rhs).node_ty();
        let output_ty = output_ty.node_ty();

        let mut subnode = |expr: ItemId, expr_ty: NodeTy| -> Result<NodeOutId, Error> {
            Ok((if expr_ty != output_ty {
                Conversion::convert_as_prim_ty(
                    ctx.module_id,
                    expr,
                    SignalTy::new(output_ty.into()),
                    generator,
                    span,
                )?
            } else {
                expr
            })
            .node_out_id())
        };

        let lhs = subnode(lhs, lhs_ty)?;
        let rhs = subnode(rhs, rhs_ty)?;

        Ok(generator
            .netlist
            .add_and_get_out(
                ctx.module_id,
                BinOpNode::new(output_ty, self.0, lhs, rhs, None),
            )
            .into())
    }
}

impl<'tcx> EvalExpr<'tcx> for BinOp {
    fn eval_expr(
        &self,
        _generator: &mut crate::generator::Generator<'tcx>,
        _expr: &'tcx rustc_hir::Expr<'tcx>,
        _ctx: &mut crate::eval_context::EvalContext<'tcx>,
    ) -> Result<fhdl_netlist::group::ItemId, crate::error::Error> {
        todo!()
    }

    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[ModuleOrItem],
        output_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
        span: rustc_span::Span,
    ) -> Result<ItemId, Error> {
        utils::args1!(args as lhs, rhs);
        let lhs = lhs.item_id();
        let rhs = rhs.item_id();

        self.bin_op(generator, lhs, rhs, output_ty, ctx, span)
    }
}
