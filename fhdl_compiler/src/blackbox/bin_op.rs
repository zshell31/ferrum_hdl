use fhdl_netlist::node::{BinOp as NodeBinOp, BinOpNode};
use rustc_middle::mir::BinOp as MirBinOp;
use rustc_span::Span;

use super::{cast::Conversion, EvalExpr};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::{item::Item, item_ty::ItemTy, Generator},
    utils,
};

pub struct BinOp(pub NodeBinOp);

impl BinOp {
    pub fn try_from_op(op: MirBinOp, span: Span) -> Result<Self, Error> {
        use MirBinOp::*;

        Ok(Self(match op {
            BitAnd => NodeBinOp::BitAnd,
            BitOr => NodeBinOp::BitOr,
            BitXor => NodeBinOp::BitXor,
            Add | AddUnchecked => NodeBinOp::Add,
            Sub | SubUnchecked => NodeBinOp::Sub,
            Mul | MulUnchecked => NodeBinOp::Mul,
            Rem => NodeBinOp::Rem,
            Div => NodeBinOp::Div,
            Shl | ShlUnchecked => NodeBinOp::Shl,
            Shr | ShrUnchecked => NodeBinOp::Shr,
            Eq => NodeBinOp::Eq,
            Ge => NodeBinOp::Ge,
            Gt => NodeBinOp::Gt,
            Le => NodeBinOp::Le,
            Lt => NodeBinOp::Lt,
            Offset => {
                return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
            }
            _ => todo!(),
        }))
    }

    pub fn bin_op<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        lhs: &Item<'tcx>,
        rhs: &Item<'tcx>,
        output_ty: ItemTy<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let _ = lhs.ty.node_ty();
        let _ = rhs.ty.node_ty();

        let should_convert_operands = self.0.should_convert_operands();
        let mut subnode =
            |expr: &Item<'tcx>, expr_ty: ItemTy<'tcx>| -> Result<Item<'tcx>, Error> {
                Ok(if should_convert_operands && expr_ty != output_ty {
                    Conversion::convert(generator, ctx.module_id, expr, output_ty, span)?
                } else {
                    expr.clone()
                })
            };

        let lhs = subnode(lhs, lhs.ty)?.node_out_id();
        let rhs = subnode(rhs, rhs.ty)?.node_out_id();

        Ok(Item::new(
            output_ty,
            generator.netlist.add_and_get_out(
                ctx.module_id,
                BinOpNode::new(output_ty.node_ty(), self.0, lhs, rhs, None),
            ),
        ))
    }
}

impl<'tcx> EvalExpr<'tcx> for BinOp {
    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: rustc_span::Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as lhs, rhs);

        self.bin_op(generator, lhs, rhs, output_ty, ctx, span)
    }
}
