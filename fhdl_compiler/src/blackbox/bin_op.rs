use fhdl_netlist::node::{BinOp as NodeBinOp, BinOpArgs, BinOpNode};
use rustc_middle::mir::BinOp as MirBinOp;
use rustc_span::Span;

use super::{args, cast::Conversion, EvalExpr};
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context},
    error::{Error, SpanError, SpanErrorKind},
};

pub struct BinOp(pub NodeBinOp);

impl BinOp {
    pub fn try_from_op(
        lhs_ty: ItemTy<'_>,
        op: MirBinOp,
        span: Span,
    ) -> Result<Self, Error> {
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
            Shl | ShlUnchecked => NodeBinOp::Sll,
            Shr | ShrUnchecked if !lhs_ty.is_signed() => NodeBinOp::Slr,
            Shr | ShrUnchecked if lhs_ty.is_signed() => NodeBinOp::Sra,
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
        lhs: &Item<'tcx>,
        rhs: &Item<'tcx>,
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let _ = lhs.ty.node_ty();
        let _ = rhs.ty.node_ty();

        let should_convert_operands = self.0.should_convert_operands();
        let mut subnode =
            |expr: &Item<'tcx>, expr_ty: ItemTy<'tcx>| -> Result<Item<'tcx>, Error> {
                Ok(if should_convert_operands && expr_ty != output_ty {
                    Conversion::convert(expr, output_ty, ctx, span)?
                } else {
                    expr.clone()
                })
            };

        let lhs = subnode(lhs, lhs.ty)?.port();
        let rhs = subnode(rhs, rhs.ty)?.port();

        Ok(Item::new(
            output_ty,
            ctx.module.add_and_get_port::<_, BinOpNode>(BinOpArgs {
                ty: output_ty.node_ty(),
                bin_op: self.0,
                lhs,
                rhs,
                sym: None,
            }),
        ))
    }
}

impl<'tcx> EvalExpr<'tcx> for BinOp {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: rustc_span::Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as lhs, rhs);

        self.bin_op(lhs, rhs, output_ty, ctx, span)
    }
}
