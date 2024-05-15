use fhdl_netlist::node::{BinOp as NodeBinOp, BinOpArgs, BinOpNode};
use rustc_middle::{mir::BinOp as MirBinOp, ty::Ty};
use rustc_span::Span;

use super::{args, cast::CastFrom, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ModuleExt},
        item_ty::ItemTy,
        Compiler, Context,
    },
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
                    if let Some(&cons) = expr.const_opt() {
                        Item::new(output_ty, cons.convert(output_ty.width()))
                    } else {
                        CastFrom::convert(expr, output_ty, ctx, span)?
                    }
                } else {
                    expr.clone()
                })
            };

        let lhs = subnode(lhs, lhs.ty)?;
        let rhs = subnode(rhs, rhs.ty)?;
        let bin_op = self.0;

        if let (Some(&lhs), Some(&rhs)) = (lhs.const_opt(), rhs.const_opt()) {
            Ok(Item::new(output_ty, lhs.eval_bin_op(rhs, bin_op)))
        } else {
            let lhs = ctx.module.to_bitvec(&lhs, span)?.port();
            let rhs = ctx.module.to_bitvec(&rhs, span)?.port();

            Ok(Item::new(
                output_ty,
                ctx.module.add_and_get_port::<_, BinOpNode>(BinOpArgs {
                    ty: output_ty.node_ty(),
                    bin_op,
                    lhs,
                    rhs,
                    sym: None,
                }),
            ))
        }
    }
}

impl<'tcx> EvalExpr<'tcx> for BinOp {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: Ty<'tcx>,
        ctx: &mut Context<'tcx>,
        span: rustc_span::Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as lhs, rhs);

        let output_ty = compiler.resolve_fn_out_ty(output_ty, span)?;
        self.bin_op(lhs, rhs, output_ty, ctx, span)
    }
}
