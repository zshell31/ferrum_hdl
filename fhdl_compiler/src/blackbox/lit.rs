use ferrum_hdl::{bit::bit_value, unsigned::unsigned_value};
use fhdl_netlist::sig_ty::{ConstParam, NodeTy};
use rustc_ast::LitKind;
use rustc_hir::Lit;

use crate::error::{Error, SpanError, SpanErrorKind};

pub fn eval_lit(prim_ty: NodeTy, lit: &Lit) -> Result<u128, Error> {
    match prim_ty {
        NodeTy::Bool => eval_bit_lit(lit),
        NodeTy::Bit => eval_bit_lit(lit),
        NodeTy::U8
        | NodeTy::U16
        | NodeTy::U32
        | NodeTy::U64
        | NodeTy::U128
        | NodeTy::Usize => eval_unsigned_lit(lit, prim_ty.width()),
        NodeTy::Unsigned(n) => eval_unsigned_lit(lit, n),
        NodeTy::Enum(_) | NodeTy::BitVec(_) | NodeTy::Clock | NodeTy::ClockDomain => Err(
            SpanError::new(SpanErrorKind::PrimTyWithoutValue(NodeTy::Clock), lit.span)
                .into(),
        ),
    }
}

fn eval_bit_lit(lit: &Lit) -> Result<u128, Error> {
    match lit.node {
        LitKind::Bool(b) => Ok(bit_value(b)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(NodeTy::Bit),
            lit.span,
        )
        .into()),
    }
}

fn eval_unsigned_lit(lit: &Lit, width: ConstParam) -> Result<u128, Error> {
    match lit.node {
        LitKind::Int(n, _) if width.is_value() => Ok(unsigned_value(n, width.value())),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(NodeTy::Unsigned(width)),
            lit.span,
        )
        .into()),
    }
}
