use ferrum_hdl::{bit::bit_value, unsigned::unsigned_value};
use fhdl_netlist::sig_ty::NodeTy;
use rustc_ast::LitKind;
use rustc_hir::Lit;

use crate::error::{Error, SpanError, SpanErrorKind};

pub fn evaluate_lit(prim_ty: NodeTy, lit: &Lit) -> Result<u128, Error> {
    match prim_ty {
        NodeTy::Bool => evaluate_bit_lit(lit),
        NodeTy::Bit => evaluate_bit_lit(lit),
        NodeTy::U8 | NodeTy::U16 | NodeTy::U32 | NodeTy::U64 | NodeTy::U128 => {
            evaluate_unsigned_lit(lit, prim_ty.width())
        }
        NodeTy::Unsigned(n) => evaluate_unsigned_lit(lit, n),
        NodeTy::Enum(_) | NodeTy::BitVec(_) | NodeTy::Clock | NodeTy::ClockDomain => Err(
            SpanError::new(SpanErrorKind::PrimTyWithoutValue(NodeTy::Clock), lit.span)
                .into(),
        ),
    }
}

fn evaluate_bit_lit(lit: &Lit) -> Result<u128, Error> {
    match lit.node {
        LitKind::Bool(b) => Ok(bit_value(b)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(NodeTy::Bit),
            lit.span,
        )
        .into()),
    }
}

fn evaluate_unsigned_lit(lit: &Lit, width: u128) -> Result<u128, Error> {
    match lit.node {
        LitKind::Int(n, _) => Ok(unsigned_value(n, width)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(NodeTy::Unsigned(width)),
            lit.span,
        )
        .into()),
    }
}
