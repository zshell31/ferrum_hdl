use ferrum_hdl::{bit::bit_value, unsigned::unsigned_value};
use fhdl_netlist::sig_ty::PrimTy;
use rustc_ast::LitKind;
use rustc_hir::Lit;

use crate::error::{Error, SpanError, SpanErrorKind};

pub fn evaluate_lit(prim_ty: PrimTy, lit: &Lit) -> Result<u128, Error> {
    match prim_ty {
        PrimTy::Bool => evaluate_bit_lit(lit),
        PrimTy::Bit => evaluate_bit_lit(lit),
        PrimTy::U8 | PrimTy::U16 | PrimTy::U32 | PrimTy::U64 | PrimTy::U128 => {
            evaluate_unsigned_lit(lit, prim_ty.width())
        }
        PrimTy::Unsigned(n) => evaluate_unsigned_lit(lit, n),
        PrimTy::Enum(_) | PrimTy::BitVec(_) | PrimTy::Clock | PrimTy::ClockDomain => Err(
            SpanError::new(SpanErrorKind::PrimTyWithoutValue(PrimTy::Clock), lit.span)
                .into(),
        ),
    }
}

fn evaluate_bit_lit(lit: &Lit) -> Result<u128, Error> {
    match lit.node {
        LitKind::Bool(b) => Ok(bit_value(b)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(PrimTy::Bit),
            lit.span,
        )
        .into()),
    }
}

fn evaluate_unsigned_lit(lit: &Lit, width: u128) -> Result<u128, Error> {
    match lit.node {
        LitKind::Int(n, _) => Ok(unsigned_value(n, width)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(PrimTy::Unsigned(width)),
            lit.span,
        )
        .into()),
    }
}
