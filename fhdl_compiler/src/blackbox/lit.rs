// use ferrum_hdl::{bit::bit_value, unsigned::unsigned_value};
// use fhdl_netlist::sig_ty::NodeTy;
// use rustc_ast::LitKind;
// use rustc_hir::Lit;

// use crate::error::{Error, SpanError, SpanErrorKind};

// pub fn eval_lit(node_ty: NodeTy, lit: &Lit) -> Result<u128, Error> {
//     match node_ty {
//         NodeTy::Bit => eval_bit_lit(lit),
//         NodeTy::Unsigned(_) => eval_unsigned_lit(lit, node_ty.width()),
//         NodeTy::BitVec(_) | NodeTy::Clock | NodeTy::ClockDomain => Err(SpanError::new(
//             SpanErrorKind::UnexpectedLitValue(node_ty),
//             lit.span,
//         )
//         .into()),
//     }
// }

// fn eval_bit_lit(lit: &Lit) -> Result<u128, Error> {
//     match lit.node {
//         LitKind::Bool(b) => Ok(bit_value(b)),
//         _ => Err(SpanError::new(
//             SpanErrorKind::UnexpectedLitValue(NodeTy::Bit),
//             lit.span,
//         )
//         .into()),
//     }
// }

// fn eval_unsigned_lit(lit: &Lit, width: u128) -> Result<u128, Error> {
//     match lit.node {
//         LitKind::Int(n, _) => Ok(unsigned_value(n, width)),
//         _ => Err(SpanError::new(
//             SpanErrorKind::UnexpectedLitValue(NodeTy::Unsigned(width)),
//             lit.span,
//         )
//         .into()),
//     }
// }
