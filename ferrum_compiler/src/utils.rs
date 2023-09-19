use ferrum_netlist::node::BinOp;
use rustc_const_eval::interpret::{ConstValue, Scalar};
use rustc_hir::{def_id::DefId, BinOpKind, Expr, ExprKind, Pat, PathSegment};
use rustc_middle::ty::{GenericArgsRef, ScalarInt, Ty, TyKind, ValTree};
use rustc_span::{symbol::Ident, Span};

use crate::error::{Error, SpanError, SpanErrorKind};

// pub fn def_id_for_hir_ty(ty: &HirTy<'_>) -> Option<DefId> {
//     match ty.kind {
//         HirTyKind::Path(QPath::Resolved(
//             _,
//             Path {
//                 res: Res::Def(_, def_id),
//                 ..
//             },
//         )) => Some(*def_id),
//         _ => None,
//     }
// }

pub fn to_bin_op(kind: BinOpKind) -> BinOp {
    match kind {
        BinOpKind::Add => BinOp::Add,
        BinOpKind::And => BinOp::And,
        BinOpKind::BitAnd => BinOp::BitAnd,
        BinOpKind::BitOr => BinOp::BitOr,
        BinOpKind::BitXor => BinOp::BitXor,
        BinOpKind::Div => BinOp::Div,
        BinOpKind::Eq => BinOp::Eq,
        BinOpKind::Ge => BinOp::Ge,
        BinOpKind::Gt => BinOp::Gt,
        BinOpKind::Le => BinOp::Le,
        BinOpKind::Lt => BinOp::Lt,
        BinOpKind::Mul => BinOp::Mul,
        BinOpKind::Ne => BinOp::Ne,
        BinOpKind::Or => BinOp::Or,
        BinOpKind::Rem => BinOp::Rem,
        BinOpKind::Shl => BinOp::Shl,
        BinOpKind::Shr => BinOp::Shr,
        BinOpKind::Sub => BinOp::Sub,
    }
}

pub fn ty_def_id(ty: Ty<'_>) -> Option<DefId> {
    match ty.kind() {
        TyKind::Adt(adt, _) => Some(adt.did()),
        TyKind::FnDef(did, _) => Some(*did),
        _ => None,
    }
}

pub fn subst(ty: Ty<'_>) -> GenericArgsRef<'_> {
    match ty.kind() {
        TyKind::Adt(_, subst) => subst,
        TyKind::FnDef(_, subst) => subst,
        _ => panic!("expected substitution for type '{}'", ty),
    }
}

pub fn subst_type(ty: Ty<'_>, index: usize) -> Option<Ty<'_>> {
    subst(ty).get(index).and_then(|generic| generic.as_type())
}

pub fn expected_call<'a, 'tcx>(
    expr: &'a Expr<'tcx>,
) -> Result<(&'a Expr<'tcx>, &'a [Expr<'tcx>]), Error>
where
    'tcx: 'a,
{
    match expr.kind {
        ExprKind::Call(rec, args) => Ok((rec, args)),
        _ => Err(SpanError::new(SpanErrorKind::ExpectedCall, expr.span).into()),
    }
}

pub fn exptected_method_call<'a, 'tcx>(
    expr: &'a Expr<'tcx>,
) -> Result<
    (
        &'a PathSegment<'tcx>,
        &'a Expr<'tcx>,
        &'a [Expr<'tcx>],
        Span,
    ),
    Error,
>
where
    'tcx: 'a,
{
    match expr.kind {
        ExprKind::MethodCall(method, rec, args, span) => Ok((method, rec, args, span)),
        _ => Err(SpanError::new(SpanErrorKind::ExpectedMethodCall, expr.span).into()),
    }
}

pub fn pat_ident(pat: &Pat<'_>) -> Result<Ident, Error> {
    pat.simple_ident()
        .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedIdentifier, pat.span))
        .map_err(Into::into)
}

// pub fn eval_scalar(scalar: Scalar) -> Option<u128> {
//     scalar.
// }

pub fn eval_const_val(value: ConstValue) -> Option<u128> {
    match value {
        ConstValue::Scalar(Scalar::Int(scalar)) => eval_scalar_int(scalar),
        _ => None,
    }
}

pub fn eval_val_tree(val_tree: ValTree) -> Option<u128> {
    val_tree.try_to_scalar_int().and_then(eval_scalar_int)
}

pub fn eval_scalar_int(scalar: ScalarInt) -> Option<u128> {
    scalar
        .try_to_u128()
        .ok()
        .or_else(|| scalar.try_to_u64().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u32().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u16().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u8().ok().map(|n| n as u128))
}
// pub fn closure_input1_def_id(expr: &Expr<'_>) -> Result<(DefId, Span), Error> {
//     match expr.kind {
//         ExprKind::Closure(Closure {
//             fn_decl:
//                 FnDecl {
//                     inputs:
//                         [HirTy {
//                             kind:
//                                 HirTyKind::Path(QPath::Resolved(
//                                     _,
//                                     Path {
//                                         res: Res::Def(_, def_id),
//                                         ..
//                                     },
//                                 )),
//                             span,
//                             ..
//                         }],
//                     ..
//                 },
//             ..
//         }) => Ok((*def_id, *span)),
//         _ => Err(
//             SpanError::new(SpanErrorKind::ExpectedClosureWithParams, expr.span).into(),
//         ),
//     }
// }
