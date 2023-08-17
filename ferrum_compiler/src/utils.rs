use rustc_hir::{Expr, ExprKind, Pat};
use rustc_span::symbol::Ident;

use crate::error::{Error, SpanError, SpanErrorKind};

pub fn pat_ident(pat: &Pat<'_>) -> Result<Ident, Error> {
    pat.simple_ident()
        .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedIdentifier, pat.span))
        .map_err(Into::into)
}

// pub fn def_id_for_hir_ty(ty: &HirTy<'_>) -> Result<DefId, Error> {
//     match ty.kind {
//         HirTyKind::Path(QPath::Resolved(
//             _,
//             Path {
//                 res: Res::Def(_, def_id),
//                 ..
//             },
//         )) => Ok(*def_id),
//         _ => Err(SpanError::new(SpanErrorKind::MissingDefId, ty.span).into()),
//     }
// }

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
