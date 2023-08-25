use std::{
    fmt::{self, Display},
    io,
};

use ferrum::prim_ty::PrimTy;
use rustc_hir::BinOpKind;
use rustc_span::{symbol::Ident, Span};

use crate::blackbox::Blackbox;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot find 'top_module' function")]
    MissingTopModule,
    #[error("{0}")]
    Span(SpanError),
    #[error("{0}")]
    Io(#[from] io::Error),
}

impl From<SpanError> for Error {
    fn from(span_error: SpanError) -> Self {
        Error::Span(span_error)
    }
}

#[derive(Debug)]
pub struct SpanError {
    pub kind: SpanErrorKind,
    pub span: Span,
}

impl Display for SpanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl SpanError {
    pub fn new(kind: SpanErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SpanErrorKind {
    #[error("cannot find blackbox for '{0}'")]
    MissingBlackbox(String),
    #[error("cannot find prim type for '{0}'")]
    MissingPrimTy(String),
    #[error("cannot find node for ident {0}")]
    MissingNodeForIdent(Ident),
    #[error("cannot define def_id")]
    MissingDefId,

    #[error("expected call")]
    ExpectedCall,
    #[error("expected method call")]
    ExpectedMethodCall,
    #[error("expected identifier")]
    ExpectedIdentifier,
    #[error("expected expression")]
    ExpectedExpr,
    #[error("expected expression at the end of the block")]
    ExpectedLastExpr,
    #[error("expected let binding")]
    ExpectedLetBind,
    #[error("expected if-else expression")]
    ExpectedIfElseExpr,

    #[error("prim type {0:?} does not have values")]
    PrimTyWithoutValue(PrimTy),
    #[error("unexpected literal value for prim type {0:?}")]
    UnexpectedLitValue(PrimTy),
    #[error("unsupported binary operation '{0:?}'")]
    UnsupportedBinOp(BinOpKind),
    #[error("unsupported conversion")]
    UnsupportedConversion,
    #[error("unsupported generic")]
    UnsupportedGeneric,

    #[error("not synthesizable expression")]
    NotSynthExpr,
    #[error("not synthesizable call")]
    NotSynthCall,
    #[error("not synthesizable input parameter")]
    NotSynthInput,
    #[error("not synthesizable expression for blackbox '{0:?}'")]
    NotSynthBlackboxExpr(Blackbox),
}
