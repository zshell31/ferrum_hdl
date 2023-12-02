use std::{
    fmt::{self, Display},
    io,
};

use ferrum_blackbox::Blackbox;
use ferrum_netlist::sig_ty::PrimTy;
use rustc_span::{symbol::Ident, Span};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot find 'top_module' function")]
    MissingTopModule,
    #[error("cannot find crate '{0}'")]
    MissingCrate(&'static str),
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

    #[error("expected call")]
    ExpectedCall,
    #[error("expected method call")]
    ExpectedMethodCall,
    #[error("expected closure")]
    ExpectedClosure,
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
    #[error("expected Array type")]
    ExpectedArray,
    #[error("expected Struct type")]
    ExpectedStructType,
    #[error("expected Enum type")]
    ExpectedEnumType,
    #[error("expected Enum variant")]
    ExpectedEnumVariant,
    #[error("expected const value")]
    ExpectedConst,
    #[error("non-indexable expression")]
    NonIndexableExpr,

    #[error("prim type {0:?} does not have values")]
    PrimTyWithoutValue(PrimTy),
    #[error("unexpected literal value for prim type {0:?}")]
    UnexpectedLitValue(PrimTy),
    #[error("unsupported conversion")]
    UnsupportedConversion,
    #[error("incompatible types ('{0}' and '{1}') in binary expression")]
    IncompatibleTypes(String, String),
    #[error("guards for match are not supported")]
    UnsupportedGuard,
    #[error("invalid pattern")]
    InvalidPattern,

    #[error("not synthesizable generic parameter")]
    NotSynthGenParam,
    #[error("not synthesizable expression")]
    NotSynthExpr,
    #[error("not synthesizable call")]
    NotSynthCall,
    #[error("not synthesizable input parameter")]
    NotSynthInput,
    #[error("not synthesizable expression for blackbox '{0:?}'")]
    NotSynthBlackboxExpr(Blackbox),
}
