use std::{
    fmt::{self, Display},
    io,
    path::PathBuf,
};

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    net_list::{TempNodeId, TyId},
    sig_ty::NodeTy,
};
use rustc_span::{symbol::Ident, Span};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot find 'top_module' function")]
    MissingTopModule,
    #[error("cannot find crate '{0}'")]
    MissingCrate(&'static str),
    #[error("cannot find metadata for crate '{0}'")]
    MissingMetadata(String),
    #[error("file '{path}' is not metadata for crate '{name}")]
    FileIsNotMetadata { path: PathBuf, name: String },
    #[error("incompatible version of metadata (expected: {expected}, found: {found})")]
    IncompatibleVersions { expected: u32, found: u32 },
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

    pub fn missing_item_id(ident: Ident) -> Self {
        Self::new(SpanErrorKind::MissingNodeForIdent(ident), ident.span)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SpanErrorKind {
    #[error("cannot find node type for '{0}'")]
    MissingNodeTy(String),
    #[error("cannot find node type for type id '{0}' during instantiating module for definition '{1}'")]
    MissingNodeTyForDef(TyId, String),
    #[error("expected node type for type '{0}' during instantiating module for definition '{1}'")]
    ExpectedNodeTypeForDef(String, String),
    #[error("cannot find node for ident '{0}'")]
    MissingNodeForIdent(Ident),
    #[error("cannot find module for '{0}")]
    MissingModule(String),
    #[error("cannot find template node '{0}'")]
    MissingTemplateNode(TempNodeId),

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
    #[error("expected Enum variant")]
    ExpectedEnumVariant,
    #[error("expected const value")]
    ExpectedConst,
    #[error("non-indexable expression")]
    NonIndexableExpr,
    #[error("expected type")]
    ExpectedTy,

    #[error("prim type {0:?} does not have values")]
    PrimTyWithoutValue(NodeTy),
    #[error("unexpected literal value for prim type {0:?}")]
    UnexpectedLitValue(NodeTy),
    #[error("unsupported conversion")]
    UnsupportedConversion,
    #[error("incompatible types ('{0}' and '{1}') in binary expression")]
    IncompatibleTypes(String, String),
    #[error("guards for match are not supported")]
    UnsupportedGuard,
    #[error("invalid pattern")]
    InvalidPattern,
    #[error("cannot extract generic args")]
    CannotExtractGenericArgs,

    #[error("not synthesizable generic parameter")]
    NotSynthGenParam,
    #[error("not synthesizable expression")]
    NotSynthExpr,
    #[error("not synthesizable call")]
    NotSynthCall,
    #[error("not synthesizable input parameter")]
    NotSynthInput,
    #[error("not synthesizable expression for blackbox '{0:?}'")]
    NotSynthBlackboxExpr(BlackboxKind),
    #[error("not synthesizable item")]
    NotSynthItem,
}
