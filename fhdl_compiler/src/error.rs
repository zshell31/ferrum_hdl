use std::{
    fmt::{self, Display},
    io,
};

use rustc_span::Span;

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

    #[error("duplicate constraint name '{0}'")]
    DuplicateConstrName(String),
    #[error("cannot find ident '{ident}' for constraint '{constr}'")]
    MissingIdent { constr: String, ident: String },
    #[error("ident '{ident}' for constraint '{constr}' is neither input or output")]
    NotInputOutputIdent { constr: String, ident: String },
    #[error("incorrect count of pins for identifier '{ident}' for constraint '{constr}': expected {expected}, got {actual}")]
    IncorrectPinsCount {
        constr: String,
        ident: String,
        expected: usize,
        actual: usize,
    },
    #[error("incorrect total count of pins for constraint '{constr}': expected {expected}, got {actual}")]
    IncorrectTotalPinsCount {
        constr: String,
        expected: usize,
        actual: usize,
    },
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
    #[error("unsupported conversion")]
    UnsupportedConversion,
    #[error("loops are unsupported")]
    UnsupportedLoops,
    #[error("invalid reset kind")]
    InvalidResetKind,
    #[error("invalid reset polarity")]
    InvalidResetPolarity,

    #[error("not synthesizable type '{0}'")]
    NotSynthType(String),
    #[error("not synthesizable generic parameter")]
    NotSynthGenParam,
    #[error("not synthesizable expression")]
    NotSynthExpr,
    #[error("not synthesizable call")]
    NotSynthCall,
    #[error("not synthesizable if-else/match expression")]
    NotSynthSwitch,
}
