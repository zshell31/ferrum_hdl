use fhdl_netlist::symbol::Symbol;

#[derive(Debug, Clone, Copy)]
pub enum SymIdent {
    Closure,
    Promoted,
    Part,
    Mux,
    Dff,
    DffEn,
    Msb,
    Out,
    Bit,
}

impl SymIdent {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Closure => "closure",
            Self::Promoted => "promoted",
            Self::Part => "part",
            Self::Mux => "mux",
            Self::Dff => "dff",
            Self::DffEn => "dff_en",
            Self::Msb => "msb",
            Self::Out => "out",
            Self::Bit => "bit",
        }
    }
}

impl From<SymIdent> for Symbol {
    #[inline]
    fn from(ident: SymIdent) -> Self {
        Symbol::new(ident.as_str())
    }
}

impl From<SymIdent> for Option<Symbol> {
    #[inline]
    fn from(ident: SymIdent) -> Self {
        Some(ident.into())
    }
}
