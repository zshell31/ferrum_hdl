use fhdl_netlist::symbol::Symbol;

#[derive(Debug, Clone, Copy)]
pub enum SymIdent {
    Closure,
    Promoted,
    Part,
    EnumPart,
    Mux,
    Discr,
    Reg,
    Msb,
    Out,
    Bit,
    Cast,
}

impl SymIdent {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Closure => "closure",
            Self::Promoted => "promoted",
            Self::Part => "part",
            Self::EnumPart => "enum_part",
            Self::Mux => "mux",
            Self::Discr => "discr",
            Self::Reg => "reg",
            Self::Msb => "msb",
            Self::Out => "out",
            Self::Bit => "bit",
            Self::Cast => "cast",
        }
    }
}

impl From<SymIdent> for Symbol {
    #[inline]
    fn from(ident: SymIdent) -> Self {
        Symbol::intern(ident.as_str())
    }
}

impl From<SymIdent> for Option<Symbol> {
    #[inline]
    fn from(ident: SymIdent) -> Self {
        Some(ident.into())
    }
}
