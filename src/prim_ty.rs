#[derive(Debug, Clone, Copy)]
pub enum PrimTy {
    Bool,
    Bit,
    U128,
    Unsigned(u8),
    Clock,
}

impl PrimTy {
    pub fn is_bool(&self) -> bool {
        matches!(self, PrimTy::Bool)
    }

    pub fn width(&self) -> u8 {
        match self {
            Self::Bool => 1,
            Self::Bit => 1,
            Self::U128 => 128,
            Self::Unsigned(n) => *n,
            Self::Clock => 1,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DummyTy;
