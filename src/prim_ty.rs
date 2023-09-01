use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn width(&self) -> u128 {
        match self {
            Self::Bool => 1,
            Self::Bit => 1,
            Self::U128 => 128,
            Self::Unsigned(n) => *n as u128,
            Self::Clock => 1,
        }
    }
}

pub trait IsPrimTy {
    const PRIM_TY: PrimTy;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SignalTy {
    Prim(PrimTy),
    Array(u8, Rc<SignalTy>),
    Group(Rc<Vec<SignalTy>>),
}

impl From<PrimTy> for SignalTy {
    fn from(prim_ty: PrimTy) -> Self {
        Self::Prim(prim_ty)
    }
}

impl SignalTy {
    pub fn group(group: impl IntoIterator<Item = SignalTy>) -> Self {
        Self::Group(Rc::new(group.into_iter().collect()))
    }

    pub fn prim_ty(&self) -> PrimTy {
        match self {
            Self::Prim(prim_ty) => *prim_ty,
            _ => panic!("expected prim type"),
        }
    }
}
