use crate::arena::with_arena;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimTy {
    Bool,
    Bit,
    U128,
    Unsigned(u8),
    BitVec(u128),
    Clock,
    ClockDomain,
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
            Self::BitVec(n) => *n,
            Self::Clock => 1,
            Self::ClockDomain => 1,
        }
    }
}

pub trait IsPrimTy {
    const PRIM_TY: PrimTy;
}

impl IsPrimTy for bool {
    const PRIM_TY: PrimTy = PrimTy::Bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignalTy {
    Prim(PrimTy),
    Array(u128, &'static SignalTy),
    Group(&'static [SignalTy]),
}

impl !Sync for SignalTy {}
impl !Send for SignalTy {}

impl From<PrimTy> for SignalTy {
    fn from(prim_ty: PrimTy) -> Self {
        Self::Prim(prim_ty)
    }
}

impl SignalTy {
    pub fn group(iter: impl IntoIterator<Item = SignalTy>) -> Self {
        Self::Group(unsafe { with_arena().alloc_from_iter(iter) })
    }

    pub fn array(n: u128, sig_ty: SignalTy) -> Self {
        Self::Array(n, unsafe { with_arena().alloc(sig_ty) })
    }

    pub fn prim_ty(&self) -> PrimTy {
        match self {
            Self::Prim(prim_ty) => *prim_ty,
            _ => panic!("expected prim type"),
        }
    }

    pub fn width(&self) -> u128 {
        match self {
            Self::Prim(prim_ty) => prim_ty.width(),
            Self::Array(n, ty) => n * ty.width(),
            Self::Group(ty) => ty.iter().map(|ty| ty.width()).sum(),
        }
    }
}
