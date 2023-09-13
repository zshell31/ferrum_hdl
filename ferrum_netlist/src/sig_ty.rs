use crate::{arena::with_arena, group_list::Named};

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
pub struct ArrayTy(pub u128, pub &'static SignalTy);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignalTy {
    Prim(PrimTy),
    Array(ArrayTy),
    Group(&'static [Named<SignalTy>]),
}

impl !Sync for SignalTy {}
impl !Send for SignalTy {}

impl From<PrimTy> for SignalTy {
    fn from(prim_ty: PrimTy) -> Self {
        Self::Prim(prim_ty)
    }
}

impl SignalTy {
    pub fn mk_group(iter: impl IntoIterator<Item = Named<SignalTy>>) -> Self {
        Self::Group(unsafe { with_arena().alloc_from_iter(iter) })
    }

    pub fn mk_array(n: u128, sig_ty: SignalTy) -> Self {
        Self::Array(ArrayTy(n, unsafe { with_arena().alloc(sig_ty) }))
    }

    pub fn opt_array_ty(&self) -> Option<ArrayTy> {
        match self {
            Self::Array(array_ty) => Some(*array_ty),
            _ => None,
        }
    }

    pub fn array_ty(&self) -> ArrayTy {
        self.opt_array_ty().expect("expected array type")
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
            Self::Array(ArrayTy(n, ty)) => n * ty.width(),
            Self::Group(ty) => ty.iter().map(|ty| ty.inner.width()).sum(),
        }
    }
}
