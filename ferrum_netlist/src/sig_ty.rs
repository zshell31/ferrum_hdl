use std::iter;

use crate::{arena::with_arena, const_functions::clog2, group_list::Named};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimTy {
    Bool,
    Bit,
    U128,
    Unsigned(u128),
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
            Self::Unsigned(n) => *n,
            Self::BitVec(n) => *n,
            Self::Clock => 1,
            Self::ClockDomain => 1,
        }
    }

    pub fn ty_for_bin_expr(lhs: PrimTy, rhs: PrimTy) -> Option<PrimTy> {
        use PrimTy::*;

        if lhs == rhs {
            return Some(lhs);
        }

        match (lhs, rhs) {
            (Bool, Bit) | (Bit, Bool) => Some(Bit),
            (Unsigned(n), U128) | (U128, Unsigned(n)) => Some(Unsigned(n)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayTy(pub u128, pub &'static SignalTy);

impl ArrayTy {
    pub fn width(&self) -> u128 {
        self.0 * self.1.width()
    }

    pub fn item_width(&self) -> u128 {
        self.1.width()
    }

    pub fn tys(&self) -> impl Iterator<Item = SignalTy> + Clone {
        iter::repeat(*self.1).take(self.0 as usize)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructTy(&'static [Named<SignalTy>]);

#[allow(clippy::len_without_is_empty)]
impl StructTy {
    pub fn new(tys: &'static [Named<SignalTy>]) -> Self {
        Self(tys)
    }

    pub fn width(&self) -> u128 {
        self.0.iter().map(|ty| ty.inner.width()).sum()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn tys(&self) -> impl Iterator<Item = Named<SignalTy>> {
        self.0.iter().copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumTy(&'static [Named<SignalTy>]);

impl EnumTy {
    pub fn width(&self) -> u128 {
        let descr_width = self.descr_width();

        self.0.iter().map(|ty| ty.inner.width()).max().unwrap_or(0) + descr_width
    }

    pub fn descr_width(&self) -> u128 {
        clog2(self.0.len()) as u128
    }

    pub fn descr_ty(&self) -> PrimTy {
        PrimTy::BitVec(self.descr_width())
    }

    pub fn tys(&self) -> impl Iterator<Item = SignalTy> + Clone {
        self.0.iter().map(|ty| ty.inner)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignalTy {
    Prim(PrimTy),
    Array(ArrayTy),
    Struct(StructTy),
    // Enum(EnumTy),
}

impl !Sync for SignalTy {}
impl !Send for SignalTy {}

impl From<PrimTy> for SignalTy {
    fn from(prim_ty: PrimTy) -> Self {
        Self::Prim(prim_ty)
    }
}

impl SignalTy {
    pub fn mk_array(n: u128, sig_ty: SignalTy) -> Self {
        Self::Array(ArrayTy(n, unsafe { with_arena().alloc(sig_ty) }))
    }

    pub fn mk_struct(iter: impl IntoIterator<Item = Named<SignalTy>>) -> Self {
        Self::Struct(StructTy(unsafe { with_arena().alloc_from_iter(iter) }))
    }

    pub fn prim_ty(&self) -> PrimTy {
        match self {
            Self::Prim(prim_ty) => *prim_ty,
            _ => panic!("expected prim type"),
        }
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

    pub fn struct_ty(&self) -> StructTy {
        match self {
            Self::Struct(ty) => *ty,
            _ => panic!("expected struct type"),
        }
    }

    // pub fn enum_ty(&self) -> EnumTy {
    //     match self {
    //         Self::Enum(ty) => *ty,
    //         _ => panic!("expected enum type"),
    //     }
    // }

    pub fn width(&self) -> u128 {
        match self {
            Self::Prim(ty) => ty.width(),
            Self::Array(ty) => ty.width(),
            Self::Struct(ty) => ty.width(),
            // Self::Enum(ty) => ty.width(),
        }
    }

    pub fn maybe_to_bitvec(&self) -> PrimTy {
        match self {
            SignalTy::Prim(prim_ty) => *prim_ty,
            _ => {
                let width = self.width();
                PrimTy::BitVec(width)
            }
        }
    }
}
