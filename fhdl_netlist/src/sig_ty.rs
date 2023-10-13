use fhdl_blackbox::BlackboxTy;
use fhdl_const_func::clog2_len;

use crate::{arena::with_arena, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Named<T> {
    pub inner: T,
    pub name: Symbol,
}

impl<T> Named<T> {
    pub fn new(inner: T, name: Symbol) -> Self {
        Self { inner, name }
    }

    pub fn is(&self, s: &str) -> bool {
        self.name.as_str() == s
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimTy {
    Bool,
    Bit,
    U8,
    U16,
    U32,
    U64,
    U128,
    Unsigned(u128),
    BitVec(u128),
    Enum(EnumTy),
    Clock,
    ClockDomain,
}

impl PrimTy {
    pub fn is_bool(&self) -> bool {
        matches!(self, PrimTy::Bool)
    }

    pub fn is_unsigned(&self, n: u128) -> bool {
        matches!(self, PrimTy::Unsigned(_n) if *_n == n)
    }

    pub fn width(&self) -> u128 {
        match self {
            Self::Bool => 1,
            Self::Bit => 1,
            Self::U8 => 8,
            Self::U16 => 16,
            Self::U32 => 32,
            Self::U64 => 64,
            Self::U128 => 128,
            Self::Unsigned(n) => *n,
            Self::BitVec(n) => *n,
            Self::Enum(enum_ty) => enum_ty.width(),
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
            (Unsigned(n), U8 | U16 | U32 | U64 | U128)
            | (U8 | U16 | U32 | U64 | U128, Unsigned(n)) => Some(Unsigned(n)),
            _ => {
                println!("ty_for_bin_expr: lhs = {:?} rhs = {:?}", lhs, rhs);
                None
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayTy {
    count: u128,
    ty: &'static SignalTy,
}

impl ArrayTy {
    pub fn new(count: u128, ty: &'static SignalTy) -> Self {
        Self { count, ty }
    }

    #[inline(always)]
    pub fn width(&self) -> u128 {
        self.count * self.ty.width()
    }

    #[inline(always)]
    pub fn count(&self) -> u128 {
        self.count
    }

    #[inline(always)]
    pub fn item_width(&self) -> u128 {
        self.ty.width()
    }

    #[inline(always)]
    pub fn tys(&self) -> ArrayTyIter<'_> {
        ArrayTyIter { ind: 0, ty: self }
    }

    #[inline(always)]
    pub fn item_ty(&self) -> &'static SignalTy {
        self.ty
    }
}

// combines `repeat` + `take` but additionally implements size_hint
pub struct ArrayTyIter<'a> {
    ind: u128,
    ty: &'a ArrayTy,
}

impl<'a> Iterator for ArrayTyIter<'a> {
    type Item = SignalTy;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ind < self.ty.count {
            self.ind += 1;
            Some(*self.ty.ty)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.ty.count as usize;
        (len, Some(len))
    }
}

impl<'a> ExactSizeIterator for ArrayTyIter<'a> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructTy {
    width: u128,
    tys: &'static [Named<SignalTy>],
}

impl StructTy {
    pub fn new(tys: &'static [Named<SignalTy>]) -> Self {
        let width = tys.iter().map(|ty| ty.inner.width()).sum();
        Self { width, tys }
    }

    #[inline(always)]
    pub fn width(&self) -> u128 {
        self.width
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.tys.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.tys.is_empty()
    }

    #[inline(always)]
    pub fn tys(&self) -> &[Named<SignalTy>] {
        self.tys
    }

    #[inline(always)]
    pub fn by_field(&self, field: &str) -> Option<SignalTy> {
        self.tys
            .iter()
            .find(|named| named.is(field))
            .map(|named| named.inner)
    }

    pub(crate) fn get_idx_by_field(&self, field: &str) -> Option<usize> {
        self.tys
            .iter()
            .enumerate()
            .find(|(_, named)| named.is(field))
            .map(|(idx, _)| idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumTy {
    data_width: u128,
    discr_width: u128,
    variants: &'static [Named<SignalTy>],
}

fn data_width(variants: &'static [Named<SignalTy>]) -> u128 {
    variants
        .iter()
        .map(|ty| ty.inner.width())
        .max()
        .unwrap_or(0)
}

fn discr_width(variants: &'static [Named<SignalTy>]) -> u128 {
    clog2_len(variants.len()) as u128
}

impl EnumTy {
    pub fn new(variants: &'static [Named<SignalTy>]) -> Self {
        Self {
            data_width: data_width(variants),
            discr_width: discr_width(variants),
            variants,
        }
    }

    #[inline(always)]
    pub fn width(&self) -> u128 {
        self.discr_width() + self.data_width()
    }

    #[inline(always)]
    pub fn discr_width(&self) -> u128 {
        self.discr_width
    }

    #[inline(always)]
    pub fn data_width(&self) -> u128 {
        self.data_width
    }

    #[inline(always)]
    pub fn discr_val(&self, idx: usize) -> u128 {
        (idx as u128) & ((1 << self.discr_width()) - 1)
    }

    pub fn prim_ty(&self) -> PrimTy {
        PrimTy::Enum(*self)
    }

    pub fn variants(&self) -> &[Named<SignalTy>] {
        self.variants
    }

    pub fn variant(&self, idx: usize) -> Named<SignalTy> {
        self.variants[idx]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignalTyKind {
    Prim(PrimTy),
    Array(ArrayTy),
    Struct(StructTy),
    Enum(EnumTy),
}

impl From<PrimTy> for SignalTyKind {
    fn from(prim_ty: PrimTy) -> Self {
        match prim_ty {
            PrimTy::Enum(enum_ty) => enum_ty.into(),
            _ => Self::Prim(prim_ty),
        }
    }
}

impl From<ArrayTy> for SignalTyKind {
    fn from(array_ty: ArrayTy) -> Self {
        Self::Array(array_ty)
    }
}

impl From<StructTy> for SignalTyKind {
    fn from(struct_ty: StructTy) -> Self {
        Self::Struct(struct_ty)
    }
}

impl From<EnumTy> for SignalTyKind {
    fn from(enum_ty: EnumTy) -> Self {
        Self::Enum(enum_ty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SignalTy {
    pub blackbox: Option<BlackboxTy>,
    pub kind: SignalTyKind,
}

impl !Sync for SignalTy {}
impl !Send for SignalTy {}

impl SignalTy {
    pub fn new(blackbox: Option<BlackboxTy>, kind: SignalTyKind) -> Self {
        Self { blackbox, kind }
    }
    pub fn mk_array(blackbox: Option<BlackboxTy>, n: u128, sig_ty: SignalTy) -> Self {
        Self::new(
            blackbox,
            SignalTyKind::Array(ArrayTy::new(n, unsafe { with_arena().alloc(sig_ty) })),
        )
    }

    pub fn mk_struct(
        blackbox: Option<BlackboxTy>,
        iter: impl IntoIterator<Item = Named<SignalTy>>,
    ) -> Self {
        Self::new(
            blackbox,
            SignalTyKind::Struct(StructTy::new(unsafe {
                with_arena().alloc_from_iter(iter)
            })),
        )
    }

    pub fn opt_prim_ty(&self) -> Option<PrimTy> {
        match self.kind {
            SignalTyKind::Prim(prim_ty) => Some(prim_ty),
            _ => None,
        }
    }

    pub fn prim_ty(&self) -> PrimTy {
        self.opt_prim_ty().expect("expected Prim type")
    }

    pub fn is_unsigned(&self, n: u128) -> bool {
        self.opt_prim_ty()
            .map(|prim| prim.is_unsigned(n))
            .unwrap_or_default()
    }

    pub fn opt_array_ty(&self) -> Option<ArrayTy> {
        match self.kind {
            SignalTyKind::Array(array_ty) => Some(array_ty),
            _ => None,
        }
    }

    pub fn array_ty(&self) -> ArrayTy {
        self.opt_array_ty().expect("expected Array type")
    }

    pub fn opt_struct_ty(&self) -> Option<StructTy> {
        match self.kind {
            SignalTyKind::Struct(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn struct_ty(&self) -> StructTy {
        self.opt_struct_ty().expect("expected Struct type")
    }

    pub fn opt_enum_ty(&self) -> Option<EnumTy> {
        match self.kind {
            SignalTyKind::Enum(enum_ty) => Some(enum_ty),
            _ => None,
        }
    }

    pub fn enum_ty(&self) -> EnumTy {
        self.opt_enum_ty().expect("expected Enum type")
    }

    pub fn is_enum_ty(&self) -> bool {
        matches!(self.kind, SignalTyKind::Enum(_))
    }

    pub fn width(&self) -> u128 {
        match self.kind {
            SignalTyKind::Prim(ty) => ty.width(),
            SignalTyKind::Array(ty) => ty.width(),
            SignalTyKind::Struct(ty) => ty.width(),
            SignalTyKind::Enum(ty) => ty.width(),
        }
    }

    pub fn maybe_to_bitvec(&self) -> PrimTy {
        match self.kind {
            SignalTyKind::Prim(prim_ty) => prim_ty,
            _ => {
                let width = self.width();
                PrimTy::BitVec(width)
            }
        }
    }

    pub fn is_unsigned_short(&self) -> bool {
        self.blackbox
            .map(|blackbox| blackbox.is_unsigned_short())
            .unwrap_or_default()
    }
}
