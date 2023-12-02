use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
    iter::Sum,
};

use fhdl_blackbox::BlackboxTy;
use fhdl_const_func::clog2_len;
use rustc_macros::{Decodable, Encodable};

use crate::{
    arena::{with_arena, ArenaSlice, ArenaValue},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub enum Param<T> {
    Value(T),
    Generic,
}

impl<T: Display> Display for Param<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(val) => val.fmt(f),
            Self::Generic => write!(f, "Generic"),
        }
    }
}

impl<T> From<T> for Param<T> {
    fn from(value: T) -> Self {
        Self::Value(value)
    }
}

impl<T> Param<T> {
    pub fn opt_value(self) -> Option<T> {
        match self {
            Self::Value(value) => Some(value),
            _ => None,
        }
    }
    pub fn value(self) -> T {
        self.opt_value().expect("expected value, got param")
    }

    pub fn is_value(self) -> bool {
        matches!(self, Self::Value(_))
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Param<U> {
        match self {
            Self::Value(val) => Param::Value(f(val)),
            Self::Generic => Param::Generic,
        }
    }
}

pub type ConstParam = Param<u128>;

impl Sum for ConstParam {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut sum = 0;
        for item in iter {
            match item.opt_value() {
                Some(val) => {
                    sum += val;
                }
                None => return Self::Generic,
            }
        }

        Self::Value(sum)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub enum NodeTy {
    Bool,
    Bit,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Unsigned(ConstParam),
    BitVec(ConstParam),
    Enum(EnumTy),
    Clock,
    ClockDomain,
    Unknown,
}

impl Debug for NodeTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Cow<'_, str> = match self {
            Self::Bool => "bool".into(),
            Self::Bit => "Bit".into(),
            Self::U8 => "u8".into(),
            Self::U16 => "u16".into(),
            Self::U32 => "u32".into(),
            Self::U64 => "u64".into(),
            Self::U128 => "u128".into(),
            Self::Usize => "usize".into(),
            Self::Unsigned(n) => format!("unsigned[{n}]").into(),
            Self::BitVec(n) => format!("bitvec[{n}]").into(),
            Self::Enum(ty) => format!("enum[{}]", ty.width()).into(),
            Self::Clock => "clock".into(),
            Self::ClockDomain => "clock_domain".into(),
            Self::Unknown => "Unknown".into(),
        };

        f.write_str(s.as_ref())
    }
}

impl NodeTy {
    pub fn is_bool(&self) -> bool {
        matches!(self, NodeTy::Bool)
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::U8
                | Self::U16
                | Self::U32
                | Self::U64
                | Self::U128
                | Self::Usize
                | Self::Unsigned(_)
        )
    }

    pub fn width(&self) -> ConstParam {
        match self {
            Self::Bool => 1.into(),
            Self::Bit => 1.into(),
            Self::U8 => 8.into(),
            Self::U16 => 16.into(),
            Self::U32 => 32.into(),
            Self::U64 => 64.into(),
            Self::U128 => 128.into(),
            Self::Usize => (usize::BITS as u128).into(),
            Self::Unsigned(n) => *n,
            Self::BitVec(n) => *n,
            Self::Enum(enum_ty) => enum_ty.width(),
            Self::Clock => 1.into(),
            Self::ClockDomain => 1.into(),
            Self::Unknown => ConstParam::Generic,
        }
    }

    pub fn ty_for_bin_expr(lhs: NodeTy, rhs: NodeTy) -> Option<NodeTy> {
        use NodeTy::*;

        if lhs == rhs {
            return Some(lhs);
        }

        match (lhs, rhs) {
            (Bool, Bit) | (Bit, Bool) => Some(Bit),
            (Unsigned(n), U8 | U16 | U32 | U64 | U128 | Usize)
            | (U8 | U16 | U32 | U64 | U128 | Usize, Unsigned(n)) => Some(Unsigned(n)),
            _ => {
                println!("ty_for_bin_expr: lhs = {:?} rhs = {:?}", lhs, rhs);
                None
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct ArrayTy {
    count: u128,
    ty: ArenaValue<SignalTy>,
}

impl ArrayTy {
    pub fn new(count: u128, ty: &'static SignalTy) -> Self {
        Self {
            count,
            ty: ty.into(),
        }
    }

    #[inline(always)]
    pub fn width(&self) -> ConstParam {
        self.ty.width().map(|width| self.count * width)
    }

    #[inline(always)]
    pub fn count(&self) -> u128 {
        self.count
    }

    #[inline(always)]
    pub fn item_width(&self) -> ConstParam {
        self.ty.width()
    }

    #[inline(always)]
    pub fn tys(&self) -> ArrayTyIter<'_> {
        ArrayTyIter { ind: 0, ty: self }
    }

    #[inline(always)]
    pub fn item_ty(&self) -> &SignalTy {
        self.ty.0
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
            Some(*self.ty.ty.0)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct StructTy {
    width: ConstParam,
    tys: ArenaSlice<Named<SignalTy>>,
}

impl StructTy {
    pub fn new(tys: &'static [Named<SignalTy>]) -> Self {
        let width = tys.iter().map(|ty| ty.inner.width()).sum();
        Self {
            width,
            tys: tys.into(),
        }
    }

    #[inline(always)]
    pub fn width(&self) -> ConstParam {
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
        self.tys.0
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct EnumTy {
    data_width: ConstParam,
    discr_width: u128,
    variants: ArenaSlice<Named<SignalTy>>,
}

// impl<D: Decoder> Decodable<D> for

fn data_width(variants: &'static [Named<SignalTy>]) -> ConstParam {
    let mut max = 0;
    for variant in variants {
        match variant.inner.width().opt_value() {
            Some(width) => {
                if width > max {
                    max = width;
                }
            }
            None => {
                return ConstParam::Generic;
            }
        }
    }

    ConstParam::Value(max)
}

fn discr_width(variants: &'static [Named<SignalTy>]) -> u128 {
    clog2_len(variants.len()) as u128
}

impl EnumTy {
    pub fn new(variants: &'static [Named<SignalTy>]) -> Self {
        Self {
            data_width: data_width(variants),
            discr_width: discr_width(variants),
            variants: variants.into(),
        }
    }

    #[inline(always)]
    pub fn width(&self) -> ConstParam {
        self.data_width()
            .map(|data_width| self.discr_width() + data_width)
    }

    #[inline(always)]
    pub fn discr_width(&self) -> u128 {
        self.discr_width
    }

    #[inline(always)]
    pub fn data_width(&self) -> ConstParam {
        self.data_width
    }

    #[inline(always)]
    pub fn discr_val(&self, idx: usize) -> u128 {
        (idx as u128) & ((1 << self.discr_width()) - 1)
    }

    pub fn prim_ty(&self) -> NodeTy {
        NodeTy::Enum(*self)
    }

    pub fn variants(&self) -> &[Named<SignalTy>] {
        self.variants.0
    }

    pub fn variant(&self, idx: usize) -> Named<SignalTy> {
        self.variants[idx]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub enum SignalTyKind {
    Node(NodeTy),
    Array(ArrayTy),
    Struct(StructTy),
    Enum(EnumTy),
}

impl From<NodeTy> for SignalTyKind {
    fn from(prim_ty: NodeTy) -> Self {
        match prim_ty {
            NodeTy::Enum(enum_ty) => enum_ty.into(),
            _ => Self::Node(prim_ty),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
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

    pub fn opt_prim_ty(&self) -> Option<NodeTy> {
        match self.kind {
            SignalTyKind::Node(prim_ty) => Some(prim_ty),
            _ => None,
        }
    }

    pub fn node_ty(&self) -> NodeTy {
        self.opt_prim_ty().expect("expected Prim type")
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

    pub fn width(&self) -> ConstParam {
        match self.kind {
            SignalTyKind::Node(ty) => ty.width(),
            SignalTyKind::Array(ty) => ty.width(),
            SignalTyKind::Struct(ty) => ty.width(),
            SignalTyKind::Enum(ty) => ty.width(),
        }
    }

    pub fn to_bitvec(&self) -> NodeTy {
        match self.kind {
            SignalTyKind::Node(prim_ty) => prim_ty,
            _ => {
                let width = self.width();
                NodeTy::BitVec(width)
            }
        }
    }

    pub fn is_unsigned_short(&self) -> bool {
        self.blackbox
            .map(|blackbox| blackbox.is_unsigned_short())
            .unwrap_or_default()
    }
}
