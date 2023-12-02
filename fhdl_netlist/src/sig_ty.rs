use std::{
    borrow::Cow,
    cmp,
    fmt::{self, Debug, Display},
    ops::{Deref, DerefMut},
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

impl<T> Deref for Named<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Named<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct Width {
    pub kind: WidthKind,
    pub is_generic: bool,
}

impl Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Width {
    fn new(kind: WidthKind, is_generic: bool) -> Self {
        Self { kind, is_generic }
    }

    #[inline(always)]
    pub fn is_generic(&self) -> bool {
        self.is_generic
    }

    pub fn opt_value(&self) -> Option<u128> {
        match self.kind {
            WidthKind::Value(value) => Some(value),
            _ => None,
        }
    }
    pub fn value(self) -> u128 {
        self.opt_value().expect("expected value, got param")
    }

    pub fn is_value(self) -> bool {
        matches!(self.kind, WidthKind::Value(_))
    }

    pub fn mk_node(ty_idx: u32) -> Self {
        Width::new(WidthKind::Node(ty_idx), true)
    }

    pub fn mk_param(ty_idx: u32, param_idx: u32) -> Self {
        Width::new(WidthKind::Param { ty_idx, param_idx }, true)
    }

    fn mk_array(count: u128, ty: &SignalTy) -> Self {
        if ty.is_generic() {
            Width::new(
                WidthKind::Array {
                    count,
                    width: ArenaValue::make_value(ty.width()),
                },
                true,
            )
        } else {
            (ty.width().value() * count).into()
        }
    }

    fn mk_struct(tys: &[Named<SignalTy>]) -> Self {
        let is_generic = tys.iter().any(|ty| ty.is_generic());
        if is_generic {
            Width::new(
                WidthKind::Struct {
                    items: ArenaSlice::make_slice(tys.iter().map(|ty| ty.width())),
                },
                true,
            )
        } else {
            tys.iter().map(|ty| ty.width().value()).sum::<u128>().into()
        }
    }

    fn mk_enum_data(tys: &[Named<SignalTy>]) -> Self {
        let is_generic = tys.iter().any(|ty| ty.is_generic());
        if is_generic {
            Width::new(
                WidthKind::EnumData {
                    items: ArenaSlice::make_slice(tys.iter().map(|ty| ty.width())),
                },
                true,
            )
        } else {
            tys.iter()
                .fold(0, |max, ty| cmp::max(ty.width().value(), max))
                .into()
        }
    }

    fn mk_enum(discr_width: u128, width: Width) -> Self {
        if width.is_generic() {
            let items = match width.kind {
                WidthKind::EnumData { items } => items,
                _ => unreachable!(),
            };
            Width::new(WidthKind::Enum { discr_width, items }, true)
        } else {
            (discr_width + width.value()).into()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub enum WidthKind {
    Value(u128),
    Node(u32),
    Param {
        ty_idx: u32,
        param_idx: u32,
    },
    Array {
        count: u128,
        width: ArenaValue<Width>,
    },
    Struct {
        items: ArenaSlice<Width>,
    },
    EnumData {
        items: ArenaSlice<Width>,
    },
    Enum {
        discr_width: u128,
        items: ArenaSlice<Width>,
    },
}

impl Display for WidthKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Self::Value(val) = self {
            write!(f, "{val}")
        } else {
            write!(f, "{:?}", self)
        }
    }
}

impl From<u128> for WidthKind {
    fn from(value: u128) -> Self {
        Self::Value(value)
    }
}

impl From<u128> for Width {
    fn from(value: u128) -> Self {
        Width::new(value.into(), false)
    }
}

impl Width {}

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
    Unsigned(Width),
    BitVec(Width),
    Enum(EnumTy),
    Clock,
    ClockDomain,
    Ty(u32),
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
            Self::BitVec(n) => format!("bitvec[{n:#?}]").into(),
            Self::Enum(ty) => format!("enum[{:#?}]", ty.width()).into(),
            Self::Clock => "clock".into(),
            Self::ClockDomain => "clock_domain".into(),
            Self::Ty(idx) => format!("type({idx:#?})").into(),
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

    pub fn width(&self) -> Width {
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
            Self::Ty(idx) => Width::mk_node(*idx),
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

    pub fn is_generic(&self) -> bool {
        match self {
            Self::Bool
            | Self::Bit
            | Self::U8
            | Self::U16
            | Self::U32
            | Self::U64
            | Self::U128
            | Self::Usize
            | Self::Clock
            | Self::ClockDomain => true,
            Self::Unsigned(n) => n.is_generic(),
            Self::BitVec(n) => n.is_generic(),
            Self::Enum(enum_ty) => enum_ty.is_generic(),
            Self::Ty(_) => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct ArrayTy {
    count: u128,
    width: Width,
    ty: ArenaValue<SignalTy>,
}

impl ArrayTy {
    pub fn new(count: u128, ty: &'static SignalTy) -> Self {
        Self {
            count,
            width: Width::mk_array(count, ty),
            ty: ty.into(),
        }
    }

    #[inline(always)]
    pub fn width(&self) -> Width {
        self.width
    }

    #[inline(always)]
    pub fn count(&self) -> u128 {
        self.count
    }

    #[inline(always)]
    pub fn item_width(&self) -> Width {
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

    #[inline(always)]
    pub fn is_generic(&self) -> bool {
        self.width.is_generic()
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
    width: Width,
    tys: ArenaSlice<Named<SignalTy>>,
}

impl StructTy {
    pub fn new(tys: &'static [Named<SignalTy>]) -> Self {
        let width = Width::mk_struct(tys);
        Self {
            width,
            tys: tys.into(),
        }
    }

    #[inline(always)]
    pub fn width(&self) -> Width {
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

    #[inline(always)]
    pub fn is_generic(&self) -> bool {
        self.width.is_generic()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct EnumTy {
    discr_width: u128,
    data_width: Width,
    variants: ArenaSlice<Named<SignalTy>>,
}

impl EnumTy {
    pub fn new(tys: &'static [Named<SignalTy>]) -> Self {
        let discr_width = clog2_len(tys.len()) as u128;
        let data_width = Width::mk_enum_data(tys);
        Self {
            discr_width,
            data_width,
            variants: tys.into(),
        }
    }

    #[inline(always)]
    pub fn width(&self) -> Width {
        Width::mk_enum(self.discr_width, self.data_width)
    }

    #[inline(always)]
    pub fn discr_width(&self) -> u128 {
        self.discr_width
    }

    #[inline(always)]
    pub fn data_width(&self) -> Width {
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

    #[inline(always)]
    pub fn is_generic(&self) -> bool {
        self.data_width.is_generic()
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

    pub fn width(&self) -> Width {
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

    pub fn is_generic(&self) -> bool {
        match self.kind {
            SignalTyKind::Node(ty) => ty.is_generic(),
            SignalTyKind::Array(ty) => ty.is_generic(),
            SignalTyKind::Struct(ty) => ty.is_generic(),
            SignalTyKind::Enum(ty) => ty.is_generic(),
        }
    }
}
