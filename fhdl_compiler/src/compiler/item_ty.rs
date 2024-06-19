use std::{
    cmp,
    fmt::{self, Debug},
    iter,
    ops::{Deref, DerefMut},
};

use either::Either;
use ferrum_hdl::const_functions::{clog2, clog2_len};
use fhdl_common::BlackboxTy;
use fhdl_netlist::{node_ty::NodeTy, symbol::Symbol};
use rustc_data_structures::intern::Interned;
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{BasicBlockData, Rvalue},
    query::Key,
    ty::{
        AdtDef, AliasKind, ClosureArgs, EarlyBinder, FieldDef, GenericArg,
        GenericArgsRef, List, Mutability, ParamEnv, Ty, VariantDiscr,
    },
};
use rustc_span::Span;
use rustc_target::abi::VariantIdx;
use rustc_type_ir::{
    IntTy,
    TyKind::{self},
    UintTy,
};

use super::{
    domain::DomainId,
    func::def_path_eq,
    utils::{TreeIter, TreeNode},
    Compiler, Context,
};
use crate::error::{Error, SpanError, SpanErrorKind};

pub fn ty_def_id(ty: Ty<'_>) -> Option<DefId> {
    match ty.kind() {
        TyKind::Adt(adt, _) => Some(adt.did()),
        TyKind::FnDef(did, _) => Some(*did),
        _ => None,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Named<T> {
    pub inner: T,
    pub name: Symbol,
}

impl<T: Debug> Debug for Named<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?}", self.name, self.inner)
    }
}

impl<T> Named<T> {
    pub fn new(inner: T, name: Symbol) -> Self {
        Self { inner, name }
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayTy<'tcx> {
    ty: ItemTy<'tcx>,
    count: u128,
}

impl<'tcx> Debug for ArrayTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(iter::repeat(self.ty).take(self.count as usize))
            .finish()
    }
}

pub type ArrayTyIter<'tcx> = impl Iterator<Item = ItemTy<'tcx>>;

impl<'tcx> ArrayTy<'tcx> {
    fn new(ty: ItemTy<'tcx>, count: u128) -> Self {
        Self { ty, count }
    }

    #[inline]
    pub fn ty(&self) -> ItemTy<'tcx> {
        self.ty
    }

    #[inline]
    pub fn count(&self) -> u128 {
        self.count
    }

    pub fn tys(&self) -> ArrayTyIter<'tcx> {
        iter::repeat(self.ty).take(self.count as usize)
    }

    #[inline]
    pub fn width(&self) -> u128 {
        self.count * self.ty.width()
    }

    fn nodes(&self) -> usize {
        self.ty.nodes() * (self.count as usize)
    }

    fn is_synth(&self) -> bool {
        self.ty.is_synth()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructTy<'tcx> {
    tys: &'tcx [Named<ItemTy<'tcx>>],
}

impl<'tcx> Debug for StructTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("struct");
        for ty in self.tys {
            s.field(ty.name.as_str(), &ty.inner);
        }
        s.finish()
    }
}

pub type StructTyIter<'tcx> = impl Iterator<Item = ItemTy<'tcx>>;

impl<'tcx> StructTy<'tcx> {
    pub fn new(tys: &'tcx [Named<ItemTy<'tcx>>]) -> Self {
        Self { tys }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.tys.len()
    }

    pub fn names(&self) -> impl Iterator<Item = &str> + 'tcx {
        self.tys.iter().map(|ty| ty.name.as_str())
    }

    pub fn tys(&self) -> StructTyIter<'tcx> {
        self.tys.iter().map(|ty| ty.inner)
    }

    pub fn named_tys(&self) -> impl Iterator<Item = Named<ItemTy<'tcx>>> {
        self.tys.iter().copied()
    }

    pub fn by_idx(&self, idx: usize) -> ItemTy<'tcx> {
        self.tys[idx].inner
    }

    pub fn width(&self) -> u128 {
        self.tys.iter().map(|ty| ty.width()).sum()
    }

    pub fn nodes(&self) -> usize {
        self.tys.iter().map(|ty| ty.inner.nodes()).sum()
    }

    pub fn is_synth(&self) -> bool {
        self.tys.iter().all(|ty| ty.is_synth())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Variant<'tcx> {
    pub discr: u128,
    pub ty: Named<ItemTy<'tcx>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumTy<'tcx> {
    discr_ty: ItemTy<'tcx>,
    data_width: u128,
    discr: Option<&'tcx [u128]>,
    variants: &'tcx [Named<ItemTy<'tcx>>],
}

impl<'tcx> Debug for EnumTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("enum ")?;
        f.debug_set().entries(self.variants.iter()).finish()
    }
}

impl<'tcx> EnumTy<'tcx> {
    fn new(
        variants: &'tcx [Named<ItemTy<'tcx>>],
        discr: Option<&'tcx [u128]>,
        discr_ty: ItemTy<'tcx>,
    ) -> Self {
        if let Some(discr) = discr {
            assert_eq!(discr.len(), variants.len());
        }
        let data_width = variants
            .iter()
            .map(|variant| variant.width())
            .fold(0, cmp::max);

        Self {
            discr_ty,
            data_width,
            discr,
            variants,
        }
    }

    #[inline]
    pub fn discr_ty(&self) -> ItemTy<'tcx> {
        self.discr_ty
    }

    #[inline]
    pub fn data_width(&self) -> u128 {
        self.data_width
    }

    #[inline]
    pub fn discr_width(&self) -> u128 {
        self.discr_ty.width()
    }

    #[inline]
    pub fn width(&self) -> u128 {
        self.discr_width() + self.data_width
    }

    #[inline]
    pub fn by_idx(&self, idx: usize) -> Variant<'tcx> {
        let discr = match self.discr {
            Some(discr) => discr[idx],
            None => idx as u128,
        };
        let ty = self.variants[idx];

        Variant { discr, ty }
    }

    #[inline]
    pub fn by_variant_idx(&self, variant_idx: VariantIdx) -> Variant<'tcx> {
        self.by_idx(variant_idx.as_usize())
    }

    #[inline]
    pub fn discriminants(&self) -> impl Iterator<Item = Variant<'tcx>> + '_ {
        match self.discr {
            Some(discr) => Either::Left(discr.iter().enumerate().map(|(idx, discr)| {
                let ty = self.variants[idx];

                Variant { discr: *discr, ty }
            })),
            None => Either::Right((0 .. self.variants.len()).map(|idx| {
                let ty = self.variants[idx];

                Variant {
                    discr: idx as u128,
                    ty,
                }
            })),
        }
    }

    pub fn is_fieldless(&self) -> bool {
        self.variants.iter().all(|variant| variant.width() == 0)
    }

    pub fn nodes(&self) -> usize {
        1
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClosureTy<'tcx> {
    pub fn_did: DefId,
    pub fn_generics: GenericArgsRef<'tcx>,
    pub ty: StructTy<'tcx>,
}

impl<'tcx> Debug for ClosureTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("closure");
        for ty in self.ty.tys {
            s.field(ty.name.as_str(), &ty.inner);
        }
        s.finish()
    }
}

impl<'tcx> ClosureTy<'tcx> {
    pub fn new(
        fn_did: DefId,
        fn_generics: GenericArgsRef<'tcx>,
        ty: StructTy<'tcx>,
    ) -> Self {
        Self {
            fn_did,
            fn_generics,
            ty,
        }
    }

    pub fn nodes(&self) -> usize {
        self.ty.nodes()
    }

    pub fn is_synth(&self) -> bool {
        self.ty.is_synth()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegTy<'tcx> {
    pub dom_id: DomainId,
    pub ty: ItemTy<'tcx>,
}

impl<'tcx> Debug for RegTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "reg {:?}", self.ty)
    }
}

impl<'tcx> RegTy<'tcx> {
    pub fn width(&self) -> u128 {
        self.ty.width()
    }

    pub fn nodes(&self) -> usize {
        self.ty.nodes()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct OptionTy<'tcx> {
    pub ty: ItemTy<'tcx>,
}

impl<'tcx> Debug for OptionTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "option {:?}", self.ty)
    }
}

impl<'tcx> OptionTy<'tcx> {
    pub fn width(&self) -> u128 {
        self.ty.width()
    }

    pub fn nodes(&self) -> usize {
        self.ty.nodes()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemTyKind<'tcx> {
    Node(NodeTy),
    Array(ArrayTy<'tcx>),
    Struct(StructTy<'tcx>),
    Closure(ClosureTy<'tcx>),
    Enum(EnumTy<'tcx>),
    Reg(RegTy<'tcx>),
    Option(OptionTy<'tcx>),
    LoopGen,
}

impl<'tcx> Debug for ItemTyKind<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Node(ty) => Debug::fmt(ty, f),
            Self::Array(ty) => Debug::fmt(ty, f),
            Self::Struct(ty) => Debug::fmt(ty, f),
            Self::Closure(ty) => Debug::fmt(ty, f),
            Self::Enum(ty) => Debug::fmt(ty, f),
            Self::Reg(ty) => Debug::fmt(ty, f),
            Self::Option(ty) => Debug::fmt(ty, f),
            Self::LoopGen => f.write_str("LoopGen"),
        }
    }
}

impl<'tcx> ItemTyKind<'tcx> {
    pub fn node_ty(&self) -> NodeTy {
        match self {
            Self::Node(node_ty) => *node_ty,
            _ => panic!("expected node ty"),
        }
    }

    pub fn node_ty_opt(&self) -> Option<NodeTy> {
        match self {
            Self::Node(node_ty) => Some(*node_ty),
            _ => None,
        }
    }

    pub fn array_ty(&self) -> ArrayTy<'tcx> {
        match self {
            Self::Array(array_ty) => *array_ty,
            _ => panic!("expected array ty"),
        }
    }

    pub fn struct_ty(&self) -> StructTy<'tcx> {
        match self {
            Self::Struct(struct_ty) => *struct_ty,
            _ => panic!("expected struct ty"),
        }
    }

    pub fn closure_ty(&self) -> ClosureTy<'tcx> {
        match self {
            Self::Closure(closure_ty) => *closure_ty,
            _ => panic!("expected closure ty"),
        }
    }

    #[inline]
    pub fn is_closure_ty(&self) -> bool {
        matches!(self, Self::Closure(_))
    }

    pub fn enum_ty(&self) -> EnumTy<'tcx> {
        match self {
            Self::Enum(enum_ty) => *enum_ty,
            _ => panic!("expected enum ty"),
        }
    }

    pub fn reg_ty(&self) -> RegTy<'tcx> {
        match self {
            Self::Reg(reg_ty) => *reg_ty,
            _ => panic!("expected reg ty"),
        }
    }

    #[inline]
    pub fn is_enum_ty(&self) -> bool {
        matches!(self, Self::Enum(_))
    }

    pub fn width(&self) -> u128 {
        match self {
            Self::Node(ty) => ty.width(),
            Self::Array(ty) => ty.width(),
            Self::Struct(ty) => ty.width(),
            Self::Closure(ty) => ty.ty.width(),
            Self::Enum(ty) => ty.width(),
            Self::Reg(ty) => ty.width(),
            Self::Option(ty) => ty.width(),
            Self::LoopGen => 0,
        }
    }

    pub fn nodes(&self) -> usize {
        match self {
            Self::Node(_) => 1,
            Self::Array(ty) => ty.nodes(),
            Self::Struct(ty) => ty.nodes(),
            Self::Closure(ty) => ty.nodes(),
            Self::Enum(ty) => ty.nodes(),
            Self::Reg(ty) => ty.nodes(),
            Self::Option(ty) => ty.nodes(),
            Self::LoopGen => 0,
        }
    }

    pub fn is_synth(&self) -> bool {
        match self {
            Self::Node(_) => false,
            Self::Array(ty) => ty.is_synth(),
            Self::Struct(ty) => ty.is_synth(),
            Self::Closure(ty) => ty.is_synth(),
            Self::Enum(_) => false,
            Self::Reg(_) => true,
            _ => false,
        }
    }
}

impl<'tcx> From<NodeTy> for ItemTyKind<'tcx> {
    #[inline]
    fn from(node_ty: NodeTy) -> Self {
        Self::Node(node_ty)
    }
}

impl<'tcx> From<ArrayTy<'tcx>> for ItemTyKind<'tcx> {
    #[inline]
    fn from(array_ty: ArrayTy<'tcx>) -> Self {
        Self::Array(array_ty)
    }
}

impl<'tcx> From<StructTy<'tcx>> for ItemTyKind<'tcx> {
    #[inline]
    fn from(struct_ty: StructTy<'tcx>) -> Self {
        Self::Struct(struct_ty)
    }
}

impl<'tcx> From<EnumTy<'tcx>> for ItemTyKind<'tcx> {
    #[inline]
    fn from(enum_ty: EnumTy<'tcx>) -> Self {
        Self::Enum(enum_ty)
    }
}

#[derive(Debug, Clone)]
pub struct WithTypeInfo<'tcx, T> {
    internee: T,
    ty: Option<Ty<'tcx>>,
    width: u128,
    nodes: usize,
    is_synth: bool,
}

impl<'tcx, T> Deref for WithTypeInfo<'tcx, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.internee
    }
}

impl<'tcx> WithTypeInfo<'tcx, ItemTyKind<'tcx>> {
    pub fn new(internee: ItemTyKind<'tcx>, ty: Option<Ty<'tcx>>) -> Self {
        let width = internee.width();
        let nodes = internee.nodes();
        let is_state = internee.is_synth();
        Self {
            internee,
            ty,
            width,
            nodes,
            is_synth: is_state,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemTy<'tcx>(Interned<'tcx, WithTypeInfo<'tcx, ItemTyKind<'tcx>>>);

impl<'tcx> Debug for ItemTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.kind(), f)
    }
}

impl<'tcx> ItemTy<'tcx> {
    pub fn new(kind: &'tcx WithTypeInfo<'tcx, ItemTyKind<'tcx>>) -> Self {
        Self(Interned::new_unchecked(kind))
    }

    #[inline]
    pub fn kind(&self) -> &ItemTyKind<'tcx> {
        &(self.0)
    }

    #[inline]
    pub fn width(&self) -> u128 {
        self.0.width
    }

    pub fn rust_ty(&self) -> Option<Ty<'tcx>> {
        self.0.ty
    }

    #[allow(clippy::wrong_self_convention)]
    #[inline]
    pub fn to_bitvec(&self) -> NodeTy {
        match self.kind() {
            ItemTyKind::Node(node_ty) => *node_ty,
            ItemTyKind::Array(array_ty) if array_ty.count() == 1 => {
                array_ty.ty().to_bitvec()
            }
            ItemTyKind::Struct(struct_ty) if struct_ty.len() == 1 => {
                struct_ty.by_idx(0).to_bitvec()
            }
            _ => NodeTy::BitVec(self.width()),
        }
    }

    pub fn is_synth(&self) -> bool {
        self.0.is_synth
    }

    pub fn nodes(&self) -> usize {
        self.0.nodes
    }

    pub fn iter(&self) -> TreeIter<ItemTyIter<'tcx>> {
        TreeIter::new(*self, self.nodes())
    }

    pub fn node_ty(&self) -> NodeTy {
        self.0.node_ty()
    }

    pub fn node_ty_opt(&self) -> Option<NodeTy> {
        self.0.node_ty_opt()
    }

    pub fn array_ty(&self) -> ArrayTy<'tcx> {
        self.0.array_ty()
    }

    pub fn struct_ty(&self) -> StructTy<'tcx> {
        self.0.struct_ty()
    }

    pub fn closure_ty(&self) -> ClosureTy<'tcx> {
        self.0.closure_ty()
    }

    pub fn is_closure_ty(&self) -> bool {
        self.0.is_closure_ty()
    }

    pub fn enum_ty(&self) -> EnumTy<'tcx> {
        self.0.enum_ty()
    }

    pub fn is_enum_ty(&self) -> bool {
        self.0.is_enum_ty()
    }

    pub fn is_unsigned(&self) -> bool {
        match self.kind() {
            ItemTyKind::Node(node_ty) => node_ty.is_unsigned(),
            _ => false,
        }
    }
    pub fn is_signed(&self) -> bool {
        match self.kind() {
            ItemTyKind::Node(node_ty) => node_ty.is_signed(),
            _ => false,
        }
    }

    pub fn reg_ty(&self) -> RegTy<'tcx> {
        self.0.reg_ty()
    }
}

pub enum ItemTyIter<'tcx> {
    Empty,
    Node(Option<NodeTy>),
    Array(ArrayTyIter<'tcx>),
    Struct(StructTyIter<'tcx>),
}

impl<'tcx> Iterator for ItemTyIter<'tcx> {
    type Item = TreeNode<NodeTy, ItemTy<'tcx>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Node(node_ty) => node_ty.take().map(TreeNode::Leaf),
            Self::Array(array_ty) => array_ty.next().map(TreeNode::Node),
            Self::Struct(struct_ty) => struct_ty.next().map(TreeNode::Node),
        }
    }
}

impl<'tcx> IntoIterator for ItemTy<'tcx> {
    type Item = TreeNode<NodeTy, ItemTy<'tcx>>;
    type IntoIter = ItemTyIter<'tcx>;

    fn into_iter(self) -> Self::IntoIter {
        match self.kind() {
            ItemTyKind::Node(node_ty) => ItemTyIter::Node(Some(*node_ty)),
            ItemTyKind::Array(array_ty) => ItemTyIter::Array(array_ty.tys()),
            ItemTyKind::Struct(struct_ty) => ItemTyIter::Struct(struct_ty.tys()),
            ItemTyKind::Closure(closure_ty) => ItemTyIter::Struct(closure_ty.ty.tys()),
            ItemTyKind::Enum(enum_ty) => {
                ItemTyIter::Node(Some(NodeTy::BitVec(enum_ty.width())))
            }
            ItemTyKind::Reg(ty) => ty.ty.into_iter(),
            ItemTyKind::Option(ty) => ty.ty.into_iter(),
            ItemTyKind::LoopGen => ItemTyIter::Empty,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Generic<'tcx> {
    Ty(ItemTy<'tcx>),
    Const(u128),
}

impl<'tcx> Generic<'tcx> {
    pub fn ty(self) -> Option<ItemTy<'tcx>> {
        match self {
            Self::Ty(item_ty) => Some(item_ty),
            _ => None,
        }
    }

    pub fn cons(self) -> Option<u128> {
        match self {
            Self::Const(cons) => Some(cons),
            _ => None,
        }
    }
}

impl<'tcx> From<ItemTy<'tcx>> for Generic<'tcx> {
    #[inline]
    fn from(ty: ItemTy<'tcx>) -> Self {
        Self::Ty(ty)
    }
}

impl<'tcx> From<u128> for Generic<'tcx> {
    #[inline]
    fn from(value: u128) -> Self {
        Self::Const(value)
    }
}

impl<'tcx> Compiler<'tcx> {
    #[inline]
    pub fn alloc_ty(
        &mut self,
        ty: impl Into<ItemTyKind<'tcx>>,
        rust_ty: Option<Ty<'tcx>>,
    ) -> ItemTy<'tcx> {
        let ty = ty.into();

        // for resolving types like "Unsigned<UnevaluatedConst .. >"
        // and "Unsigned<16>" if they are actually the same
        #[allow(clippy::map_entry)]
        if !self.allocated_ty.contains_key(&ty) {
            self.allocated_ty
                .insert(ty, ItemTy::new(self.alloc(WithTypeInfo::new(ty, rust_ty))));
        }

        self.allocated_ty.get(&ty).copied().unwrap()
    }

    pub fn opt_ty(&mut self, item_ty: ItemTy<'tcx>) -> ItemTy<'tcx> {
        self.alloc_ty(ItemTyKind::Option(OptionTy { ty: item_ty }), None)
    }

    pub fn loop_gen_ty(&mut self) -> ItemTy<'tcx> {
        self.alloc_ty(ItemTyKind::LoopGen, None)
    }

    pub fn unsigned_ty(&mut self, width: u128) -> ItemTy<'tcx> {
        self.alloc_ty(ItemTyKind::Node(NodeTy::Unsigned(width)), None)
    }

    pub fn usize_ty(&mut self) -> ItemTy<'tcx> {
        self.unsigned_ty(usize::BITS as u128)
    }

    pub fn is_std_def(&self, ty: Ty<'tcx>, def_path_str: &[&'static str]) -> bool {
        match ty.ty_def_id() {
            Some(did) if self.crates.is_std(did) => {
                let def_path = self.tcx.def_path(did);

                def_path_eq(&def_path, def_path_str)
            }
            _ => false,
        }
    }

    pub fn resolve_fn_out_ty(
        &mut self,
        ty: Ty<'tcx>,
        span: Span,
    ) -> Result<ItemTy<'tcx>, Error> {
        self.resolve_ty(ty, List::empty(), span)
    }

    pub fn resolve_ty(
        &mut self,
        rust_ty: Ty<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<ItemTy<'tcx>, Error> {
        let rust_ty = EarlyBinder::bind(rust_ty).instantiate(self.tcx, generics);

        #[allow(clippy::map_entry)]
        if !self.item_ty.contains_key(&rust_ty) {
            let item_ty: Option<ItemTy<'_>> = match rust_ty.kind() {
                TyKind::Bool => Some(self.alloc_ty(NodeTy::Bit, Some(rust_ty))),
                TyKind::Uint(UintTy::U8) => {
                    Some(self.alloc_ty(NodeTy::Unsigned(8), Some(rust_ty)))
                }
                TyKind::Uint(UintTy::U16) => {
                    Some(self.alloc_ty(NodeTy::Unsigned(16), Some(rust_ty)))
                }
                TyKind::Uint(UintTy::U32) => {
                    Some(self.alloc_ty(NodeTy::Unsigned(32), Some(rust_ty)))
                }
                TyKind::Uint(UintTy::U64) => {
                    Some(self.alloc_ty(NodeTy::Unsigned(64), Some(rust_ty)))
                }
                TyKind::Uint(UintTy::U128) => {
                    Some(self.alloc_ty(NodeTy::Unsigned(128), Some(rust_ty)))
                }
                TyKind::Uint(UintTy::Usize) => Some(
                    self.alloc_ty(NodeTy::Unsigned(usize::BITS as u128), Some(rust_ty)),
                ),
                TyKind::Int(IntTy::I8) => {
                    Some(self.alloc_ty(NodeTy::Signed(8), Some(rust_ty)))
                }
                TyKind::Int(IntTy::I16) => {
                    Some(self.alloc_ty(NodeTy::Signed(16), Some(rust_ty)))
                }
                TyKind::Int(IntTy::I32) => {
                    Some(self.alloc_ty(NodeTy::Signed(32), Some(rust_ty)))
                }
                TyKind::Int(IntTy::I64) => {
                    Some(self.alloc_ty(NodeTy::Signed(64), Some(rust_ty)))
                }
                TyKind::Int(IntTy::I128) => {
                    Some(self.alloc_ty(NodeTy::Signed(128), Some(rust_ty)))
                }
                TyKind::Int(IntTy::Isize) => Some(
                    self.alloc_ty(NodeTy::Signed(isize::BITS as u128), Some(rust_ty)),
                ),
                TyKind::Tuple(ty) => {
                    let ty = self.resolve_tuple_ty(ty.iter(), generics, span)?;
                    Some(self.alloc_ty(ty, Some(rust_ty)))
                }
                TyKind::Adt(adt, adt_generics)
                    if !self.is_blackbox_ty(adt.did()) && adt.is_struct() =>
                {
                    let ty = self.resolve_struct_ty(adt, adt_generics, generics, span)?;
                    Some(self.alloc_ty(ty, Some(rust_ty)))
                }
                TyKind::Adt(adt, adt_generics)
                    if !self.is_blackbox_ty(adt.did()) && adt.is_enum() =>
                {
                    let ty = self.resolve_enum_ty(adt, adt_generics, generics, span)?;
                    Some(self.alloc_ty(ty, Some(rust_ty)))
                }
                TyKind::Alias(AliasKind::Projection, alias_ty) => {
                    let alias_ty = self
                        .tcx
                        .try_instantiate_and_normalize_erasing_regions(
                            EarlyBinder::bind(alias_ty.args)
                                .instantiate(self.tcx, generics),
                            ParamEnv::reveal_all(),
                            EarlyBinder::bind(rust_ty),
                        )
                        .map_err(|_| {
                            SpanError::new(
                                SpanErrorKind::NotSynthType(rust_ty.to_string()),
                                span,
                            )
                        })?;

                    Some(self.resolve_ty(alias_ty, generics, span)?)
                }
                TyKind::Ref(_, ty, Mutability::Not) => {
                    Some(self.resolve_ty(*ty, generics, span)?)
                }
                TyKind::FnDef(fn_did, fn_generics) => {
                    let (instance_did, instance) =
                        self.resolve_instance(*fn_did, fn_generics, span)?;

                    let struct_ty =
                        self.resolve_tuple_ty(iter::empty(), List::empty(), span)?;

                    Some(self.alloc_ty(
                        ItemTyKind::Closure(ClosureTy::new(
                            instance_did,
                            instance.args,
                            struct_ty,
                        )),
                        Some(rust_ty),
                    ))
                }
                TyKind::Closure(closure_did, closure_generics) => {
                    let closure_args = ClosureArgs {
                        args: closure_generics,
                    };
                    let struct_ty = self.resolve_tuple_ty(
                        closure_args.upvar_tys().iter(),
                        closure_generics,
                        span,
                    )?;

                    Some(self.alloc_ty(
                        ItemTyKind::Closure(ClosureTy::new(
                            *closure_did,
                            closure_generics,
                            struct_ty,
                        )),
                        Some(rust_ty),
                    ))
                }
                TyKind::Array(ty, const_) => {
                    let item_ty = self.resolve_ty(*ty, generics, span)?;
                    let const_ = self.eval_const(*const_, span)?;

                    let array_ty = ArrayTy::new(item_ty, const_);
                    Some(self.alloc_ty(array_ty, Some(rust_ty)))
                }
                _ => None,
            };

            let item_ty = match item_ty {
                Some(item_ty) => item_ty,
                None => self.find_item_ty(rust_ty, generics, span)?.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::NotSynthType(rust_ty.to_string()), span)
                })?,
            };

            self.item_ty.insert(rust_ty, item_ty);
        }

        self.item_ty
            .get(&rust_ty)
            .copied()
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::NotSynthType(rust_ty.to_string()), span)
            })
            .map_err(Into::into)
    }

    fn find_item_ty(
        &mut self,
        ty: Ty<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<ItemTy<'tcx>>, Error> {
        let def_id = match ty_def_id(ty) {
            Some(def_id) => def_id,
            None => {
                return Ok(None);
            }
        };

        if self.crates.is_ferrum_hdl(def_id) {
            let blackbox_ty = match self.find_blackbox_ty(def_id) {
                Some(blackbox_ty) => blackbox_ty,
                None => {
                    return Ok(None);
                }
            };

            let item_ty = match blackbox_ty {
                BlackboxTy::Signal => self.generic_ty(ty, 1, generics, span)?,
                BlackboxTy::Wrapped => self.generic_ty(ty, 1, generics, span)?,
                BlackboxTy::BitVec => {
                    self.generic_const(ty, 0, generics, span)?.map(|val| {
                        self.alloc_ty(ItemTyKind::Node(NodeTy::BitVec(val)), None)
                    })
                }
                BlackboxTy::Clock => {
                    Some(self.alloc_ty(ItemTyKind::Node(NodeTy::Clock), None))
                }
                BlackboxTy::Unsigned => {
                    self.generic_const(ty, 0, generics, span)?.map(|val| {
                        self.alloc_ty(ItemTyKind::Node(NodeTy::Unsigned(val)), None)
                    })
                }
                BlackboxTy::Signed => {
                    self.generic_const(ty, 0, generics, span)?.map(|val| {
                        self.alloc_ty(ItemTyKind::Node(NodeTy::Signed(val)), None)
                    })
                }
                BlackboxTy::UnsignedInner => None,
                BlackboxTy::Reg => {
                    let dom_ty = match ty.kind() {
                        TyKind::Adt(_, generics) => generics[1].expect_ty(),
                        _ => panic!("Reg is not ADT"),
                    };
                    let dom_id = self.find_domain_by_type(dom_ty, generics);

                    self.generic_ty(ty, 2, generics, span)?.map(|ty| {
                        self.alloc_ty(ItemTyKind::Reg(RegTy { dom_id, ty }), None)
                    })
                }
            };

            return Ok(item_ty);
        }

        Ok(None)
    }

    pub fn is_inner_ty(&self, ty: Ty<'tcx>) -> bool {
        let def_id = match ty_def_id(ty) {
            Some(def_id) => def_id,
            None => return false,
        };
        let blackbox_ty = match self.find_blackbox_ty(def_id) {
            Some(blackbox_ty) => blackbox_ty,
            None => return false,
        };

        matches!(blackbox_ty, BlackboxTy::UnsignedInner)
    }

    pub fn generic_ty(
        &mut self,
        ty: Ty<'tcx>,
        idx: usize,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<ItemTy<'tcx>>, Error> {
        self.generic(ty, idx, generics, span)
            .map(|generic| generic.and_then(Generic::ty))
    }

    pub fn generic_const(
        &mut self,
        ty: Ty<'tcx>,
        idx: usize,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<u128>, Error> {
        self.generic(ty, idx, generics, span)
            .map(|generic| generic.and_then(Generic::cons))
    }

    pub fn generic(
        &mut self,
        ty: Ty<'tcx>,
        idx: usize,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<Generic<'tcx>>, Error> {
        match ty.kind() {
            TyKind::Array(ty, _) if idx == 0 => self
                .resolve_ty(*ty, generics, span)
                .map(Into::into)
                .map(Some),
            TyKind::Array(_, const_) if idx == 1 => {
                self.eval_const(*const_, span).map(Into::into).map(Some)
            }
            TyKind::Adt(adt, generics) if !generics.is_empty() => {
                // TODO: check if blackbox_ty is ignored
                let blackbox_ty = self.find_blackbox_ty(adt.did());

                match blackbox_ty {
                    Some(BlackboxTy::Clock) => Ok(None),
                    Some(BlackboxTy::Signal | BlackboxTy::Wrapped) if idx == 0 => {
                        Ok(None)
                    }
                    _ => match generics.get(idx) {
                        Some(arg) => self.from_gen_arg(arg, span).map(Some),
                        None => Ok(None),
                    },
                }
            }
            TyKind::FnDef(def_id, generics) => {
                let fn_generics = &self.tcx.generics_of(def_id).params;
                match fn_generics
                    .get(idx)
                    .and_then(|gen| generics.get(gen.index as usize))
                {
                    Some(arg) => self.from_gen_arg(arg, span).map(Some),
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn from_gen_arg(
        &mut self,
        arg: &GenericArg<'tcx>,
        span: Span,
    ) -> Result<Generic<'tcx>, Error> {
        if let Some(ty) = arg.as_type() {
            let item_ty = self.resolve_ty(ty, List::empty(), span)?;

            return Ok(Generic::Ty(item_ty));
        }

        arg.as_const()
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span).into())
            .and_then(|const_| self.eval_const(const_, span))
            .map(Generic::Const)
    }

    fn resolve_tuple_ty(
        &mut self,
        tys: impl Iterator<Item = Ty<'tcx>>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<StructTy<'tcx>, Error> {
        let tys =
            self.alloc_from_iter_res_with_gen(tys.enumerate(), |compiler, (ind, ty)| {
                let item_ty = compiler.resolve_ty(ty, generics, span)?;

                Ok(Named::new(item_ty, Symbol::new_from_ind(ind)))
            })?;

        Ok(StructTy::new(tys))
    }

    pub fn alloc_tuple_ty(
        &mut self,
        tys: impl Iterator<Item = ItemTy<'tcx>>,
    ) -> ItemTy<'tcx> {
        let tys = self.alloc_from_iter(
            tys.enumerate()
                .map(|(ind, item_ty)| Named::new(item_ty, Symbol::new_from_ind(ind))),
        );

        self.alloc_ty(StructTy::new(tys), None)
    }

    fn resolve_struct_ty(
        &mut self,
        adt: &AdtDef<'tcx>,
        adt_generics: GenericArgsRef<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<StructTy<'tcx>, Error> {
        self.resolve_struct_ty_(adt.all_fields(), adt_generics, generics, span)
    }

    fn resolve_struct_ty_(
        &mut self,
        fields: impl IntoIterator<Item = &'tcx FieldDef>,
        adt_generics: GenericArgsRef<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<StructTy<'tcx>, Error> {
        let tcx = self.tcx;
        let fields = fields.into_iter().map(|field| {
            (
                Symbol::intern(field.ident(tcx).as_str()),
                field.ty(tcx, adt_generics),
            )
        });

        let fields =
            self.alloc_from_iter_opt_res_with_gen(fields, |compiler, (sym, ty)| {
                let ignore = match ty.kind() {
                    TyKind::Adt(adt, _) => compiler.ignore_ty(adt.did()),
                    _ => false,
                };
                if !ignore {
                    Some(
                        compiler
                            .resolve_ty(ty, generics, span)
                            .map(|item_ty| Named::new(item_ty, sym)),
                    )
                } else {
                    None
                }
            })?;

        Ok(StructTy::new(fields))
    }

    fn resolve_enum_ty(
        &mut self,
        adt: &AdtDef<'tcx>,
        adt_generics: GenericArgsRef<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<EnumTy<'tcx>, Error> {
        let mut discr_seq = true;
        let variants =
            self.alloc_from_iter_res_with_gen(adt.variants(), |compiler, variant| {
                if let VariantDiscr::Explicit(_) = variant.discr {
                    discr_seq = false;
                }
                let struct_ty = compiler.resolve_struct_ty_(
                    variant.fields.iter(),
                    adt_generics,
                    generics,
                    span,
                )?;
                let item_ty = compiler.alloc_ty(ItemTyKind::Struct(struct_ty), None);

                Ok(Named::new(item_ty, Symbol::intern(variant.name.as_str())))
            })?;

        let (discr_width, discr) = if discr_seq {
            (clog2_len(variants.len()) as u128, None)
        } else {
            let mut max_discr = 0;
            let discr =
                self.alloc_from_iter(adt.discriminants(self.tcx).map(|(_, discr)| {
                    if max_discr < discr.val {
                        max_discr = discr.val;
                    }

                    discr.val
                }));

            (clog2(max_discr as usize) as u128, Some(discr))
        };

        let discr_ty = self.alloc_ty(ItemTyKind::Node(NodeTy::BitVec(discr_width)), None);

        Ok(EnumTy::new(variants, discr, discr_ty))
    }

    fn ignore_ty(&self, def_id: DefId) -> bool {
        self.crates.is_std(def_id)
            && self
                .tcx
                .get_lang_items(())
                .phantom_data()
                .filter(|phantom_data| *phantom_data == def_id)
                .is_some()
    }

    pub fn discr_has_inner_ty(
        &self,
        bbs: &BasicBlockData<'tcx>,
        ctx: &Context<'tcx>,
    ) -> bool {
        use itertools::Itertools;

        let discr = match bbs
            .statements
            .iter()
            .filter_map(|stmt| {
                stmt.kind.as_assign().and_then(|(_, rvalue)| match rvalue {
                    Rvalue::Discriminant(discr) => Some(*discr),
                    _ => None,
                })
            })
            .exactly_one()
        {
            Ok(discr) => discr,
            _ => {
                return false;
            }
        };

        let discr_ty = discr.ty(ctx.mir, self.tcx).ty;
        self.is_inner_ty(discr_ty)
    }
}
