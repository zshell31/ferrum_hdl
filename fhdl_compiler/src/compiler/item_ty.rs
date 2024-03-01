use std::{
    cmp,
    fmt::{self, Display},
    iter,
    ops::{Deref, DerefMut},
};

use ferrum_hdl::const_functions::{clog2, clog2_len};
use fhdl_common::BlackboxTy;
use fhdl_netlist::{node_ty::NodeTy, symbol::Symbol};
use rustc_data_structures::intern::Interned;
use rustc_hir::def_id::DefId;
use rustc_middle::ty::{
    AdtDef, AliasKind, ClosureArgs, EarlyBinder, FieldDef, GenericArg, GenericArgsRef,
    List, Mutability, ParamEnv, Ty, VariantDiscr,
};
use rustc_span::Span;
use rustc_target::abi::VariantIdx;
use rustc_type_ir::{
    TyKind::{self},
    UintTy,
};

use super::Compiler;
use crate::error::{Error, SpanError, SpanErrorKind};

pub fn ty_def_id(ty: Ty<'_>) -> Option<DefId> {
    match ty.kind() {
        TyKind::Adt(adt, _) => Some(adt.did()),
        TyKind::FnDef(did, _) => Some(*did),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Named<T> {
    pub inner: T,
    pub name: Symbol,
}

impl<T: Display> Display for Named<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.inner, self.name)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayTy<'tcx> {
    ty: ItemTy<'tcx>,
    count: u128,
}

impl<'tcx> Display for ArrayTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[ {}; {} ]", self.ty, self.count)
    }
}

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

    pub fn tys(&self) -> impl Iterator<Item = ItemTy<'tcx>> {
        iter::repeat(self.ty).take(self.count as usize)
    }

    #[inline]
    pub fn width(&self) -> u128 {
        self.count * self.ty.width()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructTy<'tcx> {
    tys: &'tcx [Named<ItemTy<'tcx>>],
}

impl<'tcx> Display for StructTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.tys
                .iter()
                .map(|ty| ty.to_string())
                .intersperse(", ".to_string())
                .collect::<String>()
        )
    }
}

impl<'tcx> StructTy<'tcx> {
    fn new(tys: &'tcx [Named<ItemTy<'tcx>>]) -> Self {
        Self { tys }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.tys.len()
    }

    pub fn names(&self) -> impl Iterator<Item = &str> + 'tcx {
        self.tys.iter().map(|ty| ty.name.as_str())
    }

    pub fn tys(&self) -> impl Iterator<Item = ItemTy<'tcx>> {
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
}

#[derive(Debug, Clone, Copy)]
pub struct Variant<'tcx> {
    pub discr: u128,
    pub ty: Named<ItemTy<'tcx>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumTy<'tcx> {
    discr_ty: ItemTy<'tcx>,
    data_width: u128,
    discr: Option<&'tcx [u128]>,
    variants: &'tcx [Named<ItemTy<'tcx>>],
}

impl<'tcx> Display for EnumTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "enum {{ {} }}",
            self.variants
                .iter()
                .map(|variant| variant.to_string())
                .intersperse(", ".to_string())
                .collect::<String>()
        )
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClosureTy<'tcx> {
    pub fn_did: DefId,
    pub fn_generics: GenericArgsRef<'tcx>,
    pub ty: StructTy<'tcx>,
}

impl<'tcx> Display for ClosureTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "closure {}", self.ty)
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemTyKind<'tcx> {
    Node(NodeTy),
    Array(ArrayTy<'tcx>),
    Struct(StructTy<'tcx>),
    Closure(ClosureTy<'tcx>),
    Enum(EnumTy<'tcx>),
}

impl<'tcx> Display for ItemTyKind<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Node(ty) => Display::fmt(ty, f),
            Self::Array(ty) => Display::fmt(ty, f),
            Self::Struct(ty) => Display::fmt(ty, f),
            Self::Closure(ty) => Display::fmt(ty, f),
            Self::Enum(ty) => Display::fmt(ty, f),
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

    pub fn is_closure_ty(&self) -> bool {
        matches!(self, Self::Closure(_))
    }

    pub fn enum_ty(&self) -> EnumTy<'tcx> {
        match self {
            Self::Enum(enum_ty) => *enum_ty,
            _ => panic!("expected enum ty"),
        }
    }

    pub fn width(&self) -> u128 {
        match self {
            Self::Node(ty) => ty.width(),
            Self::Array(ty) => ty.width(),
            Self::Struct(ty) => ty.width(),
            Self::Closure(ty) => ty.ty.width(),
            Self::Enum(ty) => ty.width(),
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
pub struct WithTypeInfo<T> {
    internee: T,
    width: u128,
}

impl<T> Deref for WithTypeInfo<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.internee
    }
}

impl<'tcx> WithTypeInfo<ItemTyKind<'tcx>> {
    pub fn new(internee: ItemTyKind<'tcx>) -> Self {
        let width = internee.width();
        Self { internee, width }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemTy<'tcx>(Interned<'tcx, WithTypeInfo<ItemTyKind<'tcx>>>);

impl<'tcx> Display for ItemTy<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.kind(), f)
    }
}

impl<'tcx> ItemTy<'tcx> {
    pub fn new(kind: &'tcx WithTypeInfo<ItemTyKind<'tcx>>) -> Self {
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

    #[inline]
    pub fn node_ty(&self) -> NodeTy {
        self.0.node_ty()
    }

    #[inline]
    pub fn array_ty(&self) -> ArrayTy<'tcx> {
        self.0.array_ty()
    }

    #[inline]
    pub fn struct_ty(&self) -> StructTy<'tcx> {
        self.0.struct_ty()
    }

    #[inline]
    pub fn closure_ty(&self) -> ClosureTy<'tcx> {
        self.0.closure_ty()
    }

    #[inline]
    pub fn is_closure_ty(&self) -> bool {
        self.0.is_closure_ty()
    }

    #[inline]
    pub fn enum_ty(&self) -> EnumTy<'tcx> {
        self.0.enum_ty()
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
    pub fn alloc_ty(&mut self, ty: impl Into<ItemTyKind<'tcx>>) -> ItemTy<'tcx> {
        let ty = ty.into();

        // for resolving types like "Unsigned<UnevaluatedConst .. >"
        // and "Unsigned<16>" if they are actually the same
        #[allow(clippy::map_entry)]
        if !self.allocated_ty.contains_key(&ty) {
            self.allocated_ty
                .insert(ty, ItemTy::new(self.alloc(WithTypeInfo::new(ty))));
        }

        self.allocated_ty.get(&ty).copied().unwrap()
    }

    pub fn resolve_ty(
        &mut self,
        ty: Ty<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<ItemTy<'tcx>, Error> {
        let ty = EarlyBinder::bind(ty).instantiate(self.tcx, generics);

        #[allow(clippy::map_entry)]
        if !self.item_ty.contains_key(&ty) {
            let item_ty: Option<ItemTy<'_>> = match ty.kind() {
                TyKind::Bool => Some(self.alloc_ty(NodeTy::Bit)),
                TyKind::Uint(UintTy::U8) => Some(self.alloc_ty(NodeTy::Unsigned(8))),
                TyKind::Uint(UintTy::U16) => Some(self.alloc_ty(NodeTy::Unsigned(16))),
                TyKind::Uint(UintTy::U32) => Some(self.alloc_ty(NodeTy::Unsigned(32))),
                TyKind::Uint(UintTy::U64) => Some(self.alloc_ty(NodeTy::Unsigned(64))),
                TyKind::Uint(UintTy::U128) => Some(self.alloc_ty(NodeTy::Unsigned(128))),
                TyKind::Uint(UintTy::Usize) => {
                    Some(self.alloc_ty(NodeTy::Unsigned(usize::BITS as u128)))
                }
                TyKind::Tuple(ty) => {
                    let ty = self.resolve_tuple_ty(ty.iter(), generics, span)?;
                    Some(self.alloc_ty(ty))
                }
                TyKind::Adt(adt, adt_generics)
                    if !self.is_blackbox_ty(adt.did()) && adt.is_struct() =>
                {
                    let ty = self.resolve_struct_ty(adt, adt_generics, generics, span)?;
                    Some(self.alloc_ty(ty))
                }
                TyKind::Adt(adt, adt_generics)
                    if !self.is_blackbox_ty(adt.did()) && adt.is_enum() =>
                {
                    let ty = self.resolve_enum_ty(adt, adt_generics, generics, span)?;
                    Some(self.alloc_ty(ty))
                }
                TyKind::Alias(AliasKind::Projection, alias_ty) => {
                    let alias_ty = self
                        .tcx
                        .try_instantiate_and_normalize_erasing_regions(
                            EarlyBinder::bind(alias_ty.args)
                                .instantiate(self.tcx, generics),
                            ParamEnv::reveal_all(),
                            EarlyBinder::bind(ty),
                        )
                        .map_err(|_| {
                            SpanError::new(
                                SpanErrorKind::NotSynthType(ty.to_string()),
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

                    Some(self.alloc_ty(ItemTyKind::Closure(ClosureTy::new(
                        instance_did,
                        instance.args,
                        struct_ty,
                    ))))
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

                    Some(self.alloc_ty(ItemTyKind::Closure(ClosureTy::new(
                        *closure_did,
                        closure_generics,
                        struct_ty,
                    ))))
                }
                TyKind::Array(ty, const_) => {
                    let item_ty = self.resolve_ty(*ty, generics, span)?;
                    let const_ = self.eval_const(*const_, span)?;

                    let array_ty = ArrayTy::new(item_ty, const_);
                    Some(self.alloc_ty(array_ty))
                }
                _ => None,
            };

            let item_ty = match item_ty {
                Some(item_ty) => item_ty,
                None => self.find_item_ty(ty, generics, span)?.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::NotSynthType(ty.to_string()), span)
                })?,
            };

            self.item_ty.insert(ty, item_ty);
        }

        self.item_ty
            .get(&ty)
            .copied()
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::NotSynthType(ty.to_string()), span)
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
                BlackboxTy::BitVec => self
                    .generic_const(ty, 0, generics, span)?
                    .map(|val| self.alloc_ty(ItemTyKind::Node(NodeTy::BitVec(val)))),
                BlackboxTy::Clock => Some(self.alloc_ty(ItemTyKind::Node(NodeTy::Clock))),
                BlackboxTy::Unsigned => self
                    .generic_const(ty, 0, generics, span)?
                    .map(|val| self.alloc_ty(ItemTyKind::Node(NodeTy::Unsigned(val)))),
            };

            return Ok(item_ty);
        }

        Ok(None)
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
                Symbol::new(field.ident(tcx).as_str()),
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
                let item_ty = compiler.alloc_ty(ItemTyKind::Struct(struct_ty));

                Ok(Named::new(item_ty, Symbol::new(variant.name.as_str())))
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

        let discr_ty = self.alloc_ty(ItemTyKind::Node(NodeTy::BitVec(discr_width)));

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
}
