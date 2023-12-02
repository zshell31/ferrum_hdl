use std::{fmt::Debug, iter};

use fhdl_blackbox::BlackboxTy;
use fhdl_netlist::{
    arena::with_arena,
    group::ItemId,
    sig_ty::{NodeTy, SignalTy, SignalTyKind, Width},
};
use rustc_hir::{
    def_id::{DefId, LocalDefId},
    Ty as HirTy,
};
use rustc_middle::ty::{AliasKind, EarlyBinder, ParamEnv, Ty, TyCtxt};
use rustc_span::Span;
use rustc_type_ir::{
    TyKind::{self},
    UintTy,
};

use super::{generic::Generics, Generator, SigTyInfo};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    utils,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyOrDefId<'tcx> {
    Ty(Ty<'tcx>),
    DefId(DefId),
}

pub trait IsTyOrDefId<'tcx> {
    fn make(self, tcx: TyCtxt<'tcx>, ctx: &EvalContext<'tcx>) -> TyOrDefId<'tcx>;
}

impl<'tcx> IsTyOrDefId<'tcx> for DefId {
    fn make(self, _: TyCtxt<'tcx>, _: &EvalContext<'tcx>) -> TyOrDefId<'tcx> {
        TyOrDefId::DefId(self)
    }
}

impl<'tcx> IsTyOrDefId<'tcx> for Ty<'tcx> {
    fn make(self, tcx: TyCtxt<'tcx>, ctx: &EvalContext<'tcx>) -> TyOrDefId<'tcx> {
        let ty = ctx.instantiate(tcx, self);

        TyOrDefId::Ty(ty)
    }
}

impl<'tcx> TyOrDefId<'tcx> {
    pub fn def_id(&self) -> Option<DefId> {
        match self {
            Self::Ty(ty) => utils::ty_def_id(*ty),
            Self::DefId(def_id) => Some(*def_id),
        }
    }

    pub fn ty(&self) -> Option<Ty<'tcx>> {
        match self {
            Self::Ty(ty) => Some(*ty),
            Self::DefId(_) => None,
        }
    }

    fn as_string(&self, tcx: TyCtxt<'tcx>) -> String {
        match self.def_id() {
            Some(def_id) => tcx.def_path_str(def_id),
            None => match self {
                Self::Ty(ty) => ty.to_string(),
                Self::DefId(def_id) => tcx.def_path_str(def_id),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyOrDefIdWithGen<'tcx> {
    pub ty_or_def_id: TyOrDefId<'tcx>,
    pub generics: Option<Generics>,
}

impl<'tcx> TyOrDefIdWithGen<'tcx> {
    pub fn def_id(&self) -> Option<DefId> {
        self.ty_or_def_id.def_id()
    }

    pub fn ty(&self) -> Option<Ty<'tcx>> {
        self.ty_or_def_id.ty()
    }

    pub fn as_string(&self, tcx: TyCtxt<'tcx>) -> String {
        self.ty_or_def_id.as_string(tcx)
    }

    pub fn opt_generic_ty(&self, ind: usize) -> Option<SignalTy> {
        self.generics
            .as_ref()
            .and_then(|generics| generics.as_ty_opt(ind))
    }

    pub fn generic_ty(&self, ind: usize, span: Span) -> Result<SignalTy, Error> {
        self.opt_generic_ty(ind)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedTy, span).into())
    }

    pub fn opt_generic_const(&self, ind: usize) -> Option<Width> {
        self.generics
            .as_ref()
            .and_then(|generics| generics.as_const_opt(ind))
    }

    pub fn generic_const(&self, ind: usize, span: Span) -> Result<Width, Error> {
        self.opt_generic_const(ind)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedConst, span).into())
    }

    pub fn generic_const_value(&self, ind: usize, span: Span) -> Result<u128, Error> {
        self.opt_generic_const(ind)
            .and_then(|cons| cons.opt_value())
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedConst, span).into())
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn find_sig_ty<T: IsTyOrDefId<'tcx> + Debug>(
        &mut self,
        key: T,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        self.find_sig_ty_info(key, ctx, span).map(|res| res.sig_ty)
    }

    pub fn find_sig_ty_info<T: IsTyOrDefId<'tcx> + Debug>(
        &mut self,
        key: T,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SigTyInfo<'tcx>, Error> {
        let key = key.make(self.tcx, ctx);
        let key = self.eval_generics(key, ctx, span)?;

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.sig_ty.contains_key(&key) {
            let mut sig_ty = None;

            if let Some(ty) = &key.ty() {
                match ty.kind() {
                    TyKind::Array(..) => {
                        let ty =
                            unsafe { with_arena().alloc(key.opt_generic_ty(0).unwrap()) };
                        let cons = key.opt_generic_const(1).unwrap();

                        sig_ty = Some(SignalTy::mk_array(cons.value(), *ty));
                    }
                    TyKind::Bool => {
                        sig_ty = Some(SignalTy::new(NodeTy::Bool.into()));
                    }
                    TyKind::Uint(UintTy::U8) => {
                        sig_ty = Some(SignalTy::new(NodeTy::U8.into()))
                    }
                    TyKind::Uint(UintTy::U16) => {
                        sig_ty = Some(SignalTy::new(NodeTy::U16.into()))
                    }
                    TyKind::Uint(UintTy::U32) => {
                        sig_ty = Some(SignalTy::new(NodeTy::U32.into()))
                    }
                    TyKind::Uint(UintTy::U64) => {
                        sig_ty = Some(SignalTy::new(NodeTy::U64.into()))
                    }
                    TyKind::Uint(UintTy::U128) => {
                        sig_ty = Some(SignalTy::new(NodeTy::U128.into()))
                    }
                    TyKind::Uint(UintTy::Usize) => {
                        sig_ty = Some(SignalTy::new(NodeTy::Usize.into()))
                    }
                    TyKind::Tuple(ty) => {
                        sig_ty = Some(SignalTy::new(SignalTyKind::Struct(
                            self.make_tuple_ty(ty.iter(), |generator, ty| {
                                generator.find_sig_ty(ty, ctx, span)
                            })?,
                        )));
                    }
                    TyKind::Adt(adt, _) if !self.is_blackbox_ty(adt.did()) => {
                        sig_ty = Some(self.eval_adt_ty(ty, ctx, span)?);
                    }
                    TyKind::Alias(AliasKind::Projection, alias_ty) => {
                        let alias_ty = self
                            .tcx
                            .try_instantiate_and_normalize_erasing_regions(
                                ctx.instantiate(self.tcx, alias_ty.args),
                                ParamEnv::reveal_all(),
                                EarlyBinder::bind(*ty),
                            )
                            .map_err(|_| {
                                SpanError::new(
                                    SpanErrorKind::MissingNodeTy(ty.to_string()),
                                    span,
                                )
                            })?;

                        sig_ty = Some(self.find_sig_ty(alias_ty, ctx, span)?);
                    }
                    _ => {}
                }
            }

            if sig_ty.is_none() {
                if let Some(def_id) = key.def_id() {
                    sig_ty = self.find_sig_ty_(&key, def_id, span)?;
                }
            }

            self.sig_ty.insert(
                key,
                sig_ty.map(|sig_ty| SigTyInfo {
                    sig_ty,
                    ty_or_def_id: key,
                }),
            );
        }

        self.sig_ty
            .get(&key)
            .copied()
            .unwrap()
            .ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::MissingNodeTy(key.as_string(self.tcx)),
                    span,
                )
            })
            .map_err(Into::into)
    }

    fn find_sig_ty_(
        &mut self,
        key: &TyOrDefIdWithGen<'tcx>,
        def_id: DefId,
        span: Span,
    ) -> Result<Option<SignalTy>, Error> {
        if self.crates.is_ferrum_hdl(def_id) {
            let blackbox_ty = match self.find_blackbox_ty(def_id) {
                Some(blackbox_ty) => blackbox_ty,
                None => {
                    return Ok(None);
                }
            };

            let sig_ty = match blackbox_ty {
                BlackboxTy::Signal => key.opt_generic_ty(1),
                BlackboxTy::Wrapped => key.opt_generic_ty(1),
                BlackboxTy::BitVec => key
                    .opt_generic_const(0)
                    .map(|val| SignalTy::new(NodeTy::BitVec(val).into())),
                BlackboxTy::Bit => Some(SignalTy::new(NodeTy::Bit.into())),
                BlackboxTy::Clock => Some(SignalTy::new(NodeTy::Clock.into())),
                BlackboxTy::Unsigned => {
                    let param = key.generic_const(0, span)?;
                    Some(SignalTy::new(NodeTy::Unsigned(param).into()))
                }
                BlackboxTy::UnsignedShort => {
                    let n = key.opt_generic_const(0).unwrap();
                    self.make_tuple_ty(iter::once(NodeTy::Unsigned(n)), |_, prim_ty| {
                        Ok(SignalTy::new(prim_ty.into()))
                    })
                    .ok()
                    .map(|ty| SignalTy::new(ty.into()))
                }
                BlackboxTy::Array => {
                    let n = key.opt_generic_const(0).unwrap();
                    let ty = key.opt_generic_ty(1).unwrap();

                    Some(SignalTy::mk_array(n.value(), ty))
                }
            };

            if let Some(sig_ty) = &sig_ty {
                self.blackbox_ty.insert(*sig_ty, blackbox_ty);
            }

            return Ok(sig_ty);
        }

        Ok(None)
    }

    pub fn blackbox_ty(&self, sig_ty: &SignalTy) -> Option<BlackboxTy> {
        self.blackbox_ty.get(sig_ty).copied()
    }

    pub fn is_unsigned_short(&self, sig_ty: &SignalTy) -> bool {
        self.blackbox_ty(sig_ty)
            .filter(|blackbox_ty| blackbox_ty.is_unsigned_short())
            .is_some()
    }

    pub fn find_sig_ty_for_hir_ty(
        &mut self,
        fn_id: LocalDefId,
        ty: &HirTy<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let ty = self.ast_ty_to_ty(fn_id, ty);
        self.find_sig_ty(ty, ctx, span)
    }

    pub fn eval_generics(
        &mut self,
        ty_or_def_id: TyOrDefId<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<TyOrDefIdWithGen<'tcx>, Error> {
        let generics = match ty_or_def_id {
            TyOrDefId::Ty(ty) => Generics::from_ty(ty, self, ctx, span)?,
            TyOrDefId::DefId(did) => {
                // TODO: check if did referes to type
                let ty = self.type_of(did, ctx);
                Generics::from_ty(ty, self, ctx, span)?
            }
        };

        Ok(TyOrDefIdWithGen {
            ty_or_def_id,
            generics,
        })
    }

    pub fn eval_generic_args(
        &mut self,
        ty: Ty<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<Generics, Error> {
        Generics::from_args(self, ctx.generic_args.into_iter().map(Some), ty, ctx, span)
    }

    pub fn ignore_ty(&self, def_id: DefId) -> bool {
        self.crates.is_core(def_id)
            && self
                .tcx
                .get_lang_items(())
                .phantom_data()
                .filter(|phantom_data| *phantom_data == def_id)
                .is_some()
    }

    pub fn item_ty(&self, item_id: ItemId) -> SignalTy {
        match item_id {
            ItemId::Node(node_out_id) => {
                SignalTy::new(self.netlist[node_out_id].ty.into())
            }
            ItemId::Group(group) => group.sig_ty,
        }
    }
}
