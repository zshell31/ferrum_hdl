use std::fmt::Debug;

use ferrum_netlist::{
    arena::with_arena,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_hir::{
    def_id::{DefId, LocalDefId},
    Ty as HirTy,
};
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, Ty, TyCtxt};
use rustc_span::Span;
use rustc_type_ir::{
    TyKind::{self},
    UintTy,
};

use super::{generic::Generics, Generator};
use crate::{
    blackbox::{self, Blackbox},
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyOrDefId<'tcx> {
    Ty(Ty<'tcx>),
    DefId(DefId),
}

pub trait IsTyOrDefId<'tcx> {
    fn make(self, tcx: TyCtxt<'tcx>, generics: GenericArgsRef<'tcx>) -> TyOrDefId<'tcx>;
}

impl<'tcx> IsTyOrDefId<'tcx> for DefId {
    fn make(self, _: TyCtxt<'tcx>, _: GenericArgsRef<'tcx>) -> TyOrDefId<'tcx> {
        TyOrDefId::DefId(self)
    }
}

impl<'tcx> IsTyOrDefId<'tcx> for Ty<'tcx> {
    fn make(self, tcx: TyCtxt<'tcx>, generics: GenericArgsRef<'tcx>) -> TyOrDefId<'tcx> {
        let ty = EarlyBinder::bind(self).instantiate(tcx, generics);

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

    pub fn is_local(&self) -> bool {
        matches!(self.def_id(), Some(did) if did.is_local())
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyOrDefIdWithGen<'tcx> {
    pub ty_or_def_id: TyOrDefId<'tcx>,
    pub generics: Option<Generics>,
}

impl<'tcx> TyOrDefIdWithGen<'tcx> {
    pub fn def_id(&self) -> Option<DefId> {
        self.ty_or_def_id.def_id()
    }

    pub fn as_string(&self, tcx: TyCtxt<'tcx>) -> String {
        self.ty_or_def_id.as_string(tcx)
    }

    pub fn generic_ty(&self, ind: usize) -> Option<SignalTy> {
        self.generics
            .as_ref()
            .and_then(|generics| generics.as_ty(ind))
    }

    pub fn generic_const<T: TryFrom<u128>>(&self, ind: usize) -> Option<T> {
        self.generics
            .as_ref()
            .and_then(|generics| generics.as_const(ind))
    }

    pub fn is_local(&self) -> bool {
        self.ty_or_def_id.is_local()
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn find_blackbox<T: IsTyOrDefId<'tcx>>(
        &mut self,
        key: T,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Blackbox, Error> {
        let key = key.make(self.tcx, generics);
        let key = self.evaluate_generics(key, generics, span)?;

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.blackbox.contains_key(&key) {
            let mut blackbox = None;

            if let Some(def_id) = key.def_id() {
                let def_path = self.tcx.def_path(def_id);
                blackbox = blackbox::find_blackbox(&def_path);
            }

            self.blackbox.insert(key.clone(), blackbox);
        }

        self.blackbox
            .get(&key)
            .unwrap()
            .ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::MissingBlackbox(key.as_string(self.tcx)),
                    span,
                )
            })
            .map_err(Into::into)
    }

    pub fn find_sig_ty<T: IsTyOrDefId<'tcx>>(
        &mut self,
        key: T,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let key = key.make(self.tcx, generics);
        let key = self.evaluate_generics(key, generics, span)?;

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.sig_ty.contains_key(&key) {
            let mut sig_ty = None;

            if let TyOrDefId::Ty(ty) = &key.ty_or_def_id {
                if key.is_local() {
                    sig_ty = self.evaluate_struct_ty(ty, generics, span)?;
                } else {
                    match ty.kind() {
                        TyKind::Array(..) => {
                            let ty =
                                unsafe { with_arena().alloc(key.generic_ty(0).unwrap()) };
                            let cons = key.generic_const(1).unwrap();

                            sig_ty = Some(SignalTy::mk_array(cons, *ty));
                        }
                        TyKind::Bool => {
                            sig_ty = Some(PrimTy::Bool.into());
                        }
                        TyKind::Uint(UintTy::U128 | UintTy::Usize) => {
                            sig_ty = Some(PrimTy::U128.into());
                        }
                        TyKind::Tuple(ty) => {
                            sig_ty = Some(
                                self.make_tuple_sig_ty(ty.iter(), |generator, ty| {
                                    generator.find_sig_ty(ty, generics, span)
                                })?,
                            );
                        }
                        _ => {}
                    }
                }
            }

            if sig_ty.is_none() {
                if let Some(def_id) = key.def_id() {
                    let def_path = self.tcx.def_path(def_id);
                    sig_ty = blackbox::find_sig_ty(&key, &def_path);
                }
            }

            self.sig_ty.insert(key.clone(), sig_ty);
        }

        self.sig_ty
            .get(&key)
            .cloned()
            .unwrap()
            .ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::MissingPrimTy(key.as_string(self.tcx)),
                    span,
                )
            })
            .map_err(Into::into)
    }

    pub fn find_sig_ty_for_hir_ty(
        &mut self,
        fn_id: LocalDefId,
        ty: &HirTy<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let ty = self.ast_ty_to_ty(fn_id, ty);
        self.find_sig_ty(ty, generics, span)
    }

    pub fn evaluate_generics(
        &mut self,
        ty_or_def_id: TyOrDefId<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<TyOrDefIdWithGen<'tcx>, Error> {
        let generics = match ty_or_def_id {
            TyOrDefId::Ty(ty) if !ty_or_def_id.is_local() => {
                Generics::from_ty(&ty, self, generics, span)?
            }
            _ => None,
        };

        Ok(TyOrDefIdWithGen {
            ty_or_def_id,
            generics,
        })
    }

    pub fn eval_generic_args(
        &mut self,
        generic_args: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Generics, Error> {
        Generics::from_args(self, generic_args.into_iter().map(Some), span)
    }
}
