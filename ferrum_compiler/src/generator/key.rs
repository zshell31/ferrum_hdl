use std::fmt::Debug;

use ferrum_netlist::{
    arena::with_arena,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_hir::{
    def_id::{DefId, LocalDefId, LOCAL_CRATE},
    Ty as HirTy,
};
use rustc_middle::ty::{EarlyBinder, GenericArg, List, Ty, TyCtxt};
use rustc_span::Span;
use rustc_type_ir::{
    TyKind::{self},
    UintTy,
};

use super::{generic::Generics, Generator};
use crate::{
    blackbox::{self, Blackbox},
    error::{Error, SpanError, SpanErrorKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyOrDefId<'tcx> {
    Ty(Ty<'tcx>),
    DefId(DefId),
}

impl<'tcx> From<Ty<'tcx>> for TyOrDefId<'tcx> {
    fn from(ty: Ty<'tcx>) -> Self {
        TyOrDefId::Ty(ty)
    }
}

impl<'tcx> From<DefId> for TyOrDefId<'tcx> {
    fn from(def_id: DefId) -> Self {
        TyOrDefId::DefId(def_id)
    }
}

impl<'tcx> TyOrDefId<'tcx> {
    pub fn def_id(&self) -> Option<DefId> {
        match self {
            Self::Ty(ty) => match ty.kind() {
                TyKind::Adt(adt, _) => Some(adt.did()),
                _ => None,
            },

            Self::DefId(def_id) => Some(*def_id),
        }
    }

    pub fn local_def_id(&self) -> Option<LocalDefId> {
        match self.def_id() {
            Some(did) if did.krate == LOCAL_CRATE => Some(LocalDefId {
                local_def_index: did.index,
            }),
            _ => None,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Key<'tcx> {
    pub ty_or_def_id: TyOrDefId<'tcx>,
    pub generics: Option<Generics>,
}

impl<'tcx> Key<'tcx> {
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
}

pub trait AsKey<'tcx> {
    fn as_key(
        &self,
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Key<'tcx>, Error>;
}

impl<'tcx> AsKey<'tcx> for Ty<'tcx> {
    fn as_key(
        &self,
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Key<'tcx>, Error> {
        let ty = match generics {
            Some(generics) => {
                EarlyBinder::bind(*self).instantiate(generator.tcx, generics)
            }
            None => *self,
        };

        let ty_or_def_id: TyOrDefId = ty.into();

        let generics = Generics::from_ty(&ty, generator, generics, span)?;

        Ok(Key {
            ty_or_def_id,
            generics,
        })
    }
}

impl<'tcx> AsKey<'tcx> for DefId {
    fn as_key(
        &self,
        _: &mut Generator<'tcx>,
        _: Option<&List<GenericArg<'tcx>>>,
        _: Span,
    ) -> Result<Key<'tcx>, Error> {
        // let tcx = generator.tcx;
        // let ty = tcx.type_of(*self).instantiate_identity();

        // let ty = match generics {
        //     Some(generics) => ty.instantiate(tcx, generics),
        //     None => ty.instantiate_identity(),
        // };

        Ok(Key {
            ty_or_def_id: (*self).into(),
            generics: None,
        })
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn find_blackbox<K: AsKey<'tcx>>(
        &mut self,
        key: &K,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Blackbox, Error> {
        let key = key.as_key(self, generics, span)?;

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

    pub fn find_sig_ty<K: AsKey<'tcx> + Debug>(
        &mut self,
        key: &K,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let key = key.as_key(self, generics, span)?;

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.sig_ty.contains_key(&key) {
            let mut sig_ty = None;

            if let TyOrDefId::Ty(ty) = key.ty_or_def_id {
                match ty.kind() {
                    TyKind::Array(..) => {
                        let ty =
                            unsafe { with_arena().alloc(key.generic_ty(0).unwrap()) };
                        let cons = key.generic_const(1).unwrap();

                        sig_ty = Some(SignalTy::Array(cons, ty));
                    }
                    TyKind::Bool => {
                        sig_ty = Some(PrimTy::Bool.into());
                    }
                    TyKind::Uint(UintTy::U128) => {
                        sig_ty = Some(PrimTy::U128.into());
                    }
                    TyKind::Tuple(ty) => {
                        sig_ty = Some(SignalTy::group(
                            ty.iter()
                                .map(|ty| self.find_sig_ty(&ty, generics, span))
                                .collect::<Result<Vec<_>, _>>()?,
                        ));
                    }
                    _ => {}
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
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let ty = self.ast_ty_to_ty(fn_id, ty);
        self.find_sig_ty(&ty, generics, span)
    }
}
