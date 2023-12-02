use std::{fmt::Debug, iter};

use fhdl_blackbox::{BlackboxKind, BlackboxTy};
use fhdl_netlist::{
    arena::with_arena,
    group::ItemId,
    sig_ty::{NodeTy, SignalTy, SignalTyKind, Width},
};
use rustc_ast::{
    token::{Lit, LitKind, Token, TokenKind},
    tokenstream::TokenTree,
    AttrArgs, AttrKind, DelimArgs,
};
use rustc_hir::{
    def_id::{DefId, LocalDefId},
    Ty as HirTy,
};
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_span::Span;
use rustc_type_ir::{
    TyKind::{self},
    UintTy,
};

use super::{generic::Generics, GenMode, Generator, SigTyInfo};
use crate::{
    blackbox::Blackbox,
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    utils,
};

const FHDL_TOOL: &str = "fhdl_tool";

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

    fn is_local(&self) -> bool {
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
            .and_then(|generics| generics.as_ty(ind))
    }

    pub fn generic_ty(&self, ind: usize, span: Span) -> Result<SignalTy, Error> {
        self.opt_generic_ty(ind)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedTy, span).into())
    }

    pub fn opt_generic_const(&self, ind: usize) -> Option<Width> {
        self.generics
            .as_ref()
            .and_then(|generics| generics.as_const(ind))
    }

    pub fn generic_const(&self, ind: usize, span: Span) -> Result<Width, Error> {
        self.opt_generic_const(ind)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedConst, span).into())
    }

    fn is_local(&self) -> bool {
        self.ty_or_def_id.is_local()
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn find_blackbox<T: IsTyOrDefId<'tcx>>(
        &mut self,
        key: T,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<Blackbox<'tcx>, Error> {
        let key = key.make(self.tcx, ctx);
        let key = self.eval_generics(key, ctx, span)?;

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.blackbox.contains_key(&key) {
            let mut blackbox = None;

            if let Some(def_id) = key.def_id() {
                blackbox = self.find_blackbox_(def_id);
            }

            self.blackbox.insert(key, blackbox);
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
            .map(|kind| Blackbox { kind, key })
            .map_err(Into::into)
    }

    fn find_blackbox_(&self, def_id: DefId) -> Option<BlackboxKind> {
        if self.crates.is_core(def_id) {
            let def_path = self.tcx.def_path_str(def_id);

            if def_path == "std::clone::Clone::clone" {
                return Some(BlackboxKind::StdClone);
            }
        }

        if self.crates.is_ferrum_hdl(def_id) {
            return self
                .find_fhdl_tool_attr("blackbox", def_id)
                .and_then(|kind| BlackboxKind::try_from(kind).ok());
        }

        None
    }

    pub fn find_sig_ty<T: IsTyOrDefId<'tcx>>(
        &mut self,
        key: T,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        self.find_sig_ty_info(key, ctx, span).map(|res| res.sig_ty)
    }

    fn is_local_key(&self, key: TyOrDefIdWithGen<'tcx>) -> bool {
        match self.mode {
            GenMode::Crate(_) => {
                key.is_local()
                    || key
                        .def_id()
                        .map(|def_id| self.is_synth_ty(def_id))
                        .unwrap_or_default()
            }
            GenMode::Fhdl => {
                key.is_local()
                    && key
                        .def_id()
                        .map(|def_id| self.is_synth_ty(def_id))
                        .unwrap_or_default()
            }
        }
    }

    pub fn find_sig_ty_info<T: IsTyOrDefId<'tcx>>(
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
                if self.is_local_key(key) {
                    sig_ty = Some(self.eval_adt_ty(ty, ctx, span)?);
                } else {
                    match ty.kind() {
                        TyKind::Array(..) => {
                            let ty = unsafe {
                                with_arena().alloc(key.opt_generic_ty(0).unwrap())
                            };
                            let cons = key.opt_generic_const(1).unwrap();

                            sig_ty = Some(SignalTy::mk_array(None, cons.value(), *ty));
                        }
                        TyKind::Bool => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::Bool.into()));
                        }
                        TyKind::Uint(UintTy::U8) => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::U8.into()))
                        }
                        TyKind::Uint(UintTy::U16) => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::U16.into()))
                        }
                        TyKind::Uint(UintTy::U32) => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::U32.into()))
                        }
                        TyKind::Uint(UintTy::U64) => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::U64.into()))
                        }
                        TyKind::Uint(UintTy::U128) => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::U128.into()))
                        }
                        TyKind::Uint(UintTy::Usize) => {
                            sig_ty = Some(SignalTy::new(None, NodeTy::Usize.into()))
                        }
                        TyKind::Tuple(ty) => {
                            sig_ty = Some(SignalTy::new(
                                None,
                                SignalTyKind::Struct(
                                    self.make_tuple_ty(ty.iter(), |generator, ty| {
                                        generator.find_sig_ty(ty, ctx, span)
                                    })?,
                                ),
                            ));
                        }
                        _ => {}
                    }
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
                    SpanErrorKind::MissingPrimTy(key.as_string(self.tcx)),
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

            return Ok(match blackbox_ty {
                BlackboxTy::Signal => key.opt_generic_ty(1),
                BlackboxTy::Wrapped => key.opt_generic_ty(1),
                BlackboxTy::BitVec => key.opt_generic_const(0).map(|val| {
                    SignalTy::new(Some(blackbox_ty), NodeTy::BitVec(val).into())
                }),
                BlackboxTy::Bit => {
                    Some(SignalTy::new(Some(blackbox_ty), NodeTy::Bit.into()))
                }
                BlackboxTy::Clock => {
                    Some(SignalTy::new(Some(blackbox_ty), NodeTy::Clock.into()))
                }
                BlackboxTy::Unsigned => {
                    let param = key.generic_const(0, span)?;
                    Some(SignalTy::new(
                        Some(blackbox_ty),
                        NodeTy::Unsigned(param).into(),
                    ))
                }
                BlackboxTy::UnsignedShort => {
                    let n = key.opt_generic_const(0).unwrap();
                    self.make_tuple_ty(iter::once(NodeTy::Unsigned(n)), |_, prim_ty| {
                        Ok(SignalTy::new(None, prim_ty.into()))
                    })
                    .ok()
                    .map(|ty| SignalTy::new(Some(blackbox_ty), ty.into()))
                }
                BlackboxTy::Array => {
                    let n = key.opt_generic_const(0).unwrap();
                    let ty = key.opt_generic_ty(1).unwrap();

                    Some(SignalTy::mk_array(Some(blackbox_ty), n.value(), ty))
                }
            });
        }

        Ok(None)
    }

    pub fn find_blackbox_ty(&mut self, def_id: DefId) -> Option<BlackboxTy> {
        let blackbox_ty = self.find_fhdl_tool_attr("blackbox_ty", def_id)?;
        let blackbox_ty = BlackboxTy::try_from(blackbox_ty).ok()?;

        Some(blackbox_ty)
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
            _ => None,
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

    pub fn find_fhdl_tool_attr(
        &self,
        attr_kind: &str,
        def_id: DefId,
    ) -> Option<&'tcx str> {
        let attrs = self.tcx.get_attrs_unchecked(def_id);
        for attr in attrs {
            if let AttrKind::Normal(attr) = &attr.kind {
                let segments = &attr.item.path.segments;
                if segments.len() == 2
                    && segments[0].ident.as_str() == FHDL_TOOL
                    && segments[1].ident.as_str() == attr_kind
                {
                    match &attr.item.args {
                        AttrArgs::Empty => {
                            return Some("");
                        }
                        AttrArgs::Delimited(DelimArgs { tokens, .. }) => {
                            if tokens.len() == 1 {
                                if let Some(TokenTree::Token(
                                    Token {
                                        kind:
                                            TokenKind::Literal(Lit {
                                                kind: LitKind::Str,
                                                symbol,
                                                ..
                                            }),
                                        ..
                                    },
                                    _,
                                )) = tokens.trees().next()
                                {
                                    return Some(symbol.as_str());
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        None
    }

    pub fn is_synth(&self, def_id: DefId) -> bool {
        self.find_fhdl_tool_attr("synth", def_id).is_some()
    }

    pub fn is_synth_ty(&self, def_id: DefId) -> bool {
        self.find_fhdl_tool_attr("synth_ty", def_id).is_some()
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
                SignalTy::new(None, self.net_list[node_out_id].ty.into())
            }
            ItemId::Group(group) => group.sig_ty,
        }
    }
}
