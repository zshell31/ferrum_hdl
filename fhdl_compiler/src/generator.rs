pub mod adt;
pub mod arg_matcher;
pub mod bitvec;
pub mod closure;
pub mod expr;
pub mod func;
pub mod generic;
pub mod metadata;
pub mod pattern_match;
pub mod ty_or_def_id;

use std::{env, fs, path::Path as StdPath};

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    backend::Verilog,
    group::ItemId,
    net_list::{ModuleId, NetList},
    sig_ty::SignalTy,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def_id::{DefId, LocalDefId, LOCAL_CRATE},
    Expr, HirId, ItemId as HirItemId, ItemKind, Node as HirNode, Ty as HirTy,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{
    mir::UnevaluatedConst,
    ty::{AssocKind, GenericArgs, GenericArgsRef, ParamEnv, Ty, TyCtxt},
};
use rustc_span::{def_id::CrateNum, symbol::Ident, Span};

use self::{generic::Generics, metadata::Metadata, ty_or_def_id::TyOrDefIdWithGen};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    scopes::Scopes,
    utils,
};

pub struct CompilerCallbacks {}

impl Callbacks for CompilerCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        queries
            .global_ctxt()
            .unwrap()
            .enter(|tcx| match init_generator(tcx) {
                Ok(mut generator) => {
                    generator.generate();
                }
                Err(e) => {
                    tcx.sess.err(e.to_string());
                }
            });

        Compilation::Continue
    }
}

fn init_generator(tcx: TyCtxt<'_>) -> Result<Generator, Error> {
    let mode = if env::var_os("FHDL_SYNTH").is_some() {
        GenMode::Fhdl
    } else {
        let top_module = find_top_module(tcx)?;
        GenMode::Crate(top_module)
    };
    let crates = Crates::find_crates(tcx, mode)?;

    Ok(Generator::new(tcx, crates, mode))
}

fn find_top_module(tcx: TyCtxt<'_>) -> Result<HirItemId, Error> {
    let hir = tcx.hir();
    for item_id in hir.items() {
        let item = hir.item(item_id);
        if let ItemKind::Fn(_, _, _) = item.kind {
            if item.ident.as_str() == "top_module" {
                return Ok(item_id);
            }
        }
    }

    Err(Error::MissingTopModule)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TraitKind {
    From,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonoItem<'tcx>(LocalDefId, GenericArgsRef<'tcx>);

impl<'tcx> MonoItem<'tcx> {
    pub fn new(item_id: LocalDefId, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self(item_id, generic_args)
    }
}

struct Crates {
    core: CrateNum,
    std: CrateNum,
    ferrum_hdl: CrateNum,
}

impl Crates {
    fn find_crates(tcx: TyCtxt<'_>, mode: GenMode) -> Result<Self, Error> {
        let mut core = None;
        let mut std = None;
        let mut ferrum_hdl = None;

        for krate in tcx.crates(()) {
            let crate_name = tcx.crate_name(*krate);
            let crate_name = crate_name.as_str();

            if crate_name == "core" {
                core = Some(*krate);
            }
            if crate_name == "std" {
                std = Some(*krate);
            }

            if mode.is_crate() && crate_name == "ferrum_hdl" {
                ferrum_hdl = Some(*krate);
            }
        }

        let core = core.ok_or_else(|| Error::MissingCrate("core"))?;
        let std = std.ok_or_else(|| Error::MissingCrate("std"))?;
        let ferrum_hdl = match mode {
            GenMode::Crate(_) => {
                ferrum_hdl.ok_or_else(|| Error::MissingCrate("ferrum_hdl"))?
            }
            GenMode::Fhdl => LOCAL_CRATE,
        };

        Ok(Self {
            core,
            std,
            ferrum_hdl,
        })
    }

    pub(crate) fn is_core(&self, def_id: DefId) -> bool {
        def_id.krate == self.core || def_id.krate == self.std
    }

    pub(crate) fn is_ferrum_hdl(&self, def_id: DefId) -> bool {
        def_id.krate == self.ferrum_hdl
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SigTyInfo<'tcx> {
    pub sig_ty: SignalTy,
    pub ty_or_def_id: TyOrDefIdWithGen<'tcx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenMode {
    Fhdl,
    Crate(HirItemId),
}

impl GenMode {
    pub fn is_crate(&self) -> bool {
        matches!(self, Self::Crate(_))
    }
}

pub struct Generator<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub net_list: NetList,
    pub idents: Scopes,
    mode: GenMode,
    blackbox: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<BlackboxKind>>,
    sig_ty: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<SigTyInfo<'tcx>>>,
    local_trait_impls: FxHashMap<TraitKind, (DefId, DefId)>,
    evaluated_modules: FxHashMap<MonoItem<'tcx>, ModuleId>,
    crates: Crates,
    metadata: Metadata<'tcx>,
}

impl<'tcx> Generator<'tcx> {
    fn new(tcx: TyCtxt<'tcx>, crates: Crates, mode: GenMode) -> Self {
        Self {
            tcx,
            net_list: NetList::new(),
            idents: Scopes::new(),
            mode,
            blackbox: Default::default(),
            sig_ty: Default::default(),
            local_trait_impls: Default::default(),
            evaluated_modules: Default::default(),
            crates,
            metadata: Default::default(),
        }
    }

    pub fn generate(&mut self) {
        if let Err(e) = self.generate_inner() {
            self.emit_err(e);
        }
    }

    fn generate_inner(&mut self) -> Result<(), Error> {
        let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        // let name = env::var("CARGO_PKG_NAME").unwrap();
        let name = "top_module";

        let path = StdPath::new(&dir).join("generated").join("verilog");
        fs::create_dir_all(&path)?;

        let mut path = path.join(name);
        path.set_extension("v");

        self.collect_local_trait_impls();

        let generic_args = GenericArgs::empty();
        match self.mode {
            GenMode::Fhdl => {
                for body_id in self.tcx.hir().body_owners() {
                    if self.is_synth(body_id.into()) {
                        match self.tcx.hir().get_by_def_id(body_id) {
                            HirNode::Item(item) => {
                                self.eval_fn_item(item, false, generic_args)?;
                            }
                            HirNode::ImplItem(impl_item) => {
                                self.eval_impl_item(impl_item, generic_args)?;
                            }
                            _ => {
                                return Err(SpanError::new(
                                    SpanErrorKind::NotSynthItem,
                                    self.tcx
                                        .def_ident_span(body_id)
                                        .unwrap_or_else(|| self.tcx.def_span(body_id)),
                                )
                                .into());
                            }
                        };
                    }
                }

                self.net_list.transform();
                self.net_list.reachability();

                let path = StdPath::new(&dir).join("src/fhdl.net_list");
                self.encode_netlist(path)?;

                Ok(())
            }
            GenMode::Crate(top_module) => {
                let item = self.tcx.hir().item(top_module);
                self.eval_fn_item(item, true, GenericArgs::empty())?;

                self.net_list.transform();
                self.net_list.reachability();
                self.net_list.set_names();
                self.net_list.inject_nodes();

                let verilog = Verilog::new(&self.net_list).generate();

                Ok(fs::write(path, verilog)?)
            }
        }
    }

    fn collect_local_trait_impls(&mut self) {
        for (trait_id, _) in self.tcx.all_local_trait_impls(()) {
            if self.crates.is_ferrum_hdl(*trait_id)
                && self.find_fhdl_tool_attr("cast_from", *trait_id).is_some()
            {
                let fn_did = self
                    .tcx
                    .associated_items(*trait_id)
                    .find_by_name_and_kind(
                        self.tcx,
                        Ident::from_str("cast_from"),
                        AssocKind::Fn,
                        *trait_id,
                    )
                    .unwrap()
                    .def_id;

                self.local_trait_impls
                    .insert(TraitKind::From, (*trait_id, fn_did));
            }
        }
    }

    pub fn find_trait_method(&self, trait_kind: TraitKind) -> Option<DefId> {
        self.local_trait_impls
            .get(&trait_kind)
            .map(|(_, fn_did)| *fn_did)
    }

    fn emit_err(&mut self, err: Error) {
        match err {
            Error::Span(SpanError { kind, span }) => {
                self.tcx.sess.span_err(span, kind.to_string());
            }
            _ => {
                self.tcx.sess.err(err.to_string());
            }
        };
        self.tcx.sess.abort_if_errors();
    }

    pub fn node_type(&self, hir_id: HirId, ctx: &EvalContext<'tcx>) -> Ty<'tcx> {
        let owner_id = hir_id.owner;
        let typeck_res = self.tcx.typeck(owner_id);
        let ty = typeck_res.node_type(hir_id);
        ctx.instantiate(self.tcx, ty)
    }

    pub fn subst_with(
        &self,
        subst: GenericArgsRef<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> GenericArgsRef<'tcx> {
        ctx.instantiate(self.tcx, subst)
    }

    pub fn item_id_for_ident(
        &mut self,
        module_id: ModuleId,
        ident: Ident,
    ) -> Result<ItemId, Error> {
        self.idents
            .for_module(module_id)
            .item_id(ident)
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::MissingNodeForIdent(ident), ident.span)
            })
            .map_err(Into::into)
    }

    pub fn ast_ty_to_ty(&self, fn_id: LocalDefId, ty: &HirTy<'tcx>) -> Ty<'tcx> {
        let item_ctx = &ItemCtxt::new(self.tcx, fn_id);
        let astconv = item_ctx.astconv();
        astconv.ast_ty_to_ty(ty)
    }

    pub fn method_call_generics(
        &mut self,
        expr: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<Generics, Error> {
        let fn_did = self
            .tcx
            .typeck(expr.hir_id.owner)
            .type_dependent_def_id(expr.hir_id)
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::ExpectedMethodCall, expr.span)
            })?;
        let ty = self.type_of(fn_did, ctx);
        let generic_args = self.extract_generic_args(fn_did, expr.hir_id, ctx)?;
        self.eval_generic_args(ty, &ctx.with_generic_args(generic_args), expr.span)
    }

    pub fn eval_const_val(
        &self,
        def_id: DefId,
        ctx: &EvalContext<'tcx>,
        span: Option<Span>,
    ) -> Option<u128> {
        let const_val = self
            .tcx
            .const_eval_resolve(
                ParamEnv::reveal_all(),
                UnevaluatedConst::new(def_id, ctx.generic_args),
                span,
            )
            .ok()?;

        utils::eval_const_val(const_val)
    }

    pub fn maybe_swap_generic_args_for_conversion(
        &self,
        from: bool,
        generic_args: GenericArgsRef<'tcx>,
    ) -> GenericArgsRef<'tcx> {
        match from {
            true => generic_args,
            // Swap generic args from Into::<Self, T> -> From::<Self = T, T = Self>
            false => self.tcx.mk_args(&[generic_args[1], generic_args[0]]),
        }
    }

    pub fn type_of(&self, def_id: DefId, ctx: &EvalContext<'tcx>) -> Ty<'tcx> {
        ctx.instantiate_early_binder(self.tcx, self.tcx.type_of(def_id))
    }

    pub fn local_def_id<T: Into<DefId>>(&self, def_id: T) -> Option<LocalDefId> {
        let def_id = def_id.into();
        let local_def_id = def_id.as_local()?;
        match self.mode {
            GenMode::Crate(_) => Some(local_def_id),
            GenMode::Fhdl if self.is_synth(def_id) => Some(local_def_id),
            _ => None,
        }
    }

    pub fn is_local_def_id<T: Into<DefId>>(&self, def_id: T) -> bool {
        self.local_def_id(def_id).is_some()
    }
}
