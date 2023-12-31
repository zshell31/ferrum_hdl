pub mod adt;
// pub mod arg_matcher;
pub mod attr;
pub mod bitvec;
pub mod closure;
pub mod expr;
pub mod func;
pub mod generic;
pub mod metadata;
pub mod pattern_match;
pub mod temp_nodes;
pub mod ty_or_def_id;

use std::{
    env, error::Error as StdError, fmt::Display, fs, io, path::Path as StdPath, rc::Rc,
};

use cargo_toml::Manifest;
use fhdl_blackbox::{BlackboxKind, BlackboxTy};
use fhdl_netlist::{
    backend::Verilog,
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
use serde::Deserialize;

use self::{
    closure::Closure, generic::Generics, metadata::Metadata,
    ty_or_def_id::TyOrDefIdWithGen,
};
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
        let res = queries.global_ctxt().unwrap().enter(|tcx| {
            let is_primary = env::var("CARGO_PRIMARY_PACKAGE").is_ok();
            let should_be_synthesized = match should_be_synthesized(is_primary) {
                Ok(should_be_synthesized) => should_be_synthesized,
                Err(e) => {
                    tcx.sess.err(e.to_string());
                    return false;
                }
            };

            if should_be_synthesized {
                match init_generator(tcx, is_primary) {
                    Ok(mut generator) => {
                        generator.generate();
                        true
                    }
                    Err(e) => {
                        tcx.sess.err(e.to_string());
                        false
                    }
                }
            } else {
                true
            }
        });

        if res {
            Compilation::Continue
        } else {
            Compilation::Stop
        }
    }
}

fn should_be_synthesized(is_primary: bool) -> Result<bool, Box<dyn StdError>> {
    if is_primary {
        Ok(true)
    } else {
        let manifest = env::var_os("CARGO_MANIFEST_DIR")
            .and_then(|var| var.to_str().map(|var| StdPath::new(var).join("Cargo.toml")))
            .ok_or("`CARGO_MANIFEST_DIR` is not specified")?;

        let manifest = fs::read(&manifest).map_err(|e| {
            format!(
                "Failed to read from `{}`: {}",
                manifest.to_string_lossy(),
                e
            )
        })?;

        #[allow(dead_code)]
        #[derive(Deserialize)]
        struct FhdlMetadata {
            #[serde(default)]
            synth: bool,
        }

        #[allow(dead_code)]
        #[derive(Deserialize)]
        struct Metadata {
            ferrum_hdl: Option<FhdlMetadata>,
        }

        let manifest = Manifest::<Metadata>::from_slice_with_metadata(&manifest)?;

        Ok(manifest
            .package
            .and_then(|package| package.metadata)
            .and_then(|metadata| metadata.ferrum_hdl)
            .map(|metadata| metadata.synth)
            .unwrap_or_default())
    }
}

fn init_generator(tcx: TyCtxt<'_>, is_primary: bool) -> Result<Generator, Error> {
    let top_module = if is_primary {
        Some(find_top_module(tcx)?)
    } else {
        None
    };
    let crates = Crates::find_crates(tcx, is_primary)?;

    Ok(Generator::new(tcx, is_primary, top_module, crates))
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
pub struct MonoItem<'tcx>(DefId, GenericArgsRef<'tcx>);

impl<'tcx> MonoItem<'tcx> {
    pub fn new<T: Into<DefId>>(item_id: T, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self(item_id.into(), generic_args)
    }
}

struct Crates {
    core: CrateNum,
    std: CrateNum,
    ferrum_hdl: CrateNum,
}

impl Crates {
    fn find_crates(tcx: TyCtxt<'_>, is_primary: bool) -> Result<Self, Error> {
        const CORE: &str = "core";
        const STD: &str = "std";
        const FERRUM_HDL: &str = "ferrum_hdl";

        let mut core = None;
        let mut std = None;
        let mut ferrum_hdl = None;

        for krate in tcx.crates(()) {
            let crate_name = tcx.crate_name(*krate);
            let crate_name = crate_name.as_str();

            if crate_name == CORE {
                core = Some(*krate);
                continue;
            }
            if crate_name == STD {
                std = Some(*krate);
                continue;
            }
            if crate_name == FERRUM_HDL {
                ferrum_hdl = Some(*krate);
            }
        }

        let core = core.ok_or_else(|| Error::MissingCrate(CORE))?;
        let std = std.ok_or_else(|| Error::MissingCrate(STD))?;
        let ferrum_hdl = (if is_primary {
            ferrum_hdl
        } else {
            ferrum_hdl.or_else(|| {
                if tcx.crate_name(LOCAL_CRATE).as_str() == FERRUM_HDL {
                    Some(LOCAL_CRATE)
                } else {
                    None
                }
            })
        })
        .ok_or_else(|| Error::MissingCrate(FERRUM_HDL))?;

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

    pub(crate) fn is_ferrum_hdl_local(&self) -> bool {
        self.ferrum_hdl == LOCAL_CRATE
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SigTyInfo<'tcx> {
    pub sig_ty: SignalTy,
    pub ty_or_def_id: TyOrDefIdWithGen<'tcx>,
}

pub struct Settings {
    pub trace_eval_expr: bool,
}

impl Settings {
    fn new() -> Self {
        Self {
            trace_eval_expr: env::var("TRACE_EVAL_EXPR").is_ok(),
        }
    }
}

pub struct Generator<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub netlist: NetList,
    pub idents: Scopes,
    pub settings: Settings,
    is_primary: bool,
    top_module: Option<HirItemId>,
    crates: Crates,
    blackbox: FxHashMap<DefId, Option<BlackboxKind>>,
    sig_ty: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<SigTyInfo<'tcx>>>,
    blackbox_ty: FxHashMap<SignalTy, BlackboxTy>,
    local_trait_impls: FxHashMap<TraitKind, (DefId, DefId)>,
    evaluated_modules: FxHashMap<MonoItem<'tcx>, ModuleId>,
    metadata: Metadata<'tcx>,
    loaded_metadata: FxHashMap<CrateNum, Rc<Metadata<'tcx>>>,
    closures: FxHashMap<ModuleId, Closure>,
}

impl<'tcx> Generator<'tcx> {
    fn new(
        tcx: TyCtxt<'tcx>,
        is_primary: bool,
        top_module: Option<HirItemId>,
        crates: Crates,
    ) -> Self {
        Self {
            tcx,
            netlist: NetList::new(),
            idents: Scopes::new(),
            settings: Settings::new(),
            top_module,
            is_primary,
            crates,
            blackbox: Default::default(),
            sig_ty: Default::default(),
            blackbox_ty: Default::default(),
            local_trait_impls: Default::default(),
            evaluated_modules: Default::default(),
            metadata: Default::default(),
            loaded_metadata: Default::default(),
            closures: Default::default(),
        }
    }

    pub fn generate(&mut self) {
        if let Err(e) = self.generate_inner() {
            self.emit_err(e);
        }
    }

    fn generate_inner(&mut self) -> Result<(), Error> {
        let crate_name = self.tcx.crate_name(LOCAL_CRATE);

        self.collect_local_trait_impls();

        let generic_args = GenericArgs::empty();
        if !self.is_primary {
            self.print_message(&"Synthesizing", Some(&crate_name.as_str()))?;

            for body_id in self.tcx.hir().body_owners() {
                if self
                    .metadata
                    .find_module_id_by_def_id(body_id.into())
                    .is_none()
                    && self.is_synth(body_id.into())
                {
                    let module_id = match self.tcx.hir().get_by_def_id(body_id) {
                        HirNode::Item(item) => {
                            self.eval_fn_item(item, false, generic_args)?
                        }
                        HirNode::ImplItem(impl_item) => {
                            self.eval_impl_fn_item(impl_item, generic_args)?
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

                    self.metadata.add_module_id(body_id, module_id);
                }
            }

            self.netlist.assert();
            self.netlist.transform();
            self.netlist.reachability();

            self.encode_netlist()?;

            Ok(())
        } else {
            let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
            let name = "top_module";

            let path = StdPath::new(&dir).join("generated").join("verilog");
            fs::create_dir_all(&path)?;

            let mut path = path.join(name);
            path.set_extension("v");

            self.print_message(
                &"Synthesizing",
                Some(&format!(
                    "{} into verilog {}",
                    crate_name.as_str(),
                    path.to_string_lossy()
                )),
            )?;

            let top_module = self.top_module.unwrap();
            let item = self.tcx.hir().item(top_module);
            self.eval_fn_item(item, true, GenericArgs::empty())?;

            self.netlist.assert();
            self.netlist.transform();
            self.netlist.reachability();
            self.netlist.set_names();
            self.netlist.inject_nodes();

            let verilog = Verilog::new(&self.netlist).generate();

            Ok(fs::write(path, verilog)?)
        }
    }

    fn print_message(
        &self,
        status: &dyn Display,
        message: Option<&dyn Display>,
    ) -> io::Result<()> {
        // TODO: use Cargo settings for colors
        // Code from https://github.com/rust-lang/cargo/blob/2130a0faf0cb6aa44c5962c2f2d313fa7e459b2b/src/cargo/core/shell.rs#L451
        use std::io::Write;

        use anstream::AutoStream;
        use anstyle::{AnsiColor, Effects, Reset, Style};

        const HEADER: Style = AnsiColor::Green.on_default().effects(Effects::BOLD);

        let style = HEADER.render();
        let reset = Reset.render();

        let mut stream = AutoStream::always(std::io::stdout());
        write!(&mut stream, "{style}{status:>12}{reset}")?;
        if let Some(message) = message {
            writeln!(&mut stream, " {message}")?;
        }

        Ok(())
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
        let generic_args = self.extract_generic_args(expr.hir_id, ctx);
        let ctx = ctx.with_generic_args(generic_args);
        let ty = self.type_of(fn_did, &ctx);
        self.eval_generic_args(ty, &ctx, expr.span)
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
            // Swap generic args for Cast::<Self>::cast<T>() -> CastFrom::<Self = T, T = Self>
            false => self.tcx.mk_args(&[generic_args[1], generic_args[0]]),
        }
    }

    pub fn type_of(&self, def_id: DefId, ctx: &EvalContext<'tcx>) -> Ty<'tcx> {
        ctx.instantiate_early_binder(self.tcx, self.tcx.type_of(def_id))
    }

    pub fn local_def_id<T: Into<DefId>>(&self, def_id: T) -> Option<LocalDefId> {
        let def_id = def_id.into();
        let local_def_id = def_id.as_local()?;

        match self.crates.is_ferrum_hdl_local() {
            false => Some(local_def_id),
            true if self.is_synth(def_id) => Some(local_def_id),
            _ => None,
        }
    }

    pub fn is_local_def_id<T: Into<DefId>>(&self, def_id: T) -> bool {
        self.local_def_id(def_id).is_some()
    }
}
