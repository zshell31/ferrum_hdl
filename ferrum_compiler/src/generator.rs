pub mod adt;
pub mod arg_matcher;
pub mod bitvec;
pub mod expr;
pub mod func;
pub mod generic;
pub mod pattern_match;
pub mod ty_or_def_id;

use std::{env, fs, path::Path as StdPath};

use ferrum_blackbox::{Blackbox, BlackboxTy};
use ferrum_netlist::{
    backend::Verilog,
    group::ItemId,
    net_list::{ModuleId, NetList, NodeId},
    node::{IsNode, NodeKind, Pass},
    params::Outputs,
    sig_ty::SignalTy,
    symbol::Symbol,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def_id::{DefId, LocalDefId},
    Expr, HirId, ItemId as HirItemId, ItemKind, Ty as HirTy,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{
    mir::UnevaluatedConst,
    ty::{AssocKind, EarlyBinder, GenericArgsRef, List, ParamEnv, Ty, TyCtxt},
};
use rustc_session::EarlyErrorHandler;
use rustc_span::{def_id::CrateNum, symbol::Ident, Span};
use rustc_type_ir::fold::TypeFoldable;

use self::{generic::Generics, ty_or_def_id::TyOrDefIdWithGen};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    idents::Idents,
    utils,
};

pub struct CompilerCallbacks {}

impl Callbacks for CompilerCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _handler: &EarlyErrorHandler,
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
    let crates = Crates::find_crates(tcx)?;
    let top_module = find_top_module(tcx)?;

    Ok(Generator::new(tcx, top_module, crates))
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

#[derive(Clone, Copy)]
pub struct EvalContext<'tcx> {
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
}

impl<'tcx> EvalContext<'tcx> {
    fn new(generic_args: GenericArgsRef<'tcx>, module_id: ModuleId) -> Self {
        Self {
            generic_args,
            module_id,
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        EarlyBinder::bind(foldable).instantiate(tcx, self.generic_args)
    }
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
    ferrum: CrateNum,
}

impl Crates {
    fn find_crates(tcx: TyCtxt<'_>) -> Result<Self, Error> {
        let mut core = None;
        let mut std = None;
        let mut ferrum = None;

        for krate in tcx.crates(()) {
            let crate_name = tcx.crate_name(*krate);
            let crate_name = crate_name.as_str();

            if crate_name == "core" {
                core = Some(*krate);
            }
            if crate_name == "std" {
                std = Some(*krate);
            }
            if crate_name == "ferrum" {
                ferrum = Some(*krate);
            }
        }

        let core = core.ok_or_else(|| Error::MissingCrate("core"))?;
        let std = std.ok_or_else(|| Error::MissingCrate("std"))?;
        let ferrum = ferrum.ok_or_else(|| Error::MissingCrate("ferrum"))?;

        Ok(Self { core, std, ferrum })
    }

    pub(crate) fn is_core(&self, def_id: DefId) -> bool {
        def_id.krate == self.core || def_id.krate == self.std
    }

    pub(crate) fn is_ferrum(&self, def_id: DefId) -> bool {
        def_id.krate == self.ferrum
    }
}

pub struct Generator<'tcx> {
    tcx: TyCtxt<'tcx>,
    top_module: HirItemId,
    blackbox: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<Blackbox>>,
    sig_ty: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<SignalTy>>,
    local_trait_impls: FxHashMap<TraitKind, (DefId, DefId)>,
    evaluated_modules: FxHashMap<MonoItem<'tcx>, ModuleId>,
    crates: Crates,
    uniq_blackbox_tys: FxHashMap<BlackboxTy, DefId>,
    pub net_list: NetList,
    pub idents: Idents,
}

impl<'tcx> !Sync for Generator<'tcx> {}
impl<'tcx> !Send for Generator<'tcx> {}

pub struct TraitImpls<'tcx> {
    pub trait_id: DefId,
    pub fn_did: DefId,
    pub impls: &'tcx [LocalDefId],
}

impl<'tcx> Generator<'tcx> {
    fn new(tcx: TyCtxt<'tcx>, top_module: HirItemId, crates: Crates) -> Self {
        Self {
            tcx,
            top_module,
            blackbox: FxHashMap::default(),
            sig_ty: FxHashMap::default(),
            local_trait_impls: FxHashMap::default(),
            evaluated_modules: FxHashMap::default(),
            crates,
            uniq_blackbox_tys: FxHashMap::default(),
            net_list: NetList::default(),
            idents: Idents::new(),
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

        let item = self.tcx.hir().item(self.top_module);
        self.evaluate_fn_item(item, List::empty(), true)?;

        self.net_list.run_stages();

        let verilog = Verilog::new(&self.net_list).generate();

        Ok(fs::write(path, verilog)?)
    }

    fn collect_local_trait_impls(&mut self) {
        for (trait_id, _) in self.tcx.all_local_trait_impls(()) {
            let def_path = self.tcx.def_path_str(*trait_id);
            if def_path == "std::convert::From" {
                let fn_did = self
                    .tcx
                    .associated_items(*trait_id)
                    .find_by_name_and_kind(
                        self.tcx,
                        Ident::from_str("from"),
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

    pub fn find_local_trait_impls(
        &self,
        trait_kind: TraitKind,
    ) -> Option<TraitImpls<'tcx>> {
        self.local_trait_impls
            .get(&trait_kind)
            .and_then(|(trait_id, fn_did)| {
                self.tcx
                    .all_local_trait_impls(())
                    .get(trait_id)
                    .map(|impls| TraitImpls {
                        trait_id: *trait_id,
                        fn_did: *fn_did,
                        impls: impls.as_slice(),
                    })
            })
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

    pub fn link_dummy_inputs(
        &mut self,
        inputs: &[ItemId],
        closure: ItemId,
        span: Span,
    ) -> Result<(), Error> {
        let dummy_inputs_len = self
            .net_list
            .dummy_inputs_len(closure)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedClosure, span))?;

        let mut n = 0;

        for (ind, node_id) in inputs
            .iter()
            .flat_map(|input| input.into_iter())
            .enumerate()
        {
            n += 1;
            let dummy_input = self
                .net_list
                .dummy_input(closure, ind)
                .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedClosure, span))?;

            let node_out = self.net_list[node_id].kind.outputs().only_one();
            let dummy_out = self.net_list[dummy_input].kind.outputs().only_one();
            let sym = dummy_out.out.sym;

            let pass = Pass::new(node_out.out.ty, node_out.node_out_id(node_id), sym);
            self.net_list.replace(dummy_input, pass);

            self.propagate_sym_down(dummy_input, sym);
        }

        assert_eq!(n, dummy_inputs_len);

        Ok(())
    }

    pub fn propagate_sym_down(&mut self, node_id: NodeId, sym: Symbol) {
        let mut id = node_id;
        while let NodeKind::Pass(Pass { input, output }) = &mut self.net_list[id].kind {
            output.sym = sym;
            let input = *input;
            self.net_list[input].sym = sym;

            id = input.node_id();
        }
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
        let generic_args = self.extract_generic_args_for_fn(fn_did, expr, ctx)?;
        self.eval_generic_args(generic_args, expr.span)
    }

    pub fn eval_const_val(
        &self,
        def_id: DefId,
        generic_args: GenericArgsRef<'tcx>,
        span: Option<Span>,
    ) -> Option<u128> {
        let const_val = self
            .tcx
            .const_eval_resolve(
                ParamEnv::reveal_all(),
                UnevaluatedConst::new(def_id, generic_args),
                span,
            )
            .ok()?;

        utils::eval_const_val(const_val)
    }
}
