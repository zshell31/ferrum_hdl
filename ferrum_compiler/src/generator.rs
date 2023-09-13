mod arg_matcher;
pub mod expr;
pub mod func;
pub mod generic;
pub mod struc;
pub mod ty_or_def_id;

use std::{env, fs, path::Path as StdPath};

use ferrum_netlist::{
    backend::Verilog,
    group_list::{GroupList, ItemId},
    inject_pass::InjectPass,
    net_list::{ModuleId, NetList},
    node::{IsNode, PassNode},
    params::Outputs,
    sig_ty::SignalTy,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def_id::{DefId, LocalDefId},
    HirId, ItemId as HirItemId, ItemKind, Ty as HirTy,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, List, Ty, TyCtxt};
use rustc_session::EarlyErrorHandler;
use rustc_span::{symbol::Ident, Span};

use self::ty_or_def_id::TyOrDefIdWithGen;
use crate::{
    blackbox::{Blackbox, ItemPath},
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
            .enter(|tcx| match find_top_module(tcx) {
                Ok(top_module) => {
                    let mut generator = Generator::new(tcx, top_module);
                    generator.generate();
                }
                Err(e) => {
                    tcx.sess.err(e.to_string());
                }
            });

        Compilation::Continue
    }
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TraitKind {
    From,
}

pub struct Generator<'tcx> {
    tcx: TyCtxt<'tcx>,
    top_module: HirItemId,
    blackbox: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<Blackbox>>,
    sig_ty: FxHashMap<TyOrDefIdWithGen<'tcx>, Option<SignalTy>>,
    local_trait_impls: FxHashMap<TraitKind, (DefId, DefId)>,
    pub net_list: NetList,
    pub group_list: GroupList,
    pub idents: Idents,
}

impl<'tcx> !Sync for Generator<'tcx> {}
impl<'tcx> !Send for Generator<'tcx> {}

pub struct TraitImpls<'a> {
    pub trait_id: DefId,
    pub fn_did: DefId,
    pub impls: &'a [LocalDefId],
}

impl<'tcx> Generator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, top_module: HirItemId) -> Self {
        Self {
            tcx,
            top_module,
            blackbox: FxHashMap::default(),
            sig_ty: FxHashMap::default(),
            local_trait_impls: FxHashMap::default(),
            net_list: NetList::default(),
            group_list: GroupList::default(),
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

        InjectPass::new(&mut self.net_list).inject();

        let verilog = Verilog::new(&self.net_list).generate();

        Ok(fs::write(path, verilog)?)
    }

    fn collect_local_trait_impls(&mut self) {
        for (trait_id, impls) in self.tcx.all_local_trait_impls(()) {
            let def_path = self.tcx.def_path(*trait_id);
            let mut kind = None;
            if def_path == ItemPath(&["convert", "From"]) {
                kind = Some(TraitKind::From);
            }

            if let Some(kind) = kind {
                let imp = impls[0];
                let fn_did = self.tcx.hir().expect_item(imp).expect_impl().items[0]
                    .trait_item_def_id
                    .unwrap();
                self.local_trait_impls.insert(kind, (*trait_id, fn_did));
            }
        }
    }

    pub fn find_local_trait_impls(
        &self,
        trait_kind: TraitKind,
    ) -> Option<TraitImpls<'_>> {
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

    pub fn node_type(&self, hir_id: HirId) -> Ty<'tcx> {
        let owner_id = hir_id.owner;
        let typeck_res = self.tcx.typeck(owner_id);
        typeck_res.node_type(hir_id)
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

    fn ast_ty_to_ty(&self, fn_id: LocalDefId, ty: &HirTy<'tcx>) -> Ty<'tcx> {
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
        let inputs_len = self.group_list.len(inputs);
        let dummy_inputs_len = self
            .net_list
            .dummy_inputs_len(closure)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedClosure, span))?;

        assert_eq!(inputs_len, dummy_inputs_len);

        self.group_list
            .deep_iter::<Error, _>(inputs, &mut |ind, node_id| {
                let dummy_input =
                    self.net_list.dummy_input(closure, ind).ok_or_else(|| {
                        SpanError::new(SpanErrorKind::ExpectedClosure, span)
                    })?;

                let node_out = self.net_list[node_id].outputs().only_one();
                let dummy_out = self.net_list[dummy_input].outputs().only_one();

                let pass = PassNode::new(
                    node_out.out.ty,
                    node_out.node_out_id(node_id),
                    dummy_out.out.sym,
                );

                self.net_list.replace(dummy_input, pass);

                Ok(())
            })
    }

    pub fn subst_with(
        &self,
        subst: GenericArgsRef<'tcx>,
        new_subst: GenericArgsRef<'tcx>,
    ) -> GenericArgsRef<'tcx> {
        EarlyBinder::bind(subst).instantiate(self.tcx, new_subst)
    }
}
