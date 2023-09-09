pub mod expr;
pub mod func;
pub mod generic;
pub mod key;
pub mod struc;

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
use rustc_middle::ty::{GenericArg, List, Ty, TyCtxt};
use rustc_session::EarlyErrorHandler;
use rustc_span::{symbol::Ident, Span};
use rustc_type_ir::TyKind::{self};

use self::key::{AsKey, Key};
use crate::{
    blackbox::Blackbox,
    error::{Error, SpanError, SpanErrorKind},
    idents::Idents,
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
    pub generics: Option<&'tcx List<GenericArg<'tcx>>>,
    pub module_id: ModuleId,
}

impl<'tcx> EvalContext<'tcx> {
    fn new(generics: Option<&'tcx List<GenericArg<'tcx>>>, module_id: ModuleId) -> Self {
        Self {
            generics,
            module_id,
        }
    }
}

pub struct Generator<'tcx> {
    tcx: TyCtxt<'tcx>,
    top_module: HirItemId,
    blackbox: FxHashMap<Key<'tcx>, Option<Blackbox>>,
    sig_ty: FxHashMap<Key<'tcx>, Option<SignalTy>>,
    pub net_list: NetList,
    pub group_list: GroupList,
    pub idents: Idents,
}

impl<'tcx> !Sync for Generator<'tcx> {}
impl<'tcx> !Send for Generator<'tcx> {}

impl<'tcx> Generator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, top_module: HirItemId) -> Self {
        Self {
            tcx,
            top_module,
            blackbox: FxHashMap::default(),
            sig_ty: FxHashMap::default(),
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

        let item = self.tcx.hir().item(self.top_module);
        self.evaluate_item(item, None, true)?;

        InjectPass::new(&mut self.net_list).inject();

        let verilog = Verilog::new(&self.net_list).generate();

        Ok(fs::write(path, verilog)?)
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

    pub fn generics(&self, ty: &Ty<'tcx>) -> Option<&'tcx List<GenericArg<'tcx>>> {
        match ty.kind() {
            TyKind::FnDef(_, subst) => Some(subst),
            _ => None,
        }
    }

    pub fn generic_type(&self, ty: &Ty<'tcx>, index: usize) -> Option<Ty<'tcx>> {
        self.generics(ty)
            .and_then(|generics| generics.get(index))
            .and_then(|generic| generic.as_type())
    }

    pub fn type_dependent_def_id(
        &self,
        hir_id: HirId,
        span: Span,
    ) -> Result<DefId, Error> {
        self.tcx
            .typeck(hir_id.owner)
            .type_dependent_def_id(hir_id)
            .ok_or_else(|| SpanError::new(SpanErrorKind::MissingDefId, span))
            .map_err(Into::into)
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
        let item_ctx = &ItemCtxt::new(self.tcx, fn_id) as &dyn AstConv<'tcx>;
        item_ctx.ast_ty_to_ty(ty)
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
}
