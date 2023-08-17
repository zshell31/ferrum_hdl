use std::{env, fs, path::Path as StdPath};

use ferrum::prim_ty::PrimTy;
use ferrum_netlist::{
    index::NodeIndex,
    net_list::NetList,
    node::{
        BitAndComp, BitAndNode, BitNotComp, BitNotNode, BitOrComp, BitOrNode, Const,
        ConstNode, Input, InputNode, Mux2, Mux2Node, NotComp, NotNode,
    },
    symbol::Symbol,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def::Res,
    def_id::{DefId, LocalDefId},
    intravisit::{self, FnKind, Visitor},
    BinOpKind, BodyId, Expr, ExprKind, FnDecl, HirId, ItemId, ItemKind, Param, Path,
    QPath, StmtKind, Ty as HirTy, TyKind as HirTyKind, UnOp,
};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{
    hir::nested_filter::OnlyBodies,
    ty::{Ty, TyCtxt},
};
use rustc_session::EarlyErrorHandler;
use rustc_span::{symbol::Ident, Span};
use rustc_type_ir::TyKind;

use crate::{
    blackbox::{self, Blackbox},
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

fn find_top_module(tcx: TyCtxt<'_>) -> Result<ItemId, Error> {
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
    fn def_id(&self) -> Option<DefId> {
        match self {
            Self::Ty(ty) => match ty.kind() {
                TyKind::Adt(adt, _) => Some(adt.did()),
                _ => None,
            },

            Self::DefId(def_id) => Some(*def_id),
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

pub struct Generator<'tcx> {
    tcx: TyCtxt<'tcx>,
    top_module: ItemId,
    blackbox: FxHashMap<TyOrDefId<'tcx>, Option<Blackbox>>,
    prim_ty: FxHashMap<TyOrDefId<'tcx>, Option<PrimTy>>,
    pub net_list: NetList,
    pub idents: Idents,
}

impl<'tcx> Generator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, top_module: ItemId) -> Self {
        Self {
            tcx,
            top_module,
            blackbox: FxHashMap::default(),
            prim_ty: FxHashMap::default(),
            net_list: NetList::new(Symbol::new("top_module")),
            idents: Idents::new(),
        }
    }

    pub fn generate(&mut self) {
        let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let name = env::var("CARGO_PKG_NAME").unwrap();

        let mut path = StdPath::new(&dir)
            .join("generated")
            .join("verilog")
            .join(name);

        path.set_extension("v");

        if let Err(e) = self.generate_inner(path) {
            self.emit_err(e);
        }
    }

    fn generate_inner<P: AsRef<StdPath>>(&mut self, path: P) -> Result<(), Error> {
        let item = self.tcx.hir().item(self.top_module);
        self.visit_item(item);

        let verilog = self.net_list.verilog();
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

    pub fn generic_type(&self, ty: &Ty<'tcx>, index: usize) -> Option<Ty<'tcx>> {
        match ty.kind() {
            TyKind::FnDef(_, subst) => {
                subst.get(index).and_then(|generic| generic.as_type())
            }
            _ => None,
        }
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

    pub fn find_blackbox<K: Into<TyOrDefId<'tcx>>>(
        &mut self,
        key: K,
        span: Span,
    ) -> Result<Blackbox, Error> {
        let key = key.into();

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.blackbox.contains_key(&key) {
            let mut blackbox = None;

            if let Some(def_id) = key.def_id() {
                let def_path = self.tcx.def_path(def_id);
                blackbox = blackbox::find_blackbox(&def_path);
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
            .map_err(Into::into)
    }

    pub fn find_prim_ty<K: Into<TyOrDefId<'tcx>>>(
        &mut self,
        key: K,
        span: Span,
    ) -> Result<PrimTy, Error> {
        let key = key.into();

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.prim_ty.contains_key(&key) {
            let mut prim_ty = None;

            if let TyOrDefId::Ty(ty) = key {
                if let TyKind::Bool = ty.kind() {
                    prim_ty = Some(PrimTy::Bool);
                }
            }

            if let Some(def_id) = key.def_id() {
                let def_path = self.tcx.def_path(def_id);
                prim_ty = blackbox::find_prim_ty(&def_path);
            }

            self.prim_ty.insert(key, prim_ty);
        }

        self.prim_ty
            .get(&key)
            .unwrap()
            .ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::MissingPrimTy(key.as_string(self.tcx)),
                    span,
                )
            })
            .map_err(Into::into)
    }

    pub fn node_idx_for_ident(&self, ident: Ident) -> Result<NodeIndex, Error> {
        self.idents
            .node_index(ident)
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::MissingNodeForIdent(ident), ident.span)
            })
            .map_err(Into::into)
    }

    fn evaluate_fn(
        &mut self,
        fn_decl: &FnDecl<'tcx>,
        body_id: BodyId,
        _: Span,
    ) -> Result<(), Error> {
        self.idents.push_scope();

        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());
        self.evaluate_inputs(inputs, false)?;

        let node_idx = self.evaluate_expr(body.value)?;

        // evaluate output
        let sym = self.idents.out();
        let node_out = self.net_list.node_output_mut(node_idx);
        node_out.sym = sym;
        self.net_list.add_output_node(node_idx);

        self.idents.pop_scope();

        Ok(())
    }

    fn evaluate_inputs<'a>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        is_dummy: bool,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            if let HirTyKind::Path(QPath::Resolved(
                _,
                Path {
                    span,
                    res: Res::Def(_, def_id),
                    ..
                },
            )) = input.kind
            {
                let ident = utils::pat_ident(param.pat)?;
                let sym = Symbol::new(ident.as_str());
                let prim_ty = self.find_prim_ty(*def_id, *span)?;

                let input = InputNode::new(prim_ty, sym);
                let input = if is_dummy {
                    self.net_list.add_dummy_node(input)
                } else {
                    self.net_list.add_node::<Input>(input)
                };

                self.idents.add_local_ident(ident, input);
            } else {
                return Err(
                    SpanError::new(SpanErrorKind::NotSynthInput, input.span).into()
                );
            }
        }

        Ok(())
    }

    pub fn evaluate_expr(&mut self, expr: &Expr<'tcx>) -> Result<NodeIndex, Error> {
        let ty = self.node_type(expr.hir_id);

        match expr.kind {
            ExprKind::Binary(bin_op, lhs, rhs) => {
                let prim_ty = self.find_prim_ty(ty, expr.span)?;

                match bin_op.node {
                    BinOpKind::BitAnd => {
                        let lhs = self.evaluate_expr(lhs)?;
                        let rhs = self.evaluate_expr(rhs)?;
                        Ok(self.net_list.add_node::<BitAndComp>(BitAndNode::new(
                            prim_ty,
                            lhs,
                            rhs,
                            self.idents.tmp(),
                        )))
                    }
                    BinOpKind::BitOr => {
                        let lhs = self.evaluate_expr(lhs)?;
                        let rhs = self.evaluate_expr(rhs)?;
                        Ok(self.net_list.add_node::<BitOrComp>(BitOrNode::new(
                            prim_ty,
                            lhs,
                            rhs,
                            self.idents.tmp(),
                        )))
                    }
                    _ => {
                        Err(SpanError::new(SpanErrorKind::UnsupportedBinOp, bin_op.span)
                            .into())
                    }
                }
            }
            ExprKind::Block(block, _) => {
                self.idents.push_scope();

                for stmt in block.stmts {
                    match stmt.kind {
                        StmtKind::Local(local) if local.els.is_none() => {
                            let ident = utils::pat_ident(local.pat)?;
                            let sym = Symbol::new(ident.as_str());

                            let init = local.init.ok_or_else(|| {
                                SpanError::new(SpanErrorKind::ExpectedExpr, local.span)
                            })?;

                            let node_idx = self.evaluate_expr(init)?;
                            let node_out = self.net_list.node_output_mut(node_idx);
                            node_out.sym = sym;
                            self.idents.add_local_ident(ident, node_idx);
                        }
                        _ => {
                            return Err(SpanError::new(
                                SpanErrorKind::ExpectedLetBind,
                                stmt.span,
                            )
                            .into());
                        }
                    }
                }

                let expr = match block.expr {
                    Some(expr) => expr,
                    None => {
                        return Err(SpanError::new(
                            SpanErrorKind::ExpectedLastExpr,
                            block.span,
                        )
                        .into());
                    }
                };

                let node_idx = self.evaluate_expr(expr)?;

                self.idents.pop_scope();

                Ok(node_idx)
            }
            ExprKind::Call(rec, _) => {
                if let ExprKind::Path(path) = rec.kind {
                    match path {
                        QPath::Resolved(_, Path { span, res, .. }) => {
                            let def_id = res.def_id();
                            let blackbox = self.find_blackbox(def_id, *span)?;
                            blackbox.evaluate_expr(self, expr)
                        }
                        QPath::TypeRelative(ty, _) => {
                            let res = self
                                .tcx
                                .typeck(rec.hir_id.owner)
                                .qpath_res(&path, rec.hir_id);
                            let def_id = res.def_id();
                            let blackbox = self.find_blackbox(def_id, ty.span)?;
                            blackbox.evaluate_expr(self, expr)
                        }
                        _ => {
                            Err(SpanError::new(SpanErrorKind::NotSynthCall, rec.span)
                                .into())
                        }
                    }
                } else {
                    println!("{:#?}", expr);
                    Err(SpanError::new(SpanErrorKind::NotSynthCall, expr.span).into())
                }
            }
            ExprKind::Closure(closure) => {
                let body = self.tcx.hir().body(closure.body);
                let inputs = closure.fn_decl.inputs.iter().zip(body.params.iter());
                self.evaluate_inputs(inputs, true)?;

                self.evaluate_expr(body.value)
            }
            ExprKind::DropTemps(inner) => self.evaluate_expr(inner),
            ExprKind::If(cond, if_block, else_block) => {
                let prim_ty = self.find_prim_ty(ty, expr.span)?;

                let else_block = else_block.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedIfElseExpr, expr.span)
                })?;

                let cond = self.evaluate_expr(cond)?;
                let if_block = self.evaluate_expr(if_block)?;
                let else_block = self.evaluate_expr(else_block)?;

                Ok(self.net_list.add_node::<Mux2>(Mux2Node::new(
                    prim_ty,
                    cond,
                    if_block,
                    else_block,
                    self.idents.tmp(),
                )))
            }
            ExprKind::Lit(lit) => {
                let prim_ty = self.find_prim_ty(ty, lit.span)?;
                let value = blackbox::evaluate_lit(prim_ty, lit)?;

                Ok(self.net_list.add_node::<Const>(ConstNode::new(
                    prim_ty,
                    value,
                    self.idents.tmp(),
                )))
            }
            ExprKind::MethodCall(_, _, _, span) => {
                let def_id = self.type_dependent_def_id(expr.hir_id, span)?;
                let blackbox = self.find_blackbox(def_id, span)?;
                blackbox.evaluate_expr(self, expr)
            }
            ExprKind::Path(QPath::Resolved(
                _,
                Path {
                    res: Res::Local(_),
                    segments,
                    ..
                },
            )) if segments.len() == 1 => self.node_idx_for_ident(segments[0].ident),
            ExprKind::Unary(UnOp::Not, inner) => {
                let comb = self.evaluate_expr(inner)?;
                let prim_ty = self.find_prim_ty(ty, expr.span)?;
                let sym = self.idents.tmp();

                Ok(if prim_ty.is_bool() {
                    self.net_list
                        .add_node::<NotComp>(NotNode::new(prim_ty, comb, sym))
                } else {
                    self.net_list
                        .add_node::<BitNotComp>(BitNotNode::new(prim_ty, comb, sym))
                })
            }
            _ => {
                println!("{:#?}", expr);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
            }
        }
    }
}

impl<'tcx> Visitor<'tcx> for Generator<'tcx> {
    type NestedFilter = OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_fn(
        &mut self,
        fn_kind: FnKind<'tcx>,
        fn_decl: &'tcx FnDecl<'tcx>,
        body_id: BodyId,
        span: Span,
        def_id: LocalDefId,
    ) {
        if let FnKind::ItemFn(_, _, _) = fn_kind {
            if let Err(e) = self.evaluate_fn(fn_decl, body_id, span) {
                self.emit_err(e);
                return;
            }
        }
        intravisit::walk_fn(self, fn_kind, fn_decl, body_id, def_id);
        // println!("{:#?}", self.net_list);
    }
}
