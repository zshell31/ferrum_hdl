use std::{env, fs, path::Path as StdPath};

use ferrum::prim_ty::PrimTy;
use ferrum_netlist::{
    backend::Verilog,
    module::{Module, ModuleId, ModuleList},
    net_list::NodeId,
    node::{
        BinOp, BinOpNode, BitNotNode, ConstNode, InputNode, ModInst, Mux2Node, Node,
        NotNode,
    },
};
use rustc_const_eval::interpret::{ConstValue, Scalar};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def::Res, def_id::DefId, AnonConst, BinOpKind, BodyId, ConstArg, Expr, ExprKind,
    FnDecl, FnSig, GenericArg as HirGenArg, Generics as HirGenerics, HirId, Item, ItemId,
    ItemKind, Param, Path, QPath, StmtKind, Ty as HirTy, TyKind as HirTyKind, UnOp,
    WherePredicate,
};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::ty::{EarlyBinder, GenericArg, List, Ty, TyCtxt, UnevaluatedConst};
use rustc_session::EarlyErrorHandler;
use rustc_span::{symbol::Ident, Span};
use rustc_type_ir::{
    ConstKind,
    TyKind::{self},
    UintTy,
};

use crate::{
    blackbox::{self, Blackbox, ItemPath},
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
    pub fn def_id(&self) -> Option<DefId> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Generic<'tcx> {
    Ty(TyOrDefId<'tcx>),
    Const(u8),
}

impl<'tcx> Generic<'tcx> {
    fn const_eval(unevaluated: UnevaluatedConst<'tcx>, tcx: TyCtxt<'tcx>) -> Option<u8> {
        let param_env = tcx.param_env(unevaluated.def);
        let value = tcx
            .const_eval_resolve(param_env, unevaluated.expand(), None)
            .ok()?;
        match value {
            ConstValue::Scalar(Scalar::Int(scalar)) => scalar.try_to_u8().ok(),
            _ => None,
        }
    }

    fn from_hir_gen_arg(
        arg: &HirGenArg,
        tcx: TyCtxt<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
    ) -> Option<Self> {
        match arg {
            HirGenArg::Type(ty) => utils::def_id_for_hir_ty(ty)
                .map(Into::into)
                .map(Generic::Ty),
            HirGenArg::Const(ConstArg {
                value: AnonConst { def_id, .. },
                ..
            }) => {
                let generics = generics.as_ref()?;
                let unevaluated = UnevaluatedConst::new(def_id.to_def_id(), generics);
                Self::const_eval(unevaluated, tcx).map(Generic::Const)
            }
            _ => None,
        }
    }

    fn from_gen_arg(arg: &GenericArg<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Self> {
        if let Some(ty) = arg.as_type() {
            return Some(Generic::Ty(ty.into()));
        }

        if let Some(cons) = arg.as_const() {
            match cons.kind() {
                ConstKind::Unevaluated(unevaluated) => {
                    Self::const_eval(unevaluated, tcx).map(Generic::Const)
                }
                ConstKind::Value(val_tree) => val_tree
                    .try_to_scalar_int()
                    .and_then(|scalar| scalar.try_to_u8().ok())
                    .map(Generic::Const),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Generics<'tcx> {
    G1(Generic<'tcx>),
    G2(Generic<'tcx>, Generic<'tcx>),
    G3(Generic<'tcx>, Generic<'tcx>, Generic<'tcx>),
}

impl<'tcx> Generics<'tcx> {
    fn from_ty(
        ty: &Ty<'tcx>,
        tcx: TyCtxt<'tcx>,
        generics: Option<&List<GenericArg<'tcx>>>,
    ) -> Option<Self> {
        let ty = match generics {
            Some(generics) => EarlyBinder::bind(*ty).instantiate(tcx, generics),
            None => *ty,
        };
        let res = match ty.kind() {
            TyKind::Adt(_, generics) => match generics.as_slice() {
                [gen] => Some(Self::G1(Generic::from_gen_arg(gen, tcx)?)),
                [gen1, gen2] => Some(Self::G2(
                    Generic::from_gen_arg(gen1, tcx)?,
                    Generic::from_gen_arg(gen2, tcx)?,
                )),
                [gen1, gen2, gen3] => Some(Self::G3(
                    Generic::from_gen_arg(gen1, tcx)?,
                    Generic::from_gen_arg(gen2, tcx)?,
                    Generic::from_gen_arg(gen3, tcx)?,
                )),
                _ => None,
            },
            _ => None,
        };

        res
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key<'tcx> {
    pub ty_or_def_id: TyOrDefId<'tcx>,
    pub generics: Option<Generics<'tcx>>,
}

impl<'tcx> Key<'tcx> {
    fn def_id(&self) -> Option<DefId> {
        self.ty_or_def_id.def_id()
    }

    fn as_string(&self, tcx: TyCtxt<'tcx>) -> String {
        self.ty_or_def_id.as_string(tcx)
    }
}

pub trait AsKey<'tcx> {
    fn as_key(
        &self,
        tcx: TyCtxt<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
    ) -> Key<'tcx>;
}

impl<'tcx> AsKey<'tcx> for Path<'tcx> {
    fn as_key(
        &self,
        tcx: TyCtxt<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
    ) -> Key<'tcx> {
        let def_id = self.res.def_id();

        let generics = self
            .segments
            .last()
            .map(|segment| segment.args())
            .map(|generics| generics.args)
            .filter(|args| !args.is_empty())
            .and_then(|args| match args {
                [arg] => {
                    Some(Generics::G1(Generic::from_hir_gen_arg(arg, tcx, generics)?))
                }
                [arg1, arg2] => Some(Generics::G2(
                    Generic::from_hir_gen_arg(arg1, tcx, generics)?,
                    Generic::from_hir_gen_arg(arg2, tcx, generics)?,
                )),
                [arg1, arg2, arg3] => Some(Generics::G3(
                    Generic::from_hir_gen_arg(arg1, tcx, generics)?,
                    Generic::from_hir_gen_arg(arg2, tcx, generics)?,
                    Generic::from_hir_gen_arg(arg3, tcx, generics)?,
                )),
                _ => None,
            });

        Key {
            ty_or_def_id: def_id.into(),
            generics,
        }
    }
}

impl<'tcx> AsKey<'tcx> for Ty<'tcx> {
    fn as_key(
        &self,
        tcx: TyCtxt<'tcx>,
        generics: Option<&List<GenericArg<'tcx>>>,
    ) -> Key<'tcx> {
        let generics = Generics::from_ty(self, tcx, generics);

        Key {
            ty_or_def_id: (*self).into(),
            generics,
        }
    }
}

impl<'tcx> AsKey<'tcx> for DefId {
    fn as_key(&self, _: TyCtxt<'tcx>, _: Option<&List<GenericArg<'tcx>>>) -> Key<'tcx> {
        Key {
            ty_or_def_id: (*self).into(),
            generics: None,
        }
    }
}

pub struct Generator<'tcx> {
    tcx: TyCtxt<'tcx>,
    top_module: ItemId,
    blackbox: FxHashMap<Key<'tcx>, Option<Blackbox>>,
    prim_ty: FxHashMap<Key<'tcx>, Option<PrimTy>>,
    modules: ModuleList,
    pub idents: Idents,
}

impl<'tcx> Generator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, top_module: ItemId) -> Self {
        Self {
            tcx,
            top_module,
            blackbox: FxHashMap::default(),
            prim_ty: FxHashMap::default(),
            modules: ModuleList::new(),
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
        let name = env::var("CARGO_PKG_NAME").unwrap();

        let path = StdPath::new(&dir).join("generated").join("verilog");
        fs::create_dir_all(&path)?;

        let mut path = path.join(name);
        path.set_extension("v");

        let item = self.tcx.hir().item(self.top_module);
        self.evaluate_item(item, None)?;

        let verilog = Verilog::new().generate(&self.modules);

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

    pub fn find_blackbox<K: AsKey<'tcx>>(
        &mut self,
        key: &K,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Blackbox, Error> {
        let key = key.as_key(self.tcx, generics);

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

    pub fn find_prim_ty<K: AsKey<'tcx>>(
        &mut self,
        key: &K,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<PrimTy, Error> {
        let key = key.as_key(self.tcx, generics);

        // TODO: check crate
        #[allow(clippy::map_entry)]
        if !self.prim_ty.contains_key(&key) {
            let mut prim_ty = None;

            if let TyOrDefId::Ty(ty) = key.ty_or_def_id {
                match ty.kind() {
                    TyKind::Bool => {
                        prim_ty = Some(PrimTy::Bool);
                    }
                    TyKind::Uint(UintTy::U128) => {
                        prim_ty = Some(PrimTy::U128);
                    }
                    _ => {}
                }
            }

            if let Some(def_id) = key.def_id() {
                let def_path = self.tcx.def_path(def_id);
                prim_ty = blackbox::find_prim_ty(&key, &def_path);
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

    pub fn node_id_for_ident(&self, ident: Ident) -> Result<NodeId, Error> {
        self.idents
            .node_index(ident)
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::MissingNodeForIdent(ident), ident.span)
            })
            .map_err(Into::into)
    }

    fn evaluate_item(
        &mut self,
        item: &Item<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
    ) -> Result<Option<ModuleId>, Error> {
        if let ItemKind::Fn(FnSig { decl, .. }, hir_generics, body_id) = item.kind {
            self.evaluate_generics(hir_generics)?;

            return self
                .evaluate_fn(item.ident, decl, body_id, generics)
                .map(Some);
        }

        Ok(None)
    }

    fn evaluate_generics(&mut self, generics: &HirGenerics<'tcx>) -> Result<(), Error> {
        let make_err =
            |span| Error::from(SpanError::new(SpanErrorKind::UnsupportedGeneric, span));

        let mut params: FxHashMap<DefId, Span> = generics
            .params
            .iter()
            .map(|param| (param.def_id.to_def_id(), param.span))
            .collect();

        for predicate in generics.predicates {
            match predicate {
                WherePredicate::BoundPredicate(predicate) => {
                    let (def_id, _) = predicate
                        .bounded_ty
                        .as_generic_param()
                        .ok_or_else(|| make_err(predicate.span))?;

                    let (_, span) = params
                        .remove_entry(&def_id)
                        .ok_or_else(|| make_err(predicate.span))?;

                    let mut found_signal = false;
                    for bound in predicate.bounds {
                        let trait_ref =
                            bound.trait_ref().ok_or_else(|| make_err(span))?;
                        let trait_def_id = trait_ref
                            .path
                            .res
                            .opt_def_id()
                            .ok_or_else(|| make_err(span))?;

                        // TODO: move into blackbox
                        if self.tcx.def_path(trait_def_id)
                            == ItemPath(&["signal", "Signal"])
                        {
                            let arg = trait_ref.path.segments[0]
                                .args
                                .ok_or_else(|| make_err(span))?;

                            let ty = arg.bindings[0].ty();
                            match ty.kind {
                                HirTyKind::Path(QPath::Resolved(_, path)) => {
                                    let prim_ty = self.find_prim_ty(path, None, span)?;
                                    let key = def_id.as_key(self.tcx, None);
                                    self.prim_ty.insert(key, Some(prim_ty));

                                    found_signal = true;
                                }
                                _ => return Err(make_err(span)),
                            }
                            break;
                        }
                    }

                    if !found_signal {
                        return Err(make_err(span));
                    }
                }
                _ => {
                    return Err(make_err(predicate.span()));
                }
            }
        }

        if !params.is_empty() {
            let span = params.values().next().unwrap();
            return Err(make_err(*span));
        }

        Ok(())
    }

    fn evaluate_fn(
        &mut self,
        name: Ident,
        fn_decl: &FnDecl<'tcx>,
        body_id: BodyId,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
    ) -> Result<ModuleId, Error> {
        let module_sym = self.idents.module(name);
        let module_id = self.modules.add_module(module_sym);
        let mut module = Module::new(module_id, module_sym);

        self.idents.push_scope();

        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());
        self.evaluate_inputs(inputs, generics, &mut module, false)?;

        let node_idx = self.evaluate_expr(body.value, generics, &mut module)?;

        // evaluate output
        let sym = self.idents.out();
        let node_out = module.net_list.node_output_mut(node_idx);
        node_out.sym = sym;
        module.net_list.add_output_node(node_idx);

        self.idents.pop_scope();

        self.modules.replace(module_id, module);

        Ok(module_id)
    }

    fn make_input(
        &mut self,
        ident: Ident,
        prim_ty: PrimTy,
        module: &mut Module,
        is_dummy: bool,
    ) {
        let sym = self.idents.ident(ident);

        let input = InputNode::new(prim_ty, sym);
        let input = if is_dummy {
            module.net_list.add_dummy_node(input)
        } else {
            module.net_list.add_node(input)
        };

        self.idents.add_local_ident(ident, input);
    }

    fn evaluate_inputs<'a>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        module: &mut Module,
        is_dummy: bool,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            // println!("{:#?}", input);
            // println!("{:#?}", param);
            match input.kind {
                HirTyKind::Path(QPath::Resolved(_, path)) => {
                    let ident = utils::pat_ident(param.pat)?;
                    let prim_ty = self.find_prim_ty(path, generics, path.span)?;

                    self.make_input(ident, prim_ty, module, is_dummy);
                }
                HirTyKind::Infer => {
                    let ident = utils::pat_ident(param.pat)?;
                    let prim_ty = self.find_prim_ty(
                        &self.node_type(param.hir_id),
                        generics,
                        param.span,
                    )?;

                    self.make_input(ident, prim_ty, module, is_dummy);
                }
                _ => {
                    println!("input: {:#?}", input);
                    println!("param: {:#?}", param);
                    return Err(
                        SpanError::new(SpanErrorKind::NotSynthInput, input.span).into()
                    );
                }
            };
        }

        Ok(())
    }

    pub fn evaluate_expr(
        &mut self,
        expr: &Expr<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        let ty = self.node_type(expr.hir_id);

        match expr.kind {
            ExprKind::Binary(bin_op, lhs, rhs) => {
                println!("binary");
                let prim_ty = self.find_prim_ty(&ty, generics, expr.span)?;

                let bin_op = match bin_op.node {
                    BinOpKind::BitAnd => BinOp::BitAnd,
                    BinOpKind::BitOr => BinOp::BitOr,
                    BinOpKind::BitXor => BinOp::BitXor,
                    BinOpKind::And => BinOp::And,
                    BinOpKind::Or => BinOp::Or,
                    BinOpKind::Add => BinOp::Add,
                    BinOpKind::Sub => BinOp::Sub,
                    _ => {
                        return Err(SpanError::new(
                            SpanErrorKind::UnsupportedBinOp(bin_op.node),
                            bin_op.span,
                        )
                        .into());
                    }
                };

                let lhs = self.evaluate_expr(lhs, generics, module)?;
                if let Node::Const(node) = module.net_list.node_mut(lhs) {
                    node.out.ty = prim_ty;
                    node.inject = true;
                }

                let rhs = self.evaluate_expr(rhs, generics, module)?;
                if let Node::Const(node) = module.net_list.node_mut(rhs) {
                    node.out.ty = prim_ty;
                    node.inject = true;
                }

                Ok(module.net_list.add_node(BinOpNode::new(
                    prim_ty,
                    bin_op,
                    lhs,
                    rhs,
                    self.idents.tmp(),
                )))
            }
            ExprKind::Block(block, _) => {
                println!("block");
                self.idents.push_scope();

                for stmt in block.stmts {
                    match stmt.kind {
                        StmtKind::Local(local) if local.els.is_none() => {
                            let ident = utils::pat_ident(local.pat)?;
                            let sym = self.idents.ident(ident);

                            let init = local.init.ok_or_else(|| {
                                SpanError::new(SpanErrorKind::ExpectedExpr, local.span)
                            })?;

                            let node_idx = self.evaluate_expr(init, generics, module)?;
                            let node_out = module.net_list.node_output_mut(node_idx);
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

                let node_idx = self.evaluate_expr(expr, generics, module)?;

                self.idents.pop_scope();

                Ok(node_idx)
            }
            ExprKind::Call(rec, args) => {
                println!("call");
                if let ExprKind::Path(path) = rec.kind {
                    match path {
                        QPath::Resolved(_, Path { span, res, .. }) => {
                            let def_id = res.def_id();
                            if def_id.is_local() {
                                let ty = self.node_type(rec.hir_id);
                                let generics = self.generics(&ty);
                                let item =
                                    self.tcx.hir().expect_item(def_id.expect_local());
                                let module_id =
                                    self.evaluate_item(item, generics)?.unwrap();

                                let prim_ty = {
                                    let module = self.modules.module(module_id);
                                    module
                                        .net_list
                                        .node_output(
                                            module
                                                .net_list
                                                .outputs
                                                .first()
                                                .copied()
                                                .unwrap(),
                                        )
                                        .ty
                                };

                                let inputs = args
                                    .iter()
                                    .map(|arg| self.evaluate_expr(arg, generics, module))
                                    .collect::<Result<Vec<_>, Error>>()?;

                                Ok(module.net_list.add_node(ModInst::new(
                                    prim_ty,
                                    self.idents.inst(),
                                    module_id,
                                    inputs,
                                    self.idents.tmp(),
                                )))
                            } else {
                                let blackbox =
                                    self.find_blackbox(&def_id, generics, *span)?;
                                blackbox.evaluate_expr(self, generics, expr, module)
                            }
                        }
                        QPath::TypeRelative(ty, _) => {
                            let res = self
                                .tcx
                                .typeck(rec.hir_id.owner)
                                .qpath_res(&path, rec.hir_id);
                            let def_id = res.def_id();
                            let blackbox =
                                self.find_blackbox(&def_id, generics, ty.span)?;
                            blackbox.evaluate_expr(self, generics, expr, module)
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
                println!("closure");
                let body = self.tcx.hir().body(closure.body);
                let inputs = closure.fn_decl.inputs.iter().zip(body.params.iter());
                self.evaluate_inputs(inputs, generics, module, true)?;

                self.evaluate_expr(body.value, generics, module)
            }
            ExprKind::DropTemps(inner) => self.evaluate_expr(inner, generics, module),
            ExprKind::If(cond, if_block, else_block) => {
                println!("if");
                let prim_ty = self.find_prim_ty(&ty, generics, expr.span)?;

                let else_block = else_block.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedIfElseExpr, expr.span)
                })?;

                let cond = self.evaluate_expr(cond, generics, module)?;
                let if_block = self.evaluate_expr(if_block, generics, module)?;
                let else_block = self.evaluate_expr(else_block, generics, module)?;

                Ok(module.net_list.add_node(Mux2Node::new(
                    prim_ty,
                    cond,
                    if_block,
                    else_block,
                    self.idents.tmp(),
                )))
            }
            ExprKind::Lit(lit) => {
                println!("lit");
                let prim_ty = self.find_prim_ty(&ty, generics, lit.span)?;
                let value = blackbox::evaluate_lit(prim_ty, lit)?;

                Ok(module.net_list.add_node(ConstNode::new(
                    prim_ty,
                    value,
                    self.idents.tmp(),
                )))
            }
            ExprKind::MethodCall(_, _, _, span) => {
                println!("method call");
                let def_id = self.type_dependent_def_id(expr.hir_id, span)?;
                let blackbox = self.find_blackbox(&def_id, generics, span)?;
                blackbox.evaluate_expr(self, generics, expr, module)
            }
            ExprKind::Path(QPath::Resolved(
                _,
                Path {
                    res: Res::Local(_),
                    segments,
                    ..
                },
            )) if segments.len() == 1 => {
                println!("path");

                self.node_id_for_ident(segments[0].ident)
            }
            ExprKind::Unary(UnOp::Not, inner) => {
                println!("unary");

                let comb = self.evaluate_expr(inner, generics, module)?;
                let prim_ty = self.find_prim_ty(&ty, generics, expr.span)?;
                let sym = self.idents.tmp();

                Ok(if prim_ty.is_bool() {
                    module.net_list.add_node(NotNode::new(prim_ty, comb, sym))
                } else {
                    module
                        .net_list
                        .add_node(BitNotNode::new(prim_ty, comb, sym))
                })
            }
            _ => {
                println!("{:#?}", expr);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
            }
        }
    }
}
