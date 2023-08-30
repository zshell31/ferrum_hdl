use std::{env, fs, path::Path as StdPath};

use ferrum::prim_ty::PrimTy;
use ferrum_netlist::{
    backend::Verilog,
    group_list::{Group, GroupList, ItemId},
    inject_pass::InjectPass,
    net_list::{ModuleId, NetList, NodeId},
    node::{
        BinOp, BinOpNode, BitNotNode, ConstNode, InputNode, IsNode, ModInst, Mux2Node,
        Node, NotNode, PassNode,
    },
    params::Outputs,
};
use rustc_const_eval::interpret::{ConstValue, Scalar};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def::Res, def_id::DefId, AnonConst, BinOpKind, BodyId, ConstArg, Expr, ExprKind,
    FnDecl, FnSig, GenericArg as HirGenArg, Generics as HirGenerics, HirId, Item,
    ItemId as HirItemId, ItemKind, Param, Pat, PatKind, Path, QPath, StmtKind,
    Ty as HirTy, TyKind as HirTyKind, UnOp, WherePredicate,
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
    blackbox::{self, Blackbox, EvaluateExpr, ItemPath},
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
    fn eval_const_val(value: ConstValue) -> Option<u8> {
        match value {
            ConstValue::Scalar(Scalar::Int(scalar)) => scalar.try_to_u8().ok(),
            _ => None,
        }
    }
    fn resolve_const(
        unevaluated: UnevaluatedConst<'tcx>,
        tcx: TyCtxt<'tcx>,
    ) -> Option<u8> {
        let param_env = tcx.param_env(unevaluated.def);
        let value = tcx
            .const_eval_resolve(param_env, unevaluated.expand(), None)
            .ok()?;

        Self::eval_const_val(value)
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
            }) => match generics.as_ref() {
                Some(generics) => {
                    let unevaluated = UnevaluatedConst::new(def_id.to_def_id(), generics);
                    Self::resolve_const(unevaluated, tcx).map(Generic::Const)
                }
                None => {
                    let value = tcx.const_eval_poly(def_id.to_def_id()).ok()?;
                    Self::eval_const_val(value).map(Generic::Const)
                }
            },
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
                    Self::resolve_const(unevaluated, tcx).map(Generic::Const)
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
    prim_ty: FxHashMap<Key<'tcx>, Option<PrimTy>>,
    pub net_list: NetList,
    pub group_list: GroupList,
    pub idents: Idents,
}

impl<'tcx> Generator<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, top_module: HirItemId) -> Self {
        Self {
            tcx,
            top_module,
            blackbox: FxHashMap::default(),
            prim_ty: FxHashMap::default(),
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

    pub fn item_id_for_ident(&self, ident: Ident) -> Result<ItemId, Error> {
        self.idents
            .item_id(ident)
            .ok_or_else(|| {
                SpanError::new(SpanErrorKind::MissingNodeForIdent(ident), ident.span)
            })
            .map_err(Into::into)
    }

    fn evaluate_item(
        &mut self,
        item: &Item<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        is_top_module: bool,
    ) -> Result<Option<ModuleId>, Error> {
        if let ItemKind::Fn(FnSig { decl, .. }, hir_generics, body_id) = item.kind {
            // ignore unsupported generics if current item is not top module
            self.evaluate_generics(hir_generics, !is_top_module)?;

            return self
                .evaluate_fn(item.ident, decl, body_id, generics)
                .map(Some);
        }

        Ok(None)
    }

    fn evaluate_generics(
        &mut self,
        generics: &HirGenerics<'tcx>,
        ignore: bool,
    ) -> Result<(), Error> {
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

                    if !ignore && !found_signal {
                        return Err(make_err(span));
                    }
                }
                _ => {
                    if !ignore {
                        return Err(make_err(predicate.span()));
                    }
                }
            }
        }

        if !ignore && !params.is_empty() {
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
        let module_id = self.net_list.add_module(module_sym);

        self.idents.push_scope();

        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());
        let ctx = EvalContext::new(generics, module_id);

        self.evaluate_inputs(inputs, ctx, false)?;
        let item_id = self.evaluate_expr(body.value, ctx)?;
        self.evaluate_outputs(item_id)?;

        self.idents.pop_scope();

        Ok(module_id)
    }

    fn evaluate_inputs<'a>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        ctx: EvalContext<'tcx>,
        is_dummy: bool,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            let ident = match self.param_ident(param.pat, is_dummy)? {
                Some(ident) => ident,
                None => continue,
            };

            let item_id = self.make_input(ident, input, ctx, is_dummy)?;

            self.idents.add_local_ident(ident, item_id);
        }

        Ok(())
    }

    fn param_ident(
        &self,
        param: &Pat<'tcx>,
        is_dummy: bool,
    ) -> Result<Option<Ident>, Error> {
        if is_dummy {
            match utils::pat_ident(param) {
                Ok(ident) => Ok(Some(ident)),
                Err(_) => Ok(None),
            }
        } else {
            utils::pat_ident(param).map(Some)
        }
    }

    fn make_input(
        &mut self,
        ident: Ident,
        input: &HirTy<'tcx>,
        ctx: EvalContext<'tcx>,
        is_dummy: bool,
    ) -> Result<ItemId, Error> {
        match input.kind {
            HirTyKind::Infer => {
                let prim_ty = self.find_prim_ty(
                    &self.node_type(input.hir_id),
                    ctx.generics,
                    input.span,
                )?;

                Ok(self.make_input_with_prim_ty(ident, prim_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Path(QPath::Resolved(_, path)) => {
                let prim_ty = self.find_prim_ty(path, ctx.generics, path.span)?;

                Ok(self.make_input_with_prim_ty(ident, prim_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Tup(ty) => {
                let group = ty
                    .iter()
                    .map(|ty| self.make_input(ident, ty, ctx, is_dummy))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(self.group_list.add_group(Group::new(group)).into())
            }
            _ => {
                println!("input: {:#?}", input);
                Err(SpanError::new(SpanErrorKind::NotSynthInput, input.span).into())
            }
        }
    }

    fn make_input_with_prim_ty(
        &mut self,
        ident: Ident,
        prim_ty: PrimTy,
        module_id: ModuleId,
        is_dummy: bool,
    ) -> ItemId {
        let sym = self.idents.ident(ident);
        let input = InputNode::new(prim_ty, sym);

        (if is_dummy {
            self.net_list.add_dummy_node(module_id, input)
        } else {
            self.net_list.add_node(module_id, input)
        })
        .into()
    }

    fn evaluate_outputs(&mut self, item_id: ItemId) -> Result<(), Error> {
        let mut evaluate = |node_id: NodeId| {
            let node = &mut self.net_list[node_id];
            for out in node.outputs_mut().items_mut() {
                let sym = self.idents.out();
                out.out.sym = sym;
            }
            self.net_list.add_all_outputs(node_id);

            Ok(())
        };

        self.group_list
            .deep_iter::<Error, _>(item_id, &mut evaluate)
    }

    pub fn evaluate_expr(
        &mut self,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let ty = self.node_type(expr.hir_id);

        match expr.kind {
            ExprKind::Binary(bin_op, lhs, rhs) => {
                println!("binary");
                let prim_ty = self.find_prim_ty(&ty, ctx.generics, expr.span)?;

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

                let lhs = self.evaluate_expr(lhs, ctx)?.node_id();
                if let Node::Const(node) = &mut self.net_list[lhs] {
                    node.output.ty = prim_ty;
                    node.inject = true;
                }

                let rhs = self.evaluate_expr(rhs, ctx)?.node_id();
                if let Node::Const(node) = &mut self.net_list[rhs] {
                    node.output.ty = prim_ty;
                    node.inject = true;
                }

                let lhs = self.net_list.only_one_node_out_id(lhs);
                let rhs = self.net_list.only_one_node_out_id(rhs);

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        BinOpNode::new(prim_ty, bin_op, lhs, rhs, self.idents.tmp()),
                    )
                    .into())
            }
            ExprKind::Block(block, _) => {
                println!("block");
                self.idents.push_scope();

                for stmt in block.stmts {
                    match stmt.kind {
                        StmtKind::Local(local) if local.els.is_none() => {
                            let init = local.init.ok_or_else(|| {
                                SpanError::new(SpanErrorKind::ExpectedExpr, local.span)
                            })?;
                            let item_id = self.evaluate_expr(init, ctx)?;

                            let item_id = match item_id {
                                ItemId::Node(node_id) => {
                                    let outputs_len =
                                        self.net_list[node_id].outputs().len();

                                    if outputs_len > 1 {
                                        // TODO: how to avoid allocating vec
                                        let nodes = self.net_list[node_id]
                                            .outputs()
                                            .items()
                                            .map(|out| {
                                                PassNode::new(
                                                    out.out.ty,
                                                    out.node_out_id(node_id),
                                                    self.idents.tmp(),
                                                )
                                            })
                                            .collect::<Vec<_>>();

                                        let group = nodes
                                            .into_iter()
                                            .map(|node| {
                                                self.net_list
                                                    .add_node(ctx.module_id, node)
                                                    .into()
                                            })
                                            .collect::<Vec<_>>();

                                        self.group_list
                                            .add_group(Group::new(group))
                                            .into()
                                    } else {
                                        item_id
                                    }
                                }
                                ItemId::Group(_) => item_id,
                            };

                            match local.pat.kind {
                                PatKind::Binding(..) => {
                                    let ident = utils::pat_ident(local.pat)?;

                                    if let ItemId::Node(node_id) = item_id {
                                        let out = self.net_list[node_id]
                                            .outputs_mut()
                                            .only_one_mut();
                                        out.out.sym = self.idents.ident(ident);
                                    }

                                    self.idents.add_local_ident(ident, item_id)
                                }
                                PatKind::Tuple(pats, dot_dot_pos)
                                    if dot_dot_pos.as_opt_usize().is_none() =>
                                {
                                    let group_id = item_id.group_id();
                                    self.group_list.shadow_iter::<Error>(
                                        group_id,
                                        |ind, item_id| {
                                            if let Some(ident) =
                                                utils::pat_idents(pats, ind)?
                                            {
                                                if let ItemId::Node(node_id) = item_id {
                                                    let out = self.net_list[node_id]
                                                        .outputs_mut()
                                                        .only_one_mut();
                                                    out.out.sym =
                                                        self.idents.ident(ident);
                                                }
                                                self.idents
                                                    .add_local_ident(ident, item_id);
                                            }

                                            Ok(())
                                        },
                                    )?;
                                }
                                _ => {
                                    return Err(SpanError::new(
                                        SpanErrorKind::NotSynthExpr,
                                        local.span,
                                    )
                                    .into());
                                }
                            };
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

                let node_id = self.evaluate_expr(expr, ctx)?;

                self.idents.pop_scope();

                Ok(node_id)
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
                                    self.evaluate_item(item, generics, false)?.unwrap();

                                let mut inputs = Vec::new();

                                for arg in args {
                                    let item_id =
                                        self.evaluate_expr(arg, EvalContext {
                                            generics,
                                            module_id: ctx.module_id,
                                        })?;

                                    let mut evaluate = |node_id: NodeId| {
                                        for out in
                                            self.net_list[node_id].outputs().items()
                                        {
                                            inputs.push(out.node_out_id(node_id));
                                        }

                                        Result::<(), Error>::Ok(())
                                    };

                                    self.group_list.deep_iter(item_id, &mut evaluate)?;
                                }

                                let outputs = self.net_list[module_id]
                                    .outputs()
                                    .map(|node_out_id| {
                                        (self.net_list[node_out_id].ty, self.idents.tmp())
                                    })
                                    .collect::<Vec<_>>();

                                let inst_sym =
                                    self.idents.inst(self.net_list[module_id].name);

                                Ok(self
                                    .net_list
                                    .add_node(
                                        ctx.module_id,
                                        ModInst::new(
                                            inst_sym, module_id, inputs, outputs,
                                        ),
                                    )
                                    .into())
                            } else {
                                let blackbox =
                                    self.find_blackbox(&def_id, ctx.generics, *span)?;
                                blackbox.evaluate_expr(self, expr, ctx)
                            }
                        }
                        QPath::TypeRelative(ty, _) => {
                            let res = self
                                .tcx
                                .typeck(rec.hir_id.owner)
                                .qpath_res(&path, rec.hir_id);
                            let def_id = res.def_id();

                            let blackbox =
                                self.find_blackbox(&def_id, ctx.generics, ty.span)?;
                            blackbox.evaluate_expr(self, expr, ctx)
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

                self.evaluate_inputs(inputs, ctx, true)?;
                self.evaluate_expr(body.value, ctx)
            }
            ExprKind::DropTemps(inner) => self.evaluate_expr(inner, ctx),
            ExprKind::Field(expr, ident) => {
                let group_id = self.evaluate_expr(expr, ctx)?;
                let group_id = group_id.group_id();

                self.group_list[group_id]
                    .by_field(ident.as_str())
                    .ok_or_else(|| {
                        SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
                    })
            }
            ExprKind::If(cond, if_block, else_block) => {
                println!("if");
                let prim_ty = self.find_prim_ty(&ty, ctx.generics, expr.span)?;

                let else_block = else_block.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedIfElseExpr, expr.span)
                })?;

                let cond = self.evaluate_expr(cond, ctx)?.node_id();
                let if_block = self.evaluate_expr(if_block, ctx)?.node_id();
                let else_block = self.evaluate_expr(else_block, ctx)?.node_id();

                let cond = self.net_list.only_one_node_out_id(cond);
                let if_block = self.net_list.only_one_node_out_id(if_block);
                let else_block = self.net_list.only_one_node_out_id(else_block);

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        Mux2Node::new(
                            prim_ty,
                            cond,
                            if_block,
                            else_block,
                            self.idents.tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::Lit(lit) => {
                println!("lit");
                let prim_ty = self.find_prim_ty(&ty, ctx.generics, lit.span)?;
                let value = blackbox::evaluate_lit(prim_ty, lit)?;

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        ConstNode::new(prim_ty, value, self.idents.tmp()),
                    )
                    .into())
            }
            ExprKind::MethodCall(_, _, _, span) => {
                println!("method call");
                let def_id = self.type_dependent_def_id(expr.hir_id, span)?;
                let blackbox = self.find_blackbox(&def_id, ctx.generics, span)?;
                blackbox.evaluate_expr(self, expr, ctx)
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

                self.item_id_for_ident(segments[0].ident)
            }
            ExprKind::Tup(exprs) => {
                let groups = exprs
                    .iter()
                    .map(|expr| self.evaluate_expr(expr, ctx))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(self.group_list.add_group(Group::new(groups)).into())
            }
            ExprKind::Unary(UnOp::Not, inner) => {
                println!("unary");

                let comb = self.evaluate_expr(inner, ctx)?.node_id();
                let prim_ty = self.find_prim_ty(&ty, ctx.generics, expr.span)?;
                let sym = self.idents.tmp();

                let comb = self.net_list.only_one_node_out_id(comb);

                Ok((if prim_ty.is_bool() {
                    self.net_list
                        .add_node(ctx.module_id, NotNode::new(prim_ty, comb, sym))
                } else {
                    self.net_list
                        .add_node(ctx.module_id, BitNotNode::new(prim_ty, comb, sym))
                })
                .into())
            }
            _ => {
                println!("{:#?}", expr);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
            }
        }
    }
}
