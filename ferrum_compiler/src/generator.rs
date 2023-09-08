use std::{env, fmt::Debug, fs, iter, path::Path as StdPath};

use either::Either;
use ferrum_netlist::{
    arena::with_arena,
    backend::Verilog,
    group_list::{Group, GroupKind, GroupList, ItemId},
    inject_pass::InjectPass,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{
        BinOp, BinOpNode, BitNotNode, ConstNode, InputNode, IsNode, ModInst, Mux2Node,
        Node, NotNode, PassNode, Splitter,
    },
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_ast::{BorrowKind, LitKind, Mutability};
use rustc_const_eval::interpret::{ConstValue, Scalar};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def::Res,
    def_id::{DefId, LocalDefId},
    BinOpKind, BodyId, Expr, ExprKind, FnDecl, FnSig, Generics as HirGenerics, HirId,
    Item, ItemId as HirItemId, ItemKind, Param, Pat, PatKind, Path, QPath, StmtKind,
    Ty as HirTy, TyKind as HirTyKind, UnOp, WherePredicate,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::ty::{
    EarlyBinder, GenericArg, List, ScalarInt, Ty, TyCtxt, UnevaluatedConst,
};
use rustc_session::EarlyErrorHandler;
use rustc_span::{source_map::Spanned, symbol::Ident, Span};
use rustc_type_ir::{
    ConstKind,
    TyKind::{self},
    UintTy,
};

use crate::{
    bitvec::ArrayDesc,
    blackbox::{self, bit_vec_trans, BitVecTransArgs, Blackbox, EvaluateExpr, ItemPath},
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
pub enum Generic {
    Ty(SignalTy),
    Const(u128),
}

impl From<SignalTy> for Generic {
    fn from(sig_ty: SignalTy) -> Self {
        Self::Ty(sig_ty)
    }
}

impl From<u128> for Generic {
    fn from(cons: u128) -> Self {
        Self::Const(cons)
    }
}

impl Generic {
    pub fn as_ty(&self) -> Option<&SignalTy> {
        match self {
            Self::Ty(sig_ty) => Some(sig_ty),
            _ => None,
        }
    }

    pub fn as_const<T: TryFrom<u128>>(&self) -> Option<T> {
        match self {
            Self::Const(val) => T::try_from(*val).ok(),
            _ => None,
        }
    }

    fn eval_scalar_int(scalar: ScalarInt) -> Option<u128> {
        scalar
            .try_to_u128()
            .ok()
            .or_else(|| scalar.try_to_u64().ok().map(|n| n as u128))
            .or_else(|| scalar.try_to_u32().ok().map(|n| n as u128))
            .or_else(|| scalar.try_to_u16().ok().map(|n| n as u128))
            .or_else(|| scalar.try_to_u8().ok().map(|n| n as u128))
    }

    fn eval_const_val(value: ConstValue) -> Option<u128> {
        match value {
            ConstValue::Scalar(Scalar::Int(scalar)) => Self::eval_scalar_int(scalar),
            _ => None,
        }
    }

    fn resolve_const<'tcx>(
        unevaluated: UnevaluatedConst<'tcx>,
        tcx: TyCtxt<'tcx>,
    ) -> Option<u128> {
        let param_env = tcx.param_env(unevaluated.def);
        let value = tcx
            .const_eval_resolve(param_env, unevaluated.expand(), None)
            .ok()?;

        Self::eval_const_val(value)
    }

    fn from_gen_arg<'tcx>(
        arg: &GenericArg<'tcx>,
        generator: &mut Generator<'tcx>,
        span: Span,
    ) -> Result<Self, Error> {
        if let Some(ty) = arg.as_type() {
            let sig_ty = generator.find_sig_ty(&ty, None, span)?;
            return Ok(Generic::Ty(sig_ty));
        }

        let cons_val = arg.as_const().and_then(|cons| match cons.kind() {
            ConstKind::Unevaluated(unevaluated) => {
                Self::resolve_const(unevaluated, generator.tcx)
            }
            ConstKind::Value(val_tree) => {
                val_tree.try_to_scalar_int().and_then(Self::eval_scalar_int)
            }
            _ => None,
        });

        cons_val
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span).into())
            .map(Generic::Const)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generics(&'static [Generic]);

impl Generics {
    pub fn get(&self, ind: usize) -> Option<&Generic> {
        self.0.get(ind)
    }

    pub fn as_ty(&self, ind: usize) -> Option<SignalTy> {
        self.get(ind).and_then(|generic| generic.as_ty()).copied()
    }

    pub fn as_const<T: TryFrom<u128>>(&self, ind: usize) -> Option<T> {
        self.get(ind).and_then(|generic| generic.as_const())
    }

    fn from_ty<'tcx>(
        ty: &Ty<'tcx>,
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Option<Self>, Error> {
        let tcx = generator.tcx;

        let ty = match generics {
            Some(generics) => EarlyBinder::bind(*ty).instantiate(tcx, generics),
            None => *ty,
        };

        match ty.kind() {
            TyKind::Array(ty, cons) => {
                let sig_ty: Generic = generator.find_sig_ty(ty, generics, span)?.into();
                let cons: Generic = cons
                    .try_to_scalar_int()
                    .and_then(Generic::eval_scalar_int)
                    .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span))?
                    .into();

                Ok(Some(Self(unsafe {
                    with_arena().alloc_slice(&[sig_ty, cons])
                })))
            }
            TyKind::Adt(adt, generics) if !generics.is_empty() => {
                // TODO: refactor
                if generator.tcx.def_path(adt.did()) == ItemPath(&["domain", "Clock"]) {
                    return Ok(None);
                }

                let generics = if generator.tcx.def_path(adt.did())
                    == ItemPath(&["signal", "Signal"])
                {
                    Either::Left(generics.iter().skip(1)) // the first generic argument is ClockDomain
                } else {
                    Either::Right(generics.iter())
                };

                Ok(Some(Self(unsafe {
                    with_arena().alloc_from_res_iter(generics.map(|generic| {
                        Generic::from_gen_arg(&generic, generator, span)
                    }))?
                })))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Key<'tcx> {
    pub ty_or_def_id: TyOrDefId<'tcx>,
    pub generics: Option<Generics>,
}

impl<'tcx> Key<'tcx> {
    fn def_id(&self) -> Option<DefId> {
        self.ty_or_def_id.def_id()
    }

    fn as_string(&self, tcx: TyCtxt<'tcx>) -> String {
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

        let generics = Generics::from_ty(&ty, generator, generics, span)?;

        Ok(Key {
            ty_or_def_id: ty.into(),
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

    fn evaluate_item(
        &mut self,
        item: &Item<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        is_top_module: bool,
    ) -> Result<Option<ModuleId>, Error> {
        if let ItemKind::Fn(FnSig { decl, .. }, hir_generics, body_id) = item.kind {
            let fn_id = item.hir_id().owner.def_id;
            // ignore unsupported generics if current item is not top module
            self.evaluate_generics(fn_id, hir_generics, generics, !is_top_module)?;

            return self
                .evaluate_fn(item.ident, decl, body_id, generics)
                .map(Some);
        }

        Ok(None)
    }

    fn evaluate_generics(
        &mut self,
        fn_id: LocalDefId,
        hir_generics: &HirGenerics<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        ignore: bool,
    ) -> Result<(), Error> {
        let make_err =
            |span| Error::from(SpanError::new(SpanErrorKind::UnsupportedGeneric, span));

        let mut params: FxHashMap<DefId, Span> = hir_generics
            .params
            .iter()
            .map(|param| (param.def_id.to_def_id(), param.span))
            .collect();

        for predicate in hir_generics.predicates {
            match predicate {
                WherePredicate::BoundPredicate(predicate) => {
                    let def_id = match predicate.bounded_ty.as_generic_param() {
                        Some((def_id, _)) => def_id,
                        None => {
                            continue;
                        }
                    };

                    let span = match params.remove_entry(&def_id) {
                        Some((_, span)) => span,
                        None => {
                            continue;
                        }
                    };

                    let mut found_signal = false;
                    for bound in predicate.bounds {
                        let trait_ref = match bound.trait_ref() {
                            Some(trait_ref) => trait_ref,
                            None => {
                                continue;
                            }
                        };

                        let trait_def_id = match trait_ref.path.res.opt_def_id() {
                            Some(def_id) => def_id,
                            None => {
                                continue;
                            }
                        };

                        // TODO: move into blackbox
                        if self.tcx.def_path(trait_def_id)
                            == ItemPath(&["signal", "Signal"])
                        {
                            let arg = trait_ref.path.segments[0]
                                .args
                                .ok_or_else(|| make_err(span))?;

                            let ty = arg.bindings[0].ty();
                            let sig_ty =
                                self.find_sig_ty_for_hir_ty(fn_id, ty, generics, span)?;

                            let key = def_id.as_key(self, generics, span)?;
                            self.sig_ty.insert(key, Some(sig_ty));

                            let key = self
                                .ast_ty_to_ty(fn_id, predicate.bounded_ty)
                                .as_key(self, generics, span)?;
                            self.sig_ty.insert(key, Some(sig_ty));

                            found_signal = true;

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

    fn ast_ty_to_ty(&self, fn_id: LocalDefId, ty: &HirTy<'tcx>) -> Ty<'tcx> {
        let item_ctx = &ItemCtxt::new(self.tcx, fn_id) as &dyn AstConv<'tcx>;
        item_ctx.ast_ty_to_ty(ty)
    }

    fn find_sig_ty_for_hir_ty(
        &mut self,
        fn_id: LocalDefId,
        ty: &HirTy<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let ty = self.ast_ty_to_ty(fn_id, ty);
        self.find_sig_ty(&ty, generics, span)
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

        self.idents.for_module(module_id).push_scope();

        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());
        let ctx = EvalContext::new(generics, module_id);

        self.evaluate_inputs(inputs, ctx, false, &mut |_| {})?;
        let item_id = self.evaluate_expr(body.value, ctx)?;
        self.evaluate_outputs(item_id)?;

        self.idents.for_module(module_id).pop_scope();

        Ok(module_id)
    }

    fn evaluate_inputs<'a, F: FnMut(ItemId)>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        ctx: EvalContext<'tcx>,
        is_dummy: bool,
        f: &mut F,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            let item_id = self.make_input(input, ctx, is_dummy)?;
            self.pattern_match(param.pat, item_id, ctx.module_id)?;

            f(item_id);
        }

        Ok(())
    }

    fn pattern_match(
        &mut self,
        pat: &Pat<'tcx>,
        item_id: ItemId,
        module_id: ModuleId,
    ) -> Result<(), Error> {
        match pat.kind {
            PatKind::Binding(..) => {
                let ident = utils::pat_ident(pat)?;
                match item_id {
                    ItemId::Node(node_id) => {
                        let sym = self.idents.for_module(module_id).ident(ident);
                        self.net_list[node_id].outputs_mut().only_one_mut().out.sym = sym;
                    }
                    ItemId::Group(group_id) => {
                        let item_ids = self.group_list[group_id].item_ids;
                        for item_id in item_ids.iter() {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }
                    }
                }

                self.idents
                    .for_module(module_id)
                    .add_local_ident(ident, item_id);
            }
            PatKind::Slice(before, wild, after) => {
                let ArrayDesc { count, width } =
                    self.opt_array_desc(item_id).ok_or_else(|| {
                        SpanError::new(SpanErrorKind::ExpectedArray, pat.span)
                    })?;

                let sig_ty =
                    self.item_ty(self.group_list[item_id.group_id()].item_ids[0]);

                let to = self.to_bitvec(module_id, item_id);

                self.slice_pattern_match(to, width, sig_ty, before, None)?;

                if wild.is_some() {
                    let start = ((count - after.len()) as u128) * width;
                    self.slice_pattern_match(to, width, sig_ty, after, Some(start))?;
                }
            }
            PatKind::Tuple(pats, dot_dot_pos) => {
                let group_id = item_id.group_id();
                match dot_dot_pos.as_opt_usize() {
                    Some(pos) => {
                        let group = &self.group_list[group_id];
                        let len = group.len();
                        assert!(pats.len() < len);

                        let item_ids = group.item_ids;
                        for (pat, item_id) in
                            pats[0 .. pos].iter().zip(item_ids[0 .. pos].iter())
                        {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }

                        for (pat, item_id) in pats[pos ..]
                            .iter()
                            .zip(item_ids[(len - (pats.len() - pos)) ..].iter())
                        {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }
                    }
                    None => {
                        let group = &self.group_list[group_id];
                        assert_eq!(pats.len(), group.len());

                        let item_ids = group.item_ids;
                        for (pat, item_id) in pats.iter().zip(item_ids.iter()) {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }
                    }
                }
            }
            PatKind::Wild => {}
            _ => {
                println!("{:#?}", pat);
                return Err(
                    SpanError::new(SpanErrorKind::ExpectedIdentifier, pat.span).into()
                );
            }
        }

        Ok(())
    }

    fn slice_pattern_match(
        &mut self,
        node_out_id: NodeOutId,
        width: u128,
        sig_ty: SignalTy,
        pat: &[Pat<'tcx>],
        start: Option<u128>,
    ) -> Result<(), Error> {
        if pat.is_empty() {
            return Ok(());
        }

        let module_id = node_out_id.node_id().module_id();
        let splitter = Splitter::new(
            node_out_id,
            (0 .. pat.len()).map(|_| {
                (
                    PrimTy::BitVec(width),
                    self.idents.for_module(module_id).tmp(),
                )
            }),
            start,
        );
        let node_id = self.net_list.add_node(module_id, splitter);

        let outputs = self.net_list[node_id].outputs();
        assert_eq!(outputs.len(), pat.len());

        let node_out_ids = outputs
            .items()
            .map(|out| out.node_out_id(node_id))
            .collect::<Vec<_>>();

        for (node_out_id, before_pat) in node_out_ids.into_iter().zip(pat) {
            let from = self.from_bitvec(module_id, node_out_id, sig_ty);
            self.pattern_match(before_pat, from, module_id)?;
        }

        Ok(())
    }

    fn make_input(
        &mut self,
        input: &HirTy<'tcx>,
        ctx: EvalContext<'tcx>,
        is_dummy: bool,
    ) -> Result<ItemId, Error> {
        match input.kind {
            HirTyKind::Infer => {
                let sig_ty = self.find_sig_ty(
                    &self.node_type(input.hir_id),
                    ctx.generics,
                    input.span,
                )?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Path(QPath::Resolved(_, path)) => {
                let fn_id = input.hir_id.owner.def_id;
                let sig_ty = self
                    .find_sig_ty(&path.res.def_id(), ctx.generics, input.span)
                    .or_else(|_| {
                        self.find_sig_ty_for_hir_ty(
                            fn_id,
                            input,
                            ctx.generics,
                            input.span,
                        )
                    })?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Tup(ty) => {
                let group = ty
                    .iter()
                    .map(|ty| self.make_input(ty, ctx, is_dummy))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(self
                    .group_list
                    .add_group(Group::new(GroupKind::Group, group))
                    .into())
            }
            _ => {
                println!("input: {:#?}", input);
                Err(SpanError::new(SpanErrorKind::NotSynthInput, input.span).into())
            }
        }
    }

    fn make_input_with_sig_ty(
        &mut self,
        sig_ty: SignalTy,
        module_id: ModuleId,
        is_dummy: bool,
    ) -> ItemId {
        match sig_ty {
            SignalTy::Prim(prim_ty) => {
                let input =
                    InputNode::new(prim_ty, self.idents.for_module(module_id).tmp());
                (if is_dummy {
                    self.net_list.add_dummy_node(module_id, input)
                } else {
                    self.net_list.add_node(module_id, input)
                })
                .into()
            }
            SignalTy::Array(n, ty) => {
                let group = Group::new(
                    sig_ty.into(),
                    iter::repeat(ty).take(n as usize).map(|sig_ty| {
                        self.make_input_with_sig_ty(*sig_ty, module_id, is_dummy)
                    }),
                );

                self.group_list.add_group(group).into()
            }
            SignalTy::Group(ty) => {
                let group = Group::new(
                    sig_ty.into(),
                    ty.iter().map(|sig_ty| {
                        self.make_input_with_sig_ty(*sig_ty, module_id, is_dummy)
                    }),
                );
                self.group_list.add_group(group).into()
            }
        }
    }

    fn evaluate_outputs(&mut self, item_id: ItemId) -> Result<(), Error> {
        self.group_list
            .deep_iter::<Error, _>(&[item_id], &mut |_, node_id| {
                Self::make_output(&mut self.net_list, &mut self.idents, node_id);

                Ok(())
            })
    }

    fn make_output(net_list: &mut NetList, idents: &mut Idents, node_id: NodeId) {
        let module_id = node_id.module_id();
        let node = &net_list[node_id];
        let node_id = if node.is_input() || node.is_pass() {
            let out = node.outputs().only_one();
            let mut pass = PassNode::new(
                out.out.ty,
                out.node_out_id(node_id),
                idents.for_module(module_id).tmp(),
            );
            pass.inject = Some(false);

            net_list.add_node(node_id.module_id(), pass)
        } else {
            node_id
        };

        let node = &mut net_list[node_id];
        for out in node.outputs_mut().items_mut() {
            let sym = idents.for_module(module_id).out();
            out.out.sym = sym;
        }

        net_list.add_all_outputs(node_id);
    }

    pub fn evaluate_expr(
        &mut self,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let ty = self.node_type(expr.hir_id);

        match expr.kind {
            ExprKind::AddrOf(BorrowKind::Ref, Mutability::Not, expr) => {
                self.evaluate_expr(expr, ctx)
            }
            ExprKind::Array(items) => {
                let item_ids = unsafe {
                    with_arena().alloc_from_res_iter(
                        items.iter().map(|item| self.evaluate_expr(item, ctx)),
                    )?
                };
                let group = Group::new_with_item_ids(GroupKind::Array, item_ids);

                Ok(self.group_list.add_group(group).into())
            }
            ExprKind::Binary(bin_op, lhs, rhs) => {
                println!("binary");
                let prim_ty = self.find_sig_ty(&ty, ctx.generics, expr.span)?.prim_ty();

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
                        BinOpNode::new(
                            prim_ty,
                            bin_op,
                            lhs,
                            rhs,
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::Block(block, _) => {
                println!("block");
                self.idents.for_module(ctx.module_id).push_scope();

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
                                                    self.idents
                                                        .for_module(ctx.module_id)
                                                        .tmp(),
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
                                            .add_group(Group::new(
                                                GroupKind::Group,
                                                group,
                                            ))
                                            .into()
                                    } else {
                                        item_id
                                    }
                                }
                                ItemId::Group(_) => item_id,
                            };

                            let item_id = match item_id {
                                ItemId::Node(node_id) => {
                                    let out =
                                        &self.net_list[node_id].outputs().only_one();
                                    self.net_list
                                        .add_node(
                                            ctx.module_id,
                                            PassNode::new(
                                                out.out.ty,
                                                out.node_out_id(node_id),
                                                self.idents
                                                    .for_module(ctx.module_id)
                                                    .tmp(),
                                            ),
                                        )
                                        .into()
                                }
                                ItemId::Group(_) => item_id,
                            };

                            self.pattern_match(local.pat, item_id, ctx.module_id)?;
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

                let item_id = self.evaluate_expr(expr, ctx)?;

                self.idents.for_module(ctx.module_id).pop_scope();

                Ok(item_id)
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

                                    let mut evaluate = |_, node_id: NodeId| {
                                        for out in
                                            self.net_list[node_id].outputs().items()
                                        {
                                            inputs.push(out.node_out_id(node_id));
                                        }

                                        Result::<(), Error>::Ok(())
                                    };

                                    self.group_list
                                        .deep_iter(&[item_id], &mut evaluate)?;
                                }

                                let outputs = self.net_list[module_id]
                                    .outputs()
                                    .map(|node_out_id| {
                                        (
                                            self.net_list[node_out_id].ty,
                                            self.idents.for_module(ctx.module_id).tmp(),
                                        )
                                    })
                                    .collect::<Vec<_>>();

                                let inst_sym = self
                                    .idents
                                    .for_module(ctx.module_id)
                                    .inst(self.net_list[module_id].name);

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
                        QPath::TypeRelative(_, _) => {
                            let res = self
                                .tcx
                                .typeck(rec.hir_id.owner)
                                .qpath_res(&path, rec.hir_id);
                            let def_id = res.def_id();

                            let blackbox =
                                self.find_blackbox(&def_id, ctx.generics, rec.span)?;
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

                // TODO: how to avoid allocating vec
                let mut item_ids = vec![];
                self.evaluate_inputs(inputs, ctx, true, &mut |item_id| {
                    item_ids.push(item_id);
                })?;

                let mut dummy_inputs = vec![];
                self.group_list
                    .deep_iter::<Error, _>(&item_ids, &mut |_, node_id| {
                        if self.net_list[node_id].is_dummy_input() {
                            dummy_inputs.push(node_id);
                        }

                        Ok(())
                    })?;

                let closure = self.evaluate_expr(body.value, ctx)?;

                self.net_list.add_dummy_inputs(closure, dummy_inputs);

                Ok(closure)
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
                let prim_ty = self.find_sig_ty(&ty, ctx.generics, expr.span)?.prim_ty();

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
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::Index(
                expr,
                Expr {
                    kind:
                        ExprKind::Lit(Spanned {
                            node: LitKind::Int(ind, ..),
                            ..
                        }),
                    ..
                },
                span,
            ) => bit_vec_trans(
                self,
                expr,
                ctx,
                |generator, ctx, BitVecTransArgs { rec, bit_vec, .. }| {
                    let ArrayDesc { width, .. } =
                        generator.opt_array_desc(rec).ok_or_else(|| {
                            SpanError::new(SpanErrorKind::ExpectedArray, span)
                        })?;

                    let ty = PrimTy::BitVec(width);
                    let start = ind * width;
                    Ok((
                        generator.net_list.add_node(
                            ctx.module_id,
                            Splitter::new(
                                bit_vec,
                                [(ty, generator.idents.for_module(ctx.module_id).tmp())],
                                Some(start),
                            ),
                        ),
                        ty.into(),
                    ))
                },
            ),
            ExprKind::Lit(lit) => {
                println!("lit");
                let prim_ty = self.find_sig_ty(&ty, ctx.generics, lit.span)?.prim_ty();
                let value = blackbox::evaluate_lit(prim_ty, lit)?;

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        ConstNode::new(
                            prim_ty,
                            value,
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
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

                self.item_id_for_ident(ctx.module_id, segments[0].ident)
            }
            ExprKind::Tup(exprs) => {
                let groups = unsafe {
                    with_arena().alloc_from_res_iter(
                        exprs.iter().map(|expr| self.evaluate_expr(expr, ctx)),
                    )?
                };

                Ok(self
                    .group_list
                    .add_group(Group::new_with_item_ids(GroupKind::Group, groups))
                    .into())
            }
            ExprKind::Unary(UnOp::Not, inner) => {
                println!("unary");

                let comb = self.evaluate_expr(inner, ctx)?.node_id();
                let prim_ty = self.find_sig_ty(&ty, ctx.generics, expr.span)?.prim_ty();
                let sym = self.idents.for_module(ctx.module_id).tmp();

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
