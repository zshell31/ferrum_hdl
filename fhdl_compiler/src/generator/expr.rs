use std::{
    collections::hash_map::Entry,
    convert::identity,
    sync::atomic::{AtomicBool, AtomicU8, Ordering},
};

use fhdl_netlist::{
    group::ItemId,
    net_list::{NodeId, NodeOutId},
    node::{
        BinOp, BinOpNode, BitNot, Case, Const, Input, IsNode, ModInst, MultiPass, Mux2,
        Not, Pass, Splitter,
    },
    params::Outputs,
    sig_ty::{PrimTy, SignalTy, SignalTyKind},
    symbol::Symbol,
};
use if_chain::if_chain;
use rustc_ast::LitKind;
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::{
    def::{CtorOf, DefKind, Res},
    def_id::{DefId, LocalDefId},
    Expr, ExprKind, Node as HirNode, PatKind, Path, QPath, StmtKind, Ty as HirTy,
    TyKind as HirTyKind, UnOp,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_middle::ty::{
    AdtKind, FnSig, GenericArgsRef, InstanceDef, ParamEnv, ParamEnvAnd, Ty, TyKind,
};
use rustc_span::{source_map::Spanned, Span, Symbol as RustSymbol};
use smallvec::SmallVec;

use super::{
    arg_matcher::ArgMatcher, func::ModuleOrItemId, generic::Generic, EvalContext,
    Generator, MonoItem, TraitKind,
};
use crate::{
    blackbox::{cast::StdConversion, lit, EvaluateExpr},
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

static IDENT: AtomicU8 = AtomicU8::new(0);
static MOST_INNER_ERR_SCOPE: AtomicBool = AtomicBool::new(false);

#[allow(dead_code)]
struct Scope<'tcx> {
    module: Symbol,
    kind: &'tcx ExprKind<'tcx>,
    generic_args: GenericArgsRef<'tcx>,
}

#[allow(dead_code)]
#[allow(unused_variables)]
impl<'tcx> Scope<'tcx> {
    fn enter(
        module: Symbol,
        kind: &'tcx ExprKind<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Self {
        let ident = AtomicU8::fetch_add(&IDENT, 1, Ordering::Relaxed) as usize;
        println!(
            "{}--> {} ({} <{:?}>)",
            " ".repeat(ident << 2),
            Self::to_str(kind),
            module,
            generic_args
        );

        Self {
            module,
            kind,
            generic_args,
        }
    }

    fn exit(self) {
        let ident = AtomicU8::fetch_sub(&IDENT, 1, Ordering::Relaxed) as usize;
        println!(
            "{}<-- {} ({} <{:?}>)",
            " ".repeat((ident - 1) << 2),
            Self::to_str(self.kind),
            self.module,
            self.generic_args
        );
    }

    fn inner_most_error(&self, f: impl FnOnce()) {
        if !MOST_INNER_ERR_SCOPE.swap(true, Ordering::Relaxed) {
            f();
        }
    }

    fn to_str(kind: &'tcx ExprKind<'tcx>) -> &'static str {
        match kind {
            ExprKind::ConstBlock(_) => "const block",
            ExprKind::Array(_) => "array",
            ExprKind::Call(_, _) => "call",
            ExprKind::MethodCall(_, _, _, _) => "method call",
            ExprKind::Tup(_) => "tup",
            ExprKind::Binary(_, _, _) => "binary",
            ExprKind::Unary(_, _) => "unary",
            ExprKind::Lit(_) => "lit",
            ExprKind::Cast(_, _) => "cast",
            ExprKind::Type(_, _) => "type",
            ExprKind::DropTemps(_) => "drop temps",
            ExprKind::Let(_) => "let",
            ExprKind::If(_, _, _) => "if",
            ExprKind::Loop(_, _, _, _) => "loop",
            ExprKind::Match(_, _, _) => "match",
            ExprKind::Closure(_) => "closure",
            ExprKind::Block(_, _) => "block",
            ExprKind::Assign(_, _, _) => "assign",
            ExprKind::AssignOp(_, _, _) => "assign op",
            ExprKind::Field(_, _) => "field",
            ExprKind::Index(_, _, _) => "index",
            ExprKind::Path(_) => "path",
            ExprKind::AddrOf(_, _, _) => "addr of",
            ExprKind::Break(_, _) => "break",
            ExprKind::Continue(_) => "continue",
            ExprKind::Ret(_) => "ret",
            ExprKind::Become(_) => "become",
            ExprKind::InlineAsm(_) => "inline asm",
            ExprKind::OffsetOf(_, _) => "offset of",
            ExprKind::Struct(_, _, _) => "struct",
            ExprKind::Repeat(_, _) => "repeat",
            ExprKind::Yield(_, _) => "yield",
            ExprKind::Err(_) => "err",
        }
    }
}

pub enum ExprOrItemId<'tcx> {
    Expr(&'tcx Expr<'tcx>),
    Item(ItemId),
}

impl<'tcx> From<&'tcx Expr<'tcx>> for ExprOrItemId<'tcx> {
    fn from(expr: &'tcx Expr<'tcx>) -> Self {
        ExprOrItemId::Expr(expr)
    }
}

impl<'tcx> From<ItemId> for ExprOrItemId<'tcx> {
    fn from(item_id: ItemId) -> Self {
        ExprOrItemId::Item(item_id)
    }
}

impl<'tcx> ExprOrItemId<'tcx> {
    pub fn evaluate(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match self {
            ExprOrItemId::Expr(expr) => generator.evaluate_expr(expr, ctx),
            ExprOrItemId::Item(item_id) => Ok(*item_id),
        }
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn make_struct_group_from_exprs(
        &mut self,
        expr: &Expr<'tcx>,
        sub_exprs: impl IntoIterator<Item = &'tcx Expr<'tcx>> + 'tcx,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let sig_ty = self.find_sig_ty(
            self.node_type(expr.hir_id, ctx),
            ctx.generic_args,
            expr.span,
        )?;
        match sig_ty.kind {
            SignalTyKind::Struct(ty) => {
                let sub_exprs = sub_exprs
                    .into_iter()
                    .filter(|sub_expr| {
                        let ty = self.node_type(sub_expr.hir_id, ctx);
                        match utils::ty_def_id(ty) {
                            Some(def_id) => !self.ignore_ty(def_id),
                            None => true,
                        }
                    })
                    .collect::<Vec<_>>();

                assert_eq!(sub_exprs.len(), ty.len());

                self.make_struct_group(
                    ty,
                    ty.tys().iter().zip(sub_exprs).map(|(_, sub_expr)| sub_expr),
                    |generator, sub_expr| generator.evaluate_expr(sub_expr, ctx),
                )
            }
            _ => Err(SpanError::new(SpanErrorKind::ExpectedStructType, expr.span).into()),
        }
    }

    pub fn make_dummy_inputs_from_sig_ty(
        &mut self,
        sig_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
    ) -> (ItemId, SmallVec<[NodeId; 8]>) {
        let mut dummy_inputs = SmallVec::new();
        let item_id =
            self.make_dummy_inputs_from_sig_ty_inner(sig_ty, ctx, &mut dummy_inputs);
        (item_id, dummy_inputs)
    }

    fn make_dummy_inputs_from_sig_ty_inner(
        &mut self,
        sig_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
        dummy_inputs: &mut SmallVec<[NodeId; 8]>,
    ) -> ItemId {
        let module_id = ctx.module_id;

        match sig_ty.kind {
            SignalTyKind::Prim(ty) => {
                let dummy_input = self.net_list.add_dummy_node(
                    module_id,
                    Input::new(ty, self.idents.for_module(module_id).tmp()),
                );
                dummy_inputs.push(dummy_input);
                dummy_input.into()
            }
            SignalTyKind::Array(ty) => self
                .make_array_group(ty, ty.tys(), |generator, sig_ty| {
                    Ok(generator.make_dummy_inputs_from_sig_ty_inner(
                        sig_ty,
                        ctx,
                        dummy_inputs,
                    ))
                })
                .unwrap(),
            SignalTyKind::Struct(ty) => self
                .make_struct_group(
                    ty,
                    ty.tys().iter().map(|ty| ty.inner),
                    |generator, sig_ty| {
                        Ok(generator.make_dummy_inputs_from_sig_ty_inner(
                            sig_ty,
                            ctx,
                            dummy_inputs,
                        ))
                    },
                )
                .unwrap(),
            SignalTyKind::Enum(ty) => {
                let dummy_input = self.net_list.add_dummy_node(
                    module_id,
                    Input::new(ty.prim_ty(), self.idents.for_module(module_id).tmp()),
                );
                dummy_inputs.push(dummy_input);
                dummy_input.into()
            }
        }
    }

    pub fn evaluate_expr(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let scope = Scope::enter(
            self.net_list[ctx.module_id].name,
            &expr.kind,
            ctx.generic_args,
        );

        let res = self.evaluate_expr_(expr, ctx);
        if res.is_ok() {
            scope.exit();
        } else {
            scope.inner_most_error(|| {
                println!("{:#?}", expr);
            });
        }

        res
    }

    pub fn evaluate_expr_(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let ty = self.node_type(expr.hir_id, ctx);

        match expr.kind {
            ExprKind::Array(items) => {
                let array_ty = self
                    .find_sig_ty(ty, ctx.generic_args, expr.span)?
                    .array_ty();

                self.make_array_group(array_ty, items.iter(), |generator, item| {
                    generator.evaluate_expr(item, ctx)
                })
            }
            ExprKind::Binary(bin_op, lhs, rhs) => {
                self.bin_op(ty, utils::to_bin_op(bin_op.node), lhs, rhs, ctx, expr.span)
            }
            ExprKind::Block(block, _) => {
                self.idents.for_module(ctx.module_id).push_scope();

                for stmt in block.stmts {
                    match stmt.kind {
                        StmtKind::Local(local) if local.els.is_none() => {
                            let init = local.init.ok_or_else(|| {
                                SpanError::new(SpanErrorKind::ExpectedExpr, local.span)
                            })?;
                            let item_id = self.evaluate_expr(init, ctx)?;

                            let item_id = match item_id {
                                ItemId::Node(node_id) => self.combine_outputs(node_id),
                                ItemId::Group(_) => item_id,
                            };

                            let item_id = match item_id {
                                ItemId::Node(node_id) => {
                                    let out =
                                        &self.net_list[node_id].kind.outputs().only_one();
                                    self.net_list
                                        .add_node(
                                            ctx.module_id,
                                            Pass::new(
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
                        StmtKind::Item(_) => {}
                        _ => {
                            println!("{:#?}", stmt);
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
            ExprKind::Call(fn_item, args) => {
                if let ExprKind::Path(path) = fn_item.kind {
                    match path {
                        QPath::Resolved(
                            _,
                            Path {
                                res: Res::SelfCtor(_),
                                ..
                            },
                        ) => self.make_struct_group_from_exprs(expr, args, ctx),
                        QPath::Resolved(
                            _,
                            Path {
                                span,
                                res: Res::Def(DefKind::Fn, fn_did),
                                ..
                            },
                        ) => {
                            if fn_did.is_local() {
                                let fn_ty = self.node_type(fn_item.hir_id, ctx);
                                let generic_args = utils::subst(fn_ty);

                                self.evaluate_fn_call(
                                    fn_did.expect_local(),
                                    generic_args,
                                    args.iter().map(Into::into),
                                    ctx,
                                    expr.span,
                                )
                            } else {
                                let blackbox =
                                    self.find_blackbox(*fn_did, ctx.generic_args, *span)?;
                                blackbox.evaluate_expr(self, expr, ctx)
                            }
                        }
                        QPath::Resolved(
                            _,
                            Path {
                                res:
                                    Res::Def(
                                        DefKind::Ctor(CtorOf::Variant, _),
                                        variant_ctor_did,
                                    ),
                                ..
                            },
                        ) if variant_ctor_did.is_local() => self
                            .evaluate_enum_variant_ctor(
                                *variant_ctor_did,
                                ty,
                                ctx,
                                args,
                                expr.span,
                            ),
                        QPath::Resolved(
                            _,
                            Path {
                                res:
                                    Res::Def(DefKind::Ctor(CtorOf::Struct, _), struct_did),
                                ..
                            },
                        ) if struct_did.is_local()
                            || self
                                .find_blackbox_ty(*struct_did)
                                .filter(|ty| ty.has_constructor())
                                .is_some() =>
                        {
                            self.make_struct_group_from_exprs(expr, args, ctx)
                        }
                        QPath::TypeRelative(
                            qself @ HirTy {
                                kind:
                                    HirTyKind::Path(QPath::Resolved(
                                        _,
                                        Path {
                                            res: Res::SelfTyAlias { alias_to, .. },
                                            ..
                                        },
                                    )),
                                ..
                            },
                            assoc_segment,
                        ) => {
                            let ty = self
                                .tcx
                                .type_of(alias_to)
                                .instantiate(self.tcx, ctx.generic_args);
                            if_chain! {

                                if let TyKind::Adt(adt, _) = ty.kind();
                                if adt.adt_kind() == AdtKind::Enum;

                                let item_ctx =
                                    &ItemCtxt::new(self.tcx, expr.hir_id.owner.def_id);
                                let astconv = item_ctx.astconv();

                                if let Ok((_, _, variant_did)) = astconv
                                    .associated_path_to_ty(
                                        expr.hir_id,
                                        expr.span,
                                        ty,
                                        qself,
                                        assoc_segment,
                                        true,
                                    );
                                then {

                                self.evaluate_enum_variant(
                                    variant_did,
                                    ty,
                                    ctx,
                                    args,
                                    expr.span,
                                )
                                } else {

                                    self.method_call(expr, fn_item, args, ctx)
                                }
                            }
                        }
                        QPath::TypeRelative(_, _) => {
                            self.method_call(expr, fn_item, args, ctx)
                        }
                        _ => {
                            Err(SpanError::new(SpanErrorKind::NotSynthCall, fn_item.span)
                                .into())
                        }
                    }
                } else {
                    Err(SpanError::new(SpanErrorKind::NotSynthCall, expr.span).into())
                }
            }
            ExprKind::Cast(expr, _) => self.evaluate_expr(expr, ctx),
            ExprKind::Closure(closure) => {
                let body = self.tcx.hir().body(closure.body);
                let inputs = closure.fn_decl.inputs.iter().zip(body.params.iter());

                let mut item_ids = SmallVec::<[_; 8]>::new();
                self.evaluate_inputs(inputs, ctx, true, &mut |item_id| {
                    item_ids.push(item_id);
                })?;

                let closure = self.evaluate_expr(body.value, ctx)?;

                self.net_list.add_dummy_inputs(
                    closure,
                    item_ids.into_iter().flat_map(|item_id| item_id.into_iter()),
                );

                Ok(closure)
            }
            ExprKind::DropTemps(inner) => self.evaluate_expr(inner, ctx),
            ExprKind::Field(expr, ident) => {
                let group = self.evaluate_expr(expr, ctx)?.group();

                group.by_field(ident.as_str()).ok_or_else(|| {
                    SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
                })
            }
            ExprKind::If(cond, if_block, else_block) => {
                let sig_ty = self.find_sig_ty(ty, ctx.generic_args, expr.span)?;

                let else_block = else_block.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedIfElseExpr, expr.span)
                })?;

                let cond = self.evaluate_expr(cond, ctx)?.node_id();
                let if_block = self.evaluate_expr(if_block, ctx)?;
                let else_block = self.evaluate_expr(else_block, ctx)?;

                let cond = self.net_list.only_one_node_out_id(cond);
                let if_block = self.maybe_to_bitvec(ctx.module_id, if_block);
                let else_block = self.maybe_to_bitvec(ctx.module_id, else_block);

                let mux = self.net_list.add_node(
                    ctx.module_id,
                    Mux2::new(
                        sig_ty.maybe_to_bitvec(),
                        cond,
                        if_block,
                        else_block,
                        self.idents.for_module(ctx.module_id).tmp(),
                    ),
                );
                let mux = self.net_list[mux]
                    .kind
                    .outputs()
                    .only_one()
                    .node_out_id(mux);

                Ok(self.maybe_from_bitvec(ctx.module_id, mux, sig_ty))
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
                _,
            ) => self.index(expr, *ind, ctx),
            ExprKind::Lit(lit) => {
                let prim_ty = self.find_sig_ty(ty, ctx.generic_args, lit.span)?.prim_ty();
                let value = lit::evaluate_lit(prim_ty, lit)?;

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        Const::new(
                            prim_ty,
                            value,
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::Match(sel, arms, _) => {
                let expr_ty = self.find_sig_ty(ty, ctx.generic_args, expr.span)?;

                let sel_span = sel.span;
                let sel = self.evaluate_expr(sel, ctx)?;
                let sel_sig_ty = self.item_ty(sel);

                let mut variants_map = FxHashMap::default();
                let mut default = None;
                let mut small_mask = u128::MAX;
                let mut inputs = SmallVec::<[_; 8]>::new();

                for arm in arms {
                    if arm.guard.is_some() {
                        return Err(SpanError::new(
                            SpanErrorKind::UnsupportedGuard,
                            arm.span,
                        )
                        .into());
                    }
                    if let PatKind::Wild = arm.pat.kind {
                        let item_id = self.evaluate_expr(arm.body, ctx)?;
                        let node_out_id = self.maybe_to_bitvec(ctx.module_id, item_id);
                        default = Some(node_out_id);

                        continue;
                    }

                    match sel_sig_ty.kind {
                        SignalTyKind::Enum(enum_ty) => {
                            let sel = sel.node_id();
                            let (variant_def, variant_idx) =
                                self.pattern_to_variant_def(arm.pat, ctx.generic_args)?;
                            let def_id = variant_def.def_id;

                            if let Entry::Vacant(e) = variants_map.entry(def_id) {
                                let variant = self.enum_variant_from_bitvec(
                                    ctx.module_id,
                                    sel,
                                    enum_ty,
                                    variant_idx,
                                );
                                e.insert(variant);
                            }

                            let variant = *variants_map.get(&def_id).unwrap();
                            self.pattern_match(arm.pat, variant, ctx.module_id)?;
                        }
                        SignalTyKind::Array(_) | SignalTyKind::Struct(_) => {
                            self.pattern_match(arm.pat, sel, ctx.module_id)?;
                        }
                        _ => {
                            println!("{:?}", sel_sig_ty);
                            return Err(SpanError::new(
                                SpanErrorKind::NonMatchableExpr,
                                sel_span,
                            )
                            .into());
                        }
                    }

                    let variant_item_id = self.evaluate_expr(arm.body, ctx)?;
                    let discr =
                        self.pattern_to_bitvec(arm.pat, sel_sig_ty, ctx.generic_args)?;
                    if discr.mask < small_mask {
                        small_mask = discr.mask;
                    }

                    let node_out_id =
                        self.maybe_to_bitvec(ctx.module_id, variant_item_id);

                    inputs.push((discr, node_out_id));
                }

                let mut sel = self.to_bitvec(ctx.module_id, sel);

                let small_mask_ones = small_mask.trailing_ones() as u128;
                if small_mask != 0 && small_mask_ones > 0 {
                    let sel_out = self.net_list[sel];
                    let sel_width = sel_out.width();

                    let new_sel = self.net_list.add_node(
                        ctx.module_id,
                        Splitter::new(
                            sel,
                            [(
                                PrimTy::BitVec(sel_width - small_mask_ones),
                                self.idents.for_module(ctx.module_id).tmp(),
                            )],
                            None,
                            true,
                        ),
                    );
                    sel = self.net_list[new_sel]
                        .kind
                        .outputs()
                        .only_one()
                        .node_out_id(new_sel);

                    for (discr, _) in &mut inputs {
                        discr.shiftr(small_mask_ones);
                    }
                }

                let case = self.net_list.add_node(
                    ctx.module_id,
                    Case::new(
                        expr_ty.maybe_to_bitvec(),
                        sel,
                        inputs,
                        default,
                        self.idents.for_module(ctx.module_id).tmp(),
                    ),
                );
                let case = self.net_list[case]
                    .kind
                    .outputs()
                    .only_one()
                    .node_out_id(case);
                Ok(self.maybe_from_bitvec(ctx.module_id, case, expr_ty))
            }
            ExprKind::MethodCall(_, rec, args, span) => {
                let fn_did = self
                    .tcx
                    .typeck(expr.hir_id.owner)
                    .type_dependent_def_id(expr.hir_id)
                    .unwrap();
                // TODO: how to define generic args for method call
                let generic_args = self.extract_generic_args_for_fn(fn_did, expr, ctx)?;

                match self.find_local_impl_id(fn_did, generic_args) {
                    Some((impl_id, generic_args)) => self.evaluate_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some(rec.into()),
                        args.iter().map(Into::into),
                        ctx,
                        expr.span,
                    ),
                    None => {
                        let blackbox = self.find_blackbox(fn_did, generic_args, span)?;
                        if let Some(from) = blackbox.is_std_conversion() {
                            if let Some(fn_did) = self.find_trait_method(TraitKind::From)
                            {
                                let generic_args = self
                                    .maybe_swap_generic_args_for_conversion(
                                        from,
                                        generic_args,
                                    );
                                if let Some((impl_id, generic_args)) =
                                    self.find_local_impl_id(fn_did, generic_args)
                                {
                                    return self.evaluate_impl_fn_call(
                                        impl_id,
                                        generic_args,
                                        Some(rec.into()),
                                        args.iter().map(Into::into),
                                        ctx,
                                        expr.span,
                                    );
                                }
                            }
                        }
                        blackbox.evaluate_expr(self, expr, ctx)
                    }
                }
            }
            ExprKind::Path(QPath::Resolved(_, Path { res, segments, .. })) => match res {
                Res::Local(_) if segments.len() == 1 => {
                    let ident = segments[0].ident;
                    self.item_id_for_ident(ctx.module_id, ident)
                }
                Res::Def(DefKind::AssocFn, def_id) | Res::Def(DefKind::Fn, def_id) => {
                    self.evaluate_closure_fn_without_params(*def_id, expr, ctx)
                }
                Res::Def(DefKind::Const, def_id) => {
                    if def_id.is_local() {
                        if let Some(const_val) = self.eval_const_val(
                            *def_id,
                            ctx.generic_args,
                            Some(expr.span),
                        ) {
                            let sig_ty =
                                self.find_sig_ty(ty, ctx.generic_args, expr.span)?;

                            let node = self.net_list.add_node(
                                ctx.module_id,
                                Const::new(
                                    sig_ty.maybe_to_bitvec(),
                                    const_val,
                                    self.idents.for_module(ctx.module_id).tmp(),
                                ),
                            );
                            let node_out_id = self.net_list[node]
                                .kind
                                .outputs()
                                .only_one()
                                .node_out_id(node);

                            Ok(self.maybe_from_bitvec(ctx.module_id, node_out_id, sig_ty))
                        } else {
                            Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span)
                                .into())
                        }
                    } else {
                        let blackbox =
                            self.find_blackbox(*def_id, ctx.generic_args, expr.span)?;
                        blackbox.evaluate_expr(self, expr, ctx)
                    }
                }
                Res::Def(DefKind::ConstParam, def_id) => {
                    let prim_ty =
                        self.find_sig_ty(ty, ctx.generic_args, expr.span)?.prim_ty();

                    let generics = self.tcx.generics_of(self.tcx.parent(*def_id));
                    let value = generics
                        .param_def_id_to_index(self.tcx, *def_id)
                        .and_then(|ind| ctx.generic_args.get(ind as usize))
                        .and_then(|arg| Generic::from_gen_arg(arg, self, expr.span).ok())
                        .and_then(|gen| gen.as_const())
                        .ok_or_else(|| {
                            SpanError::new(SpanErrorKind::ExpectedConst, expr.span)
                        })?;

                    Ok(self
                        .net_list
                        .add_node(
                            ctx.module_id,
                            Const::new(
                                prim_ty,
                                value,
                                self.idents.for_module(ctx.module_id).tmp(),
                            ),
                        )
                        .into())
                }
                Res::Def(DefKind::Ctor(CtorOf::Variant, ..), variant_ctor_did)
                    if variant_ctor_did.is_local() =>
                {
                    self.evaluate_enum_variant_ctor(
                        *variant_ctor_did,
                        ty,
                        ctx,
                        &[],
                        expr.span,
                    )
                }
                _ => Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()),
            },
            ExprKind::Path(qpath @ QPath::TypeRelative(rel_ty, _)) => {
                let ty = self.node_type(expr.hir_id, ctx);
                if ty.is_fn() {
                    let fn_did = utils::ty_def_id(ty).unwrap();
                    self.evaluate_closure_fn_without_params(fn_did, expr, ctx)
                } else {
                    let def_id = self
                        .tcx
                        .typeck(expr.hir_id.owner)
                        .qpath_res(&qpath, expr.hir_id)
                        .opt_def_id();
                    if let Some(def_id) = def_id {
                        if let DefKind::AssocConst = self.tcx.def_kind(def_id) {
                            let rel_ty = ctx.instantiate(
                                self.tcx,
                                self.ast_ty_to_ty(expr.hir_id.owner.def_id, rel_ty),
                            );
                            let rel_ty = ctx.instantiate(self.tcx, rel_ty);
                            let args = self.tcx.mk_args(&[rel_ty.into()]);

                            if let Some(const_val) =
                                self.eval_const_val(def_id, args, Some(expr.span))
                            {
                                let sig_ty =
                                    self.find_sig_ty(ty, ctx.generic_args, expr.span)?;
                                return Ok(self
                                    .net_list
                                    .add_node(
                                        ctx.module_id,
                                        Const::new(
                                            sig_ty.prim_ty(),
                                            const_val,
                                            self.idents.for_module(ctx.module_id).tmp(),
                                        ),
                                    )
                                    .into());
                            }
                        }
                    }
                    Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
                }
            }
            ExprKind::Struct(
                QPath::Resolved(
                    _,
                    Path {
                        res:
                            Res::SelfTyAlias {
                                alias_to: struct_did,
                                ..
                            }
                            | Res::Def(DefKind::Struct, struct_did),
                        ..
                    },
                ),
                fields,
                base,
            ) if struct_did.is_local() && base.is_none() => self
                .make_struct_group_from_exprs(
                    expr,
                    fields.iter().map(|field| field.expr),
                    ctx,
                ),
            ExprKind::Struct(
                QPath::Resolved(
                    _,
                    Path {
                        res: Res::Def(DefKind::Variant, variant_did),
                        ..
                    },
                ),
                fields,
                base,
            ) if variant_did.is_local() && base.is_none() => self.evaluate_enum_variant(
                *variant_did,
                ty,
                ctx,
                fields.iter().map(|field| field.expr),
                expr.span,
            ),
            ExprKind::Tup(exprs) => {
                let struct_ty = self
                    .find_sig_ty(ty, ctx.generic_args, expr.span)?
                    .struct_ty();

                self.make_struct_group(struct_ty, exprs.iter(), |generator, expr| {
                    generator.evaluate_expr(expr, ctx)
                })
            }
            ExprKind::Unary(UnOp::Not, inner) => {
                let comb = self.evaluate_expr(inner, ctx)?.node_id();
                let prim_ty =
                    self.find_sig_ty(ty, ctx.generic_args, expr.span)?.prim_ty();
                let sym = self.idents.for_module(ctx.module_id).tmp();

                let comb = self.net_list.only_one_node_out_id(comb);

                Ok((if prim_ty.is_bool() {
                    self.net_list
                        .add_node(ctx.module_id, Not::new(prim_ty, comb, sym))
                } else {
                    self.net_list
                        .add_node(ctx.module_id, BitNot::new(prim_ty, comb, sym))
                })
                .into())
            }
            _ => Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()),
        }
    }

    fn maybe_swap_generic_args_for_conversion(
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

    fn evaluate_closure_fn_without_params(
        &mut self,
        fn_id: DefId,
        expr: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let arg = match self.tcx.hir().find_parent(expr.hir_id) {
            Some(HirNode::Expr(Expr {
                kind: ExprKind::MethodCall(_, _, args, _),
                ..
            })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
            Some(HirNode::Expr(Expr {
                kind: ExprKind::Call(_, args),
                ..
            })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
            _ => None,
        };

        if let Some(arg) = arg {
            let span = arg.span;
            let ty = self.node_type(arg.hir_id, ctx);
            let generic_args = ctx.instantiate(self.tcx, utils::subst(ty));

            let fn_sig = self.fn_sig(fn_id, Some(generic_args));
            let input_ty = self.find_sig_ty(fn_sig.inputs()[0], generic_args, span)?;
            let output_ty = self.find_sig_ty(fn_sig.output(), generic_args, span)?;

            // println!(
            //     "evaluate closure fn without params: {:?}: {:?}",
            //     fn_sig, generic_args
            // );

            if fn_id.is_local() {
                let (input, dummy_inputs) =
                    self.make_dummy_inputs_from_sig_ty(input_ty, ctx);
                let closure = match self.find_local_impl_id(fn_id, generic_args) {
                    Some((impl_id, generic_args)) => self.evaluate_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some(input.into()),
                        [],
                        ctx,
                        expr.span,
                    )?,
                    None => self.evaluate_fn_call(
                        fn_id.expect_local(),
                        generic_args,
                        [input.into()],
                        ctx,
                        expr.span,
                    )?,
                };

                self.net_list.add_dummy_inputs(closure, dummy_inputs);

                return Ok(closure);
            } else {
                let blackbox = self.find_blackbox(fn_id, ctx.generic_args, span)?;
                if let Some(from) = blackbox.is_std_conversion() {
                    let (input, dummy_inputs) =
                        self.make_dummy_inputs_from_sig_ty(input_ty, ctx);

                    let closure = match self.find_trait_method(TraitKind::From) {
                        Some(fn_did) => {
                            let generic_args = self
                                .maybe_swap_generic_args_for_conversion(
                                    from,
                                    generic_args,
                                );

                            match self.find_local_impl_id(fn_did, generic_args) {
                                Some((impl_id, generic_args)) => {
                                    Some(self.evaluate_impl_fn_call(
                                        impl_id,
                                        generic_args,
                                        Some(input.into()),
                                        [],
                                        ctx,
                                        expr.span,
                                    )?)
                                }
                                None => None,
                            }
                        }
                        None => None,
                    };

                    let closure = match closure {
                        Some(closure) => closure,
                        None => StdConversion::convert(
                            input_ty, output_ty, input, self, expr.span,
                        )?,
                    };

                    self.net_list.add_dummy_inputs(closure, dummy_inputs);

                    return Ok(closure);
                }
            }
        }
        Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
    }

    pub fn extract_generic_args_for_fn(
        &self,
        fn_id: DefId,
        expr: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<GenericArgsRef<'tcx>, Error> {
        let arg_matcher = ArgMatcher::new(self.tcx);

        let fn_args = self.subst_with(
            self.tcx.typeck(expr.hir_id.owner).node_args(expr.hir_id),
            ctx,
        );

        let fn_generic_args =
            utils::subst(self.tcx.type_of(fn_id).instantiate_identity());

        let res = arg_matcher
            .extract_params(fn_id, fn_args, fn_generic_args)
            .ok_or_else(|| {
                // println!("'{:?}' and '{:?}' does not match", fn_args, fn_generic_args);
                SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
            });

        // println!(
        //     "extract generic args: fn_args {:?} == fn_generic_args {:?} ? {:?}",
        //     fn_args, fn_generic_args, res
        // );

        res
    }

    fn eval_fn_args(
        &mut self,
        self_arg: Option<ExprOrItemId<'tcx>>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<SmallVec<[ItemId; 8]>, Error> {
        let mut evaluated = SmallVec::new();

        if let Some(self_arg) = self_arg {
            let self_arg = self_arg.evaluate(self, ctx)?;
            evaluated.push(self_arg);
        }

        for arg in args {
            let arg = arg.evaluate(self, ctx)?;
            evaluated.push(arg);
        }

        Ok(evaluated)
    }

    fn instantiate_module(
        &mut self,
        id: ModuleOrItemId,
        inputs: SmallVec<[ItemId; 8]>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let node_id = match id {
            ModuleOrItemId::Module(module_id) => {
                let inputs = inputs
                    .into_iter()
                    .flat_map(|input| {
                        input.into_iter().flat_map(|node_id| {
                            self.net_list[node_id]
                                .kind
                                .outputs()
                                .items()
                                .map(move |out| out.node_out_id(node_id))
                        })
                    })
                    .collect::<SmallVec<[NodeOutId; 8]>>();

                let outputs = self.net_list[module_id]
                    .outputs()
                    .map(|node_out_id| {
                        (
                            self.net_list[node_out_id].ty,
                            self.idents.for_module(ctx.module_id).tmp(),
                        )
                    })
                    .collect::<Vec<_>>();

                assert_eq!(outputs.len(), self.net_list[module_id].outputs_len());

                let inst_sym = self
                    .idents
                    .for_module(ctx.module_id)
                    .inst(self.net_list[module_id].name);

                self.net_list.add_node(
                    ctx.module_id,
                    ModInst::new(inst_sym, module_id, inputs, outputs),
                )
            }
            ModuleOrItemId::Item(item_id) => {
                self.link_dummy_inputs(&inputs, item_id)
                    .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedCall, span))?;

                let pass_inputs = item_id
                    .into_iter()
                    .flat_map(|node_id| {
                        self.net_list[node_id]
                            .kind
                            .outputs()
                            .items()
                            .map(move |out| out.node_out_id(node_id))
                    })
                    .collect::<SmallVec<[NodeOutId; 8]>>();

                let pass_outputs = pass_inputs
                    .iter()
                    .map(|node_out_id| {
                        (
                            self.net_list[*node_out_id].ty,
                            self.idents.for_module(ctx.module_id).tmp(),
                        )
                    })
                    .collect::<SmallVec<[(PrimTy, Symbol); 8]>>();

                self.net_list
                    .add_node(ctx.module_id, MultiPass::new(pass_inputs, pass_outputs))
            }
        };

        Ok(node_id.into())
    }

    pub fn find_local_impl_id(
        &self,
        fn_did: DefId,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Option<(LocalDefId, GenericArgsRef<'tcx>)> {
        let res = self
            .tcx
            .resolve_instance(ParamEnvAnd {
                param_env: ParamEnv::reveal_all(),
                value: (fn_did, generic_args),
            })
            .ok()
            .and_then(identity)
            .and_then(|instance| match instance.def {
                InstanceDef::Item(def) => Some((def, instance.args)),
                _ => None,
            });

        res.and_then(|(def, args)| def.as_local().map(|def| (def, args)))
        // if fn_did.is_local() && self.tcx.impl_of_method(fn_did).is_some() {
        //     return Some((fn_did.expect_local(), generic_args));
        // }

        // if self.tcx.trait_of_item(fn_did).is_some() {
        //     let res = self
        //         .tcx
        //         .resolve_instance(ParamEnvAnd {
        //             param_env: ParamEnv::reveal_all(),
        //             value: (fn_did, generic_args),
        //         })
        //         .ok()
        //         .and_then(identity)
        //         .and_then(|instance| match instance.def {
        //             InstanceDef::Item(def) => Some((def, instance.args)),
        //             _ => None,
        //         });

        //     return res.and_then(|(def, args)| def.as_local().map(|def| (def, args)));
        // }

        // None
    }

    pub fn fn_sig(
        &self,
        def_id: DefId,
        subst: Option<GenericArgsRef<'tcx>>,
    ) -> FnSig<'tcx> {
        let fn_sig = self.tcx.fn_sig(def_id);

        (match subst {
            Some(subst) => fn_sig.instantiate(self.tcx, subst),
            None => fn_sig.instantiate_identity(),
        })
        .skip_binder()
    }

    pub fn evaluate_fn_call(
        &mut self,
        fn_id: LocalDefId,
        generic_args: GenericArgsRef<'tcx>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let item = self.tcx.hir().expect_item(fn_id);
        let inlined = self.is_inlined(fn_id);

        let args = self.eval_fn_args(None, args, ctx)?;

        let id = if inlined {
            self.evaluate_fn_item(
                item,
                &EvalContext::new(generic_args, ctx.module_id),
                inlined,
            )?
            .unwrap()
        } else {
            let mono_item = MonoItem::new(fn_id, generic_args);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                // println!(
                //     "evaluate fn call: fn_id = {:?}, generic_args = {:?}",
                //     fn_id, generic_args
                // );
                let module_id = self
                    .evaluate_fn_item(
                        item,
                        &EvalContext::new(generic_args, ctx.module_id),
                        inlined,
                    )?
                    .unwrap()
                    .module_id();

                self.evaluated_modules.insert(mono_item, module_id);
            }

            (*self.evaluated_modules.get(&mono_item).unwrap()).into()
        };

        self.instantiate_module(id, args, ctx, span)
    }

    pub fn evaluate_impl_fn_call(
        &mut self,
        impl_id: LocalDefId,
        generic_args: GenericArgsRef<'tcx>,
        self_arg: Option<ExprOrItemId<'tcx>>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let impl_item = self.tcx.hir().expect_impl_item(impl_id);
        let inlined = self.is_inlined(impl_id);

        let args = self.eval_fn_args(self_arg, args, ctx)?;

        let id = if inlined {
            self.evaluate_impl_item(
                impl_item,
                &EvalContext::new(generic_args, ctx.module_id),
                true,
            )?
            .unwrap()
        } else {
            let mono_item = MonoItem::new(impl_id, generic_args);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                // println!(
                //     "evaluate impl fn call: impl_id = {:?}, generic_args = {:?}",
                //     impl_id, generic_args
                // );
                let module_id = self
                    .evaluate_impl_item(
                        impl_item,
                        &EvalContext::new(generic_args, ctx.module_id),
                        false,
                    )?
                    .unwrap()
                    .module_id();

                self.evaluated_modules.insert(mono_item, module_id);
            }

            (*self.evaluated_modules.get(&mono_item).unwrap()).into()
        };

        self.instantiate_module(id, args, ctx, span)
    }

    fn is_inlined(&self, did: LocalDefId) -> bool {
        self.tcx
            .get_attrs(did.to_def_id(), RustSymbol::intern("inline"))
            .next()
            .is_some()
    }

    fn evaluate_enum_variant_ctor(
        &mut self,
        variant_ctor_did: DefId,
        ty: Ty<'tcx>,
        ctx: &EvalContext<'tcx>,
        args: &'tcx [Expr<'tcx>],
        span: Span,
    ) -> Result<ItemId, Error> {
        let variant_did = self.tcx.parent(variant_ctor_did);
        self.evaluate_enum_variant(variant_did, ty, ctx, args, span)
    }

    fn evaluate_enum_variant(
        &mut self,
        variant_did: DefId,
        ty: Ty<'tcx>,
        ctx: &EvalContext<'tcx>,
        fields: impl IntoIterator<Item = &'tcx Expr<'tcx>>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let sig_ty = self.find_sig_ty(ty, ctx.generic_args, span)?;
        let enum_ty = sig_ty.enum_ty();

        let enum_did = self.tcx.parent(variant_did);
        let variant_idx = self
            .tcx
            .adt_def(enum_did)
            .variant_index_with_id(variant_did)
            .as_usize();

        let sig_ty = enum_ty.variant(variant_idx);
        let struct_ty = sig_ty
            .inner
            .opt_struct_ty()
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedStructType, span))?;

        let group = self.make_struct_group(struct_ty, fields, |generator, field| {
            generator.evaluate_expr(field, ctx)
        })?;

        Ok(self.enum_variant_to_bitvec(ctx.module_id, enum_ty, variant_idx, group))
    }

    pub fn bin_op(
        &mut self,
        ty: Ty<'tcx>,
        bin_op: BinOp,
        lhs: &'tcx Expr<'tcx>,
        rhs: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let prim_ty = self.find_sig_ty(ty, ctx.generic_args, span)?.prim_ty();

        let lhs_ty = self.node_type(lhs.hir_id, ctx);
        let lhs_prim_ty = self.find_sig_ty(lhs_ty, ctx.generic_args, span)?.prim_ty();

        let rhs_ty = self.node_type(rhs.hir_id, ctx);
        let rhs_prim_ty = self.find_sig_ty(rhs_ty, ctx.generic_args, span)?.prim_ty();

        let subexpr_ty =
            PrimTy::ty_for_bin_expr(lhs_prim_ty, rhs_prim_ty).ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::IncompatibleTypes(
                        lhs_ty.to_string(),
                        rhs_ty.to_string(),
                    ),
                    span,
                )
            })?;

        let mut subnode =
            |expr: &'tcx Expr<'tcx>, prim_ty: PrimTy| -> Result<NodeOutId, Error> {
                let span = expr.span;
                let node = self.evaluate_expr(expr, ctx)?;

                let item_id = if prim_ty != subexpr_ty {
                    StdConversion::convert(
                        SignalTy::new(None, prim_ty.into()),
                        SignalTy::new(None, subexpr_ty.into()),
                        node,
                        self,
                        span,
                    )?
                } else {
                    node
                };
                let node_id = item_id.node_id();

                Ok(self.net_list[node_id]
                    .kind
                    .outputs()
                    .only_one()
                    .node_out_id(node_id))
            };

        let lhs = subnode(lhs, lhs_prim_ty)?;
        let rhs = subnode(rhs, rhs_prim_ty)?;

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

    pub fn method_call(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        fn_item: &'tcx Expr<'tcx>,
        args: &'tcx [Expr<'tcx>],
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let fn_ty = self.node_type(fn_item.hir_id, ctx);
        let fn_id = utils::ty_def_id(fn_ty).unwrap();
        let generic_args = utils::subst(fn_ty);

        match self.find_local_impl_id(fn_id, generic_args) {
            Some((impl_id, generic_args)) => self.evaluate_impl_fn_call(
                impl_id,
                generic_args,
                None,
                args.iter().map(Into::into),
                ctx,
                expr.span,
            ),
            None => {
                let blackbox =
                    self.find_blackbox(fn_id, ctx.generic_args, fn_item.span)?;
                blackbox.evaluate_expr(self, expr, ctx)
            }
        }
    }

    pub fn index(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        ind: u128,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let span = expr.span;
        let expr = self.evaluate_expr(expr, ctx)?;
        match self.item_ty(expr).kind {
            SignalTyKind::Prim(prim) => {
                assert!(ind < prim.width());
                let indexed = self.net_list[expr.node_id()]
                    .kind
                    .outputs()
                    .only_one()
                    .node_out_id(expr.node_id());
                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        Splitter::new(
                            indexed,
                            [(PrimTy::Bit, self.idents.for_module(ctx.module_id).tmp())],
                            Some(ind),
                            false,
                        ),
                    )
                    .into())
            }
            SignalTyKind::Array(array_ty) => {
                assert!(ind < array_ty.count());

                Ok(expr.group().item_ids()[ind as usize])
            }
            _ => Err(SpanError::new(SpanErrorKind::NonIndexableExpr, span).into()),
        }
    }
}
