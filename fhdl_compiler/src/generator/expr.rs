use std::{
    collections::hash_map::Entry,
    convert::identity,
    sync::atomic::{AtomicBool, AtomicU8, Ordering},
};

use fhdl_netlist::{
    group::ItemId,
    net_list::NodeOutId,
    node::{BinOp, BinOpNode, BitNot, Case, Const, Mux2, Not, Splitter},
    sig_ty::{NodeTy, SignalTy, SignalTyKind},
    symbol::Symbol,
};
use if_chain::if_chain;
use rustc_ast::LitKind;
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::{
    def::{CtorOf, DefKind, Res},
    def_id::{DefId, LocalDefId},
    Expr, ExprKind, HirId, PatKind, Path, QPath, StmtKind, Ty as HirTy,
    TyKind as HirTyKind, UnOp,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_middle::ty::{
    AdtKind, GenericArgsRef, InstanceDef, ParamEnv, ParamEnvAnd, Ty, TyKind,
};
use rustc_span::{source_map::Spanned, Span};
use smallvec::SmallVec;

use super::{arg_matcher::ArgMatcher, EvalContext, Generator};
use crate::{
    blackbox::{cast::Conversion, lit},
    error::{Error, SpanError, SpanErrorKind},
    generator::generic::Generics,
    scopes::SymIdent,
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
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match self {
            ExprOrItemId::Expr(expr) => generator.eval_expr(expr, ctx),
            ExprOrItemId::Item(item_id) => Ok(*item_id),
        }
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn make_struct_group_from_exprs(
        &mut self,
        expr: &Expr<'tcx>,
        sub_exprs: impl IntoIterator<Item = &'tcx Expr<'tcx>> + 'tcx,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let sig_ty =
            self.find_sig_ty(self.node_type(expr.hir_id, ctx), ctx, expr.span)?;
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
                    |generator, sub_expr| generator.eval_expr(sub_expr, ctx),
                )
            }
            _ => Err(SpanError::new(SpanErrorKind::ExpectedStructType, expr.span).into()),
        }
    }

    pub fn eval_expr(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let scope = Scope::enter(
            self.net_list[ctx.module_id].name,
            &expr.kind,
            ctx.generic_args,
        );

        let res = self.eval_expr_(expr, ctx);
        if res.is_ok() {
            scope.exit();
        } else {
            scope.inner_most_error(|| {
                println!("{:#?}", expr);
            });
        }

        res
    }

    pub fn eval_expr_(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let expr_ty = self.node_type(expr.hir_id, ctx);

        match expr.kind {
            ExprKind::Array(items) => {
                let array_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?.array_ty();

                self.make_array_group(array_ty, items.iter(), |generator, item| {
                    generator.eval_expr(item, ctx)
                })
            }
            ExprKind::Binary(bin_op, lhs, rhs) => self.bin_op(
                expr_ty,
                utils::to_bin_op(bin_op.node),
                lhs,
                rhs,
                ctx,
                expr.span,
            ),
            ExprKind::Block(block, _) => {
                self.idents.for_module(ctx.module_id).push_scope();

                for stmt in block.stmts {
                    match stmt.kind {
                        StmtKind::Local(local) if local.els.is_none() => {
                            let init = local.init.ok_or_else(|| {
                                SpanError::new(SpanErrorKind::ExpectedExpr, local.span)
                            })?;
                            let item_id = self.eval_expr(init, ctx)?;

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

                let item_id = self.eval_expr(expr, ctx)?;

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
                            if self.is_local_def_id(*fn_did) {
                                let fn_ty = self.node_type(fn_item.hir_id, ctx);
                                let generic_args = utils::subst(fn_ty).unwrap();

                                self.eval_fn_call(
                                    fn_did.expect_local(),
                                    generic_args,
                                    args.iter().map(Into::into),
                                    ctx,
                                    expr.span,
                                )
                            } else {
                                let blackbox = self.find_blackbox(*fn_did, ctx, *span)?;
                                blackbox.eval_expr(self, expr, ctx)
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
                        ) if self.is_local_def_id(*variant_ctor_did) => self
                            .eval_enum_variant_ctor(
                                *variant_ctor_did,
                                expr_ty,
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
                        ) if self.is_local_def_id(*struct_did)
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
                            let ty = self.type_of(*alias_to, ctx);
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

                                self.eval_enum_variant(
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
                            let _ = self.find_sig_ty(expr_ty, ctx, expr.span)?;
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
            ExprKind::Cast(expr, ty) => {
                let from = self.eval_expr(expr, ctx)?;
                let to_ty = self.find_sig_ty_for_hir_ty(
                    expr.hir_id.owner.def_id,
                    ty,
                    ctx,
                    expr.span,
                )?;
                Conversion::convert_as_prim_ty(from, to_ty, self, expr.span)
            }
            ExprKind::Closure(closure) => {
                let body = self.tcx.hir().body(closure.body);
                let inputs = closure.fn_decl.inputs.iter().zip(body.params.iter());

                self.eval_inputs(inputs, ctx, true)?;
                let closure = self.eval_expr(body.value, ctx)?;

                Ok(closure)
            }
            ExprKind::DropTemps(inner) => self.eval_expr(inner, ctx),
            ExprKind::Field(expr, ident) => {
                let group = self.eval_expr(expr, ctx)?.group();

                group.by_field(ident.as_str()).ok_or_else(|| {
                    SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
                })
            }
            ExprKind::If(cond, if_block, else_block) => {
                let sig_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?;

                let else_block = else_block.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedIfElseExpr, expr.span)
                })?;

                let cond = self.eval_expr(cond, ctx)?.node_out_id();
                let if_block = self.eval_expr(if_block, ctx)?;
                let else_block = self.eval_expr(else_block, ctx)?;

                let if_block = self.to_bitvec(ctx.module_id, if_block);
                let else_block = self.to_bitvec(ctx.module_id, else_block);

                let mux = self.net_list.add(
                    ctx.module_id,
                    Mux2::new(
                        sig_ty.to_bitvec(),
                        cond,
                        if_block,
                        else_block,
                        SymIdent::Mux,
                    ),
                );
                let mux = self.net_list[mux].only_one_out().node_out_id();

                Ok(self.from_bitvec(ctx.module_id, mux, sig_ty))
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
                let prim_ty = self.find_sig_ty(expr_ty, ctx, lit.span)?.node_ty();
                let value = lit::eval_lit(prim_ty, lit)?;

                Ok(self
                    .net_list
                    .add_and_get_out(
                        ctx.module_id,
                        Const::new(prim_ty, value.into(), None),
                    )
                    .into())
            }
            ExprKind::Match(sel, arms, _) => {
                let expr_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?;

                let sel = self.eval_expr(sel, ctx)?;
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
                        let item_id = self.eval_expr(arm.body, ctx)?;
                        let node_out_id = self.to_bitvec(ctx.module_id, item_id);
                        default = Some(node_out_id);

                        continue;
                    }

                    match sel_sig_ty.kind {
                        SignalTyKind::Enum(enum_ty) => {
                            let sel = sel.node_out_id();
                            let (variant_def, variant_idx) =
                                self.pattern_to_variant_def(arm.pat, ctx)?;
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
                        SignalTyKind::Node(_)
                        | SignalTyKind::Array(_)
                        | SignalTyKind::Struct(_) => {
                            self.pattern_match(arm.pat, sel, ctx.module_id)?;
                        }
                    }

                    let variant_item_id = self.eval_expr(arm.body, ctx)?;
                    let discr = self.pattern_to_bitvec(arm.pat, sel_sig_ty, ctx)?;
                    if discr.mask < small_mask {
                        small_mask = discr.mask;
                    }

                    let node_out_id = self.to_bitvec(ctx.module_id, variant_item_id);

                    inputs.push((discr, node_out_id));
                }

                let mut sel = self.to_bitvec(ctx.module_id, sel);

                // TODO: move to verilog generation
                let small_mask_ones = small_mask.trailing_ones() as u128;
                if small_mask != 0 && small_mask_ones > 0 {
                    let sel_out = self.net_list[sel];
                    let sel_width = sel_out.width();

                    let new_sel = self.net_list.add(
                        ctx.module_id,
                        Splitter::new(
                            sel,
                            [(
                                NodeTy::BitVec(
                                    (sel_width.value() - small_mask_ones).into(),
                                ),
                                None,
                            )],
                            None,
                            true,
                        ),
                    );
                    sel = self.net_list[new_sel].only_one_out().node_out_id();

                    for (discr, _) in &mut inputs {
                        discr.shiftr(small_mask_ones);
                    }
                }

                let case = self.net_list.add(
                    ctx.module_id,
                    Case::new(expr_ty.to_bitvec(), sel, inputs, default, SymIdent::Mux),
                );
                let case = self.net_list[case].only_one_out().node_out_id();
                Ok(self.from_bitvec(ctx.module_id, case, expr_ty))
            }
            ExprKind::MethodCall(_, rec, args, span) => {
                let fn_did = self
                    .tcx
                    .typeck(expr.hir_id.owner)
                    .type_dependent_def_id(expr.hir_id)
                    .unwrap();
                // TODO: how to define generic args for method call
                let generic_args = self.extract_generic_args(fn_did, expr.hir_id, ctx)?;

                match self.find_local_impl_id(fn_did, generic_args) {
                    Some((impl_id, generic_args)) => self.eval_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some(rec.into()),
                        args.iter().map(Into::into),
                        ctx,
                        expr.span,
                    ),
                    None => {
                        let blackbox = self.find_blackbox(
                            fn_did,
                            &ctx.with_generic_args(generic_args),
                            span,
                        )?;
                        blackbox.eval_expr(self, expr, ctx)
                    }
                }
            }
            ExprKind::Path(QPath::Resolved(_, Path { res, segments, .. })) => match res {
                Res::Local(_) if segments.len() == 1 => {
                    let ident = segments[0].ident;
                    self.item_id_for_ident(ctx.module_id, ident)
                }
                Res::Def(DefKind::AssocFn, def_id) | Res::Def(DefKind::Fn, def_id) => {
                    self.eval_closure_fn_without_params(*def_id, expr, ctx)
                }
                Res::Def(DefKind::Const, def_id) => {
                    if self.is_local_def_id(*def_id) {
                        if let Some(const_val) =
                            self.eval_const_val(*def_id, ctx, Some(expr.span))
                        {
                            let sig_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?;

                            let node = self.net_list.add(
                                ctx.module_id,
                                Const::new(sig_ty.to_bitvec(), const_val.into(), None),
                            );
                            let node_out_id =
                                self.net_list[node].only_one_out().node_out_id();

                            Ok(self.from_bitvec(ctx.module_id, node_out_id, sig_ty))
                        } else {
                            Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span)
                                .into())
                        }
                    } else {
                        let blackbox = self.find_blackbox(*def_id, ctx, expr.span)?;
                        blackbox.eval_expr(self, expr, ctx)
                    }
                }
                Res::Def(DefKind::ConstParam, def_id) => {
                    let parent = self.tcx.parent(*def_id);
                    let generics = Generics::from_ty(
                        self.type_of(parent, ctx),
                        self,
                        ctx,
                        expr.span,
                    )
                    .and_then(|generics| {
                        generics.ok_or_else(|| {
                            SpanError::new(
                                SpanErrorKind::CannotExtractGenericArgs,
                                expr.span,
                            )
                            .into()
                        })
                    })?;

                    let value = self
                        .tcx
                        .generics_of(parent)
                        .param_def_id_to_index(self.tcx, *def_id)
                        .and_then(|ind| generics.as_const(ind as usize))
                        .ok_or_else(|| {
                            SpanError::new(SpanErrorKind::ExpectedConst, expr.span)
                        })?;

                    let node_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?.node_ty();
                    Ok(self
                        .net_list
                        .add_and_get_out(ctx.module_id, Const::new(node_ty, value, None))
                        .into())
                }
                Res::Def(DefKind::Ctor(CtorOf::Variant, ..), variant_ctor_did)
                    if self.is_local_def_id(*variant_ctor_did) =>
                {
                    self.eval_enum_variant_ctor(
                        *variant_ctor_did,
                        expr_ty,
                        ctx,
                        &[],
                        expr.span,
                    )
                }
                _ => Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()),
            },
            ExprKind::Path(qpath @ QPath::TypeRelative(_, _)) => {
                let ty = self.node_type(expr.hir_id, ctx);
                if ty.is_fn() {
                    let fn_did = utils::ty_def_id(ty).unwrap();
                    self.eval_closure_fn_without_params(fn_did, expr, ctx)
                } else {
                    let def_id = self
                        .tcx
                        .typeck(expr.hir_id.owner)
                        .qpath_res(&qpath, expr.hir_id)
                        .opt_def_id();
                    if let Some(def_id) = def_id {
                        if let DefKind::AssocConst = self.tcx.def_kind(def_id) {
                            // let rel_ty = ctx.instantiate(
                            //     self.tcx,
                            //     self.ast_ty_to_ty(expr.hir_id.owner.def_id, rel_ty),
                            // );
                            // let rel_ty = ctx.instantiate(self.tcx, rel_ty);
                            // let args = self.tcx.mk_args(&[ty.into()]);
                            let args =
                                self.extract_generic_args(def_id, expr.hir_id, ctx)?;

                            if let Some(const_val) = self.eval_const_val(
                                def_id,
                                &ctx.with_generic_args(args),
                                Some(expr.span),
                            ) {
                                let prim_ty =
                                    self.find_sig_ty(ty, ctx, expr.span)?.to_bitvec();
                                return Ok(self
                                    .net_list
                                    .add_and_get_out(
                                        ctx.module_id,
                                        Const::new(prim_ty, const_val.into(), None),
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
            ) if self.is_local_def_id(*struct_did) && base.is_none() => self
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
            ) if self.is_local_def_id(*variant_did) && base.is_none() => self
                .eval_enum_variant(
                    *variant_did,
                    expr_ty,
                    ctx,
                    fields.iter().map(|field| field.expr),
                    expr.span,
                ),
            ExprKind::Tup(exprs) => {
                let struct_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?.struct_ty();

                self.make_struct_group(struct_ty, exprs.iter(), |generator, expr| {
                    generator.eval_expr(expr, ctx)
                })
            }
            ExprKind::Unary(UnOp::Not, inner) => {
                let comb = self.eval_expr(inner, ctx)?.node_out_id();
                let prim_ty = self.find_sig_ty(expr_ty, ctx, expr.span)?.node_ty();

                Ok((if prim_ty.is_bool() {
                    self.net_list
                        .add_and_get_out(ctx.module_id, Not::new(prim_ty, comb, None))
                } else {
                    self.net_list
                        .add_and_get_out(ctx.module_id, BitNot::new(prim_ty, comb, None))
                })
                .into())
            }
            _ => Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()),
        }
    }

    pub fn extract_generic_args(
        &self,
        did: DefId,
        hir_id: HirId,
        ctx: &EvalContext<'tcx>,
    ) -> Result<GenericArgsRef<'tcx>, Error> {
        let arg_matcher = ArgMatcher::new(self.tcx);

        let node_args =
            self.subst_with(self.tcx.typeck(hir_id.owner).node_args(hir_id), ctx);

        match utils::subst(self.tcx.type_of(did).instantiate_identity()) {
            Some(generic_args) => arg_matcher
                .extract_params(did, node_args, generic_args)
                .ok_or_else(|| {
                    SpanError::new(
                        SpanErrorKind::CannotExtractGenericArgs,
                        self.tcx.hir().span(hir_id),
                    )
                    .into()
                }),
            None => Ok(node_args),
        }
    }

    pub fn find_local_impl_id(
        &self,
        fn_did: DefId,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Option<(LocalDefId, GenericArgsRef<'tcx>)> {
        self.tcx
            .resolve_instance(ParamEnvAnd {
                param_env: ParamEnv::reveal_all(),
                value: (fn_did, generic_args),
            })
            .ok()
            .and_then(identity)
            .and_then(|instance| match instance.def {
                InstanceDef::Item(def_id)
                    if self.tcx.impl_of_method(def_id).is_some() =>
                {
                    self.local_def_id(def_id)
                        .map(|def_id| (def_id, instance.args))
                }
                _ => None,
            })
    }

    fn eval_enum_variant_ctor(
        &mut self,
        variant_ctor_did: DefId,
        ty: Ty<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        args: &'tcx [Expr<'tcx>],
        span: Span,
    ) -> Result<ItemId, Error> {
        let variant_did = self.tcx.parent(variant_ctor_did);
        self.eval_enum_variant(variant_did, ty, ctx, args, span)
    }

    fn eval_enum_variant(
        &mut self,
        variant_did: DefId,
        ty: Ty<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        fields: impl IntoIterator<Item = &'tcx Expr<'tcx>>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let sig_ty = self.find_sig_ty(ty, ctx, span)?;
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
            generator.eval_expr(field, ctx)
        })?;

        Ok(self.enum_variant_to_bitvec(ctx.module_id, enum_ty, variant_idx, group))
    }

    pub fn bin_op(
        &mut self,
        ty: Ty<'tcx>,
        bin_op: BinOp,
        lhs: &'tcx Expr<'tcx>,
        rhs: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let prim_ty = self.find_sig_ty(ty, ctx, span)?.node_ty();

        let lhs_ty = self.node_type(lhs.hir_id, ctx);
        let lhs_prim_ty = self.find_sig_ty(lhs_ty, ctx, span)?.node_ty();

        let rhs_ty = self.node_type(rhs.hir_id, ctx);
        let rhs_prim_ty = self.find_sig_ty(rhs_ty, ctx, span)?.node_ty();

        let subexpr_ty =
            NodeTy::ty_for_bin_expr(lhs_prim_ty, rhs_prim_ty).ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::IncompatibleTypes(
                        lhs_ty.to_string(),
                        rhs_ty.to_string(),
                    ),
                    span,
                )
            })?;

        let mut subnode =
            |expr: &'tcx Expr<'tcx>, prim_ty: NodeTy| -> Result<NodeOutId, Error> {
                let span = expr.span;
                let node = self.eval_expr(expr, ctx)?;

                let item_id = if prim_ty != subexpr_ty {
                    Conversion::convert_as_prim_ty(
                        node,
                        SignalTy::new(None, subexpr_ty.into()),
                        self,
                        span,
                    )?
                } else {
                    node
                };
                Ok(item_id.node_out_id())
            };

        let lhs = subnode(lhs, lhs_prim_ty)?;
        let rhs = subnode(rhs, rhs_prim_ty)?;

        Ok(self
            .net_list
            .add_and_get_out(
                ctx.module_id,
                BinOpNode::new(prim_ty, bin_op, lhs, rhs, None),
            )
            .into())
    }

    pub fn method_call(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        fn_item: &'tcx Expr<'tcx>,
        args: &'tcx [Expr<'tcx>],
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let fn_ty = self.node_type(fn_item.hir_id, ctx);
        let fn_did = utils::ty_def_id(fn_ty).unwrap();
        let generic_args = utils::subst(fn_ty).unwrap();

        match self.find_local_impl_id(fn_did, generic_args) {
            Some((impl_id, generic_args)) => self.eval_impl_fn_call(
                impl_id,
                generic_args,
                None,
                args.iter().map(Into::into),
                ctx,
                expr.span,
            ),
            None => {
                let blackbox = self.find_blackbox(fn_did, ctx, fn_item.span)?;
                blackbox.eval_expr(self, expr, ctx)
            }
        }
    }

    pub fn index(
        &mut self,
        _expr: &'tcx Expr<'tcx>,
        _ind: u128,
        _ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        todo!()
        // let span = expr.span;
        // let expr = self.eval_expr(expr, ctx)?;
        // match self.item_ty(expr).kind {
        //     SignalTyKind::Node(prim) => {
        //         let indexed = expr.node_out_id();
        //         Ok(self
        //             .net_list
        //             .add_and_get_out(
        //                 ctx.module_id,
        //                 Splitter::new(
        //                     indexed,
        //                     [(NodeTy::Bit, None)],
        //                     Some(ind.into()),
        //                     false,
        //                 ),
        //             )
        //             .into())
        //     }
        //     SignalTyKind::Array(array_ty) => Ok(expr.group().item_ids()[ind as usize]),
        //     _ => Err(SpanError::new(SpanErrorKind::NonIndexableExpr, span).into()),
        // }
    }
}
