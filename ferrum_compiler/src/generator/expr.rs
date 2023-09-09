use std::sync::atomic::{AtomicU8, Ordering};

use ferrum_netlist::{
    arena::with_arena,
    group_list::{Group, GroupKind, ItemId, Named},
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{
        BinOp, BinOpNode, BitNotNode, ConstNode, IsNode, ModInst, Mux2Node, Node,
        NotNode, PassNode, Splitter,
    },
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
    symbol::Symbol,
};
use rustc_ast::LitKind;
use rustc_hir::{def::Res, BinOpKind, Expr, ExprKind, Path, QPath, StmtKind, UnOp};
use rustc_span::{source_map::Spanned, symbol::Ident};

use super::{EvalContext, Generator};
use crate::{
    bitvec::ArrayDesc,
    blackbox::{self, bit_vec_trans, BitVecTransArgs, EvaluateExpr},
    error::{Error, SpanError, SpanErrorKind},
};

static IDENT: AtomicU8 = AtomicU8::new(0);

struct Scope<'tcx>(&'tcx ExprKind<'tcx>);

impl<'tcx> Scope<'tcx> {
    fn enter(kind: &'tcx ExprKind<'tcx>) -> Self {
        let ident = AtomicU8::fetch_add(&IDENT, 1, Ordering::Relaxed) as usize;
        println!("{}--> {}", " ".repeat(ident << 2), Self::to_str(kind));
        Self(kind)
    }

    fn exit(self) {
        let ident = AtomicU8::fetch_sub(&IDENT, 1, Ordering::Relaxed) as usize;
        println!(
            "{}<-- {}",
            " ".repeat((ident - 1) << 2),
            Self::to_str(self.0)
        );
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

impl<'tcx> Generator<'tcx> {
    pub fn make_tuple_group<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().enumerate().map(
                |(ind, item)| {
                    f(self, item).map(|item_id| {
                        Named::new(item_id, Some(Symbol::new_from_ind(ind)))
                    })
                },
            ))?
        };

        Ok(self
            .group_list
            .add_group(Group::new_with_item_ids(GroupKind::Group, group))
            .into())
    }

    pub fn make_tuple_sig_ty<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<SignalTy, Error>,
    ) -> Result<SignalTy, Error> {
        let ty = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().enumerate().map(
                |(ind, item)| {
                    f(self, item)
                        .map(|sig_ty| Named::new(sig_ty, Some(Symbol::new_from_ind(ind))))
                },
            ))?
        };

        Ok(SignalTy::Group(ty))
    }

    pub fn make_array_group<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(
                iter.into_iter()
                    .map(|item| f(self, item).map(|item_id| Named::new(item_id, None))),
            )?
        };

        Ok(self
            .group_list
            .add_group(Group::new_with_item_ids(GroupKind::Array, group))
            .into())
    }

    pub fn make_struct_group<T>(
        &mut self,
        iter: impl IntoIterator<Item = (Option<Symbol>, T)>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().map(|(sym, item)| {
                f(self, item).map(|item_id| Named::new(item_id, sym))
            }))?
        };

        Ok(self
            .group_list
            .add_group(Group::new_with_item_ids(GroupKind::Group, group))
            .into())
    }

    pub fn make_struct_sig_ty<T>(
        &mut self,
        iter: impl IntoIterator<Item = (Ident, T)>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<SignalTy, Error>,
    ) -> Result<SignalTy, Error> {
        let ty = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().map(
                |(ident, item): (Ident, T)| {
                    f(self, item).map(|sig_ty| {
                        Named::new(sig_ty, Some(Symbol::new(ident.as_str())))
                    })
                },
            ))?
        };

        Ok(SignalTy::Group(ty))
    }

    pub fn evaluate_expr(
        &mut self,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let ty = self.node_type(expr.hir_id);

        let scope = Scope::enter(&expr.kind);

        let res = match expr.kind {
            ExprKind::Array(items) => self
                .make_array_group(items.iter(), |generator, item| {
                    generator.evaluate_expr(item, ctx)
                }),
            ExprKind::Binary(bin_op, lhs, rhs) => {
                let prim_ty = self.find_sig_ty(ty, ctx.generics, expr.span)?.prim_ty();

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

                                        self.make_tuple_group(
                                            nodes,
                                            |generator, node| {
                                                Ok(generator
                                                    .net_list
                                                    .add_node(ctx.module_id, node)
                                                    .into())
                                            },
                                        )?
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
                if let ExprKind::Path(path) = rec.kind {
                    match path {
                        QPath::Resolved(_, Path { span, res, .. }) => {
                            let def_id = res.def_id();
                            if def_id.is_local() {
                                let ty = self.node_type(rec.hir_id);
                                let generics = self.generics(&ty);
                                let item =
                                    self.tcx.hir().expect_item(def_id.expect_local());
                                let module_id = self
                                    .evaluate_fn_item(item, generics, false)?
                                    .unwrap();

                                self.instantiate_module(
                                    module_id,
                                    None,
                                    args,
                                    EvalContext {
                                        generics,
                                        module_id: ctx.module_id,
                                    },
                                )
                            } else {
                                let blackbox =
                                    self.find_blackbox(def_id, ctx.generics, *span)?;
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
                                self.find_blackbox(def_id, ctx.generics, rec.span)?;
                            blackbox.evaluate_expr(self, expr, ctx)
                        }
                        _ => {
                            println!("{:?}", expr);
                            Err(SpanError::new(SpanErrorKind::NotSynthCall, rec.span)
                                .into())
                        }
                    }
                } else {
                    Err(SpanError::new(SpanErrorKind::NotSynthCall, expr.span).into())
                }
            }
            ExprKind::Closure(closure) => {
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

                let res = self.group_list[group_id]
                    .by_field(ident.as_str())
                    .ok_or_else(|| {
                        SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
                    });
                if res.is_err() {
                    println!("field: {}", ident);
                }
                res
            }
            ExprKind::If(cond, if_block, else_block) => {
                let prim_ty = self.find_sig_ty(ty, ctx.generics, expr.span)?.prim_ty();

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
                let prim_ty = self.find_sig_ty(ty, ctx.generics, lit.span)?.prim_ty();
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
            ExprKind::MethodCall(_, rec, args, span) => {
                let def_id = self.type_dependent_def_id(expr.hir_id, span)?;

                if def_id.is_local() {
                    let rec = self.evaluate_expr(rec, ctx)?;

                    let impl_item =
                        self.tcx.hir().expect_impl_item(def_id.expect_local());
                    let module_id =
                        self.evaluate_impl_item(impl_item, ctx.generics)?.unwrap();

                    self.instantiate_module(module_id, Some(rec), args, ctx)
                } else {
                    let blackbox = self.find_blackbox(def_id, ctx.generics, span)?;
                    blackbox.evaluate_expr(self, expr, ctx)
                }
            }
            ExprKind::Path(QPath::Resolved(
                _,
                Path {
                    res: Res::Local(_),
                    segments,
                    ..
                },
            )) if segments.len() == 1 => {
                let ident = segments[0].ident;
                self.item_id_for_ident(ctx.module_id, ident)
            }
            ExprKind::Tup(exprs) => self
                .make_tuple_group(exprs.iter(), |generator, expr| {
                    generator.evaluate_expr(expr, ctx)
                }),
            ExprKind::Unary(UnOp::Not, inner) => {
                let comb = self.evaluate_expr(inner, ctx)?.node_id();
                let prim_ty = self.find_sig_ty(ty, ctx.generics, expr.span)?.prim_ty();
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
        };

        if res.is_ok() {
            scope.exit();
        }

        res
    }

    pub fn instantiate_module(
        &mut self,
        module_id: ModuleId,
        self_arg: Option<ItemId>,
        args: &[Expr<'tcx>],
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let mut inputs = Vec::new();

        if let Some(item_id) = self_arg {
            self.add_module_input(item_id, &mut inputs);
        }

        for arg in args {
            let item_id = self.evaluate_expr(arg, ctx)?;
            self.add_module_input(item_id, &mut inputs);
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
                ModInst::new(inst_sym, module_id, inputs, outputs),
            )
            .into())
    }

    fn add_module_input(&self, item_id: ItemId, inputs: &mut Vec<NodeOutId>) {
        let mut evaluate = |_, node_id: NodeId| {
            for out in self.net_list[node_id].outputs().items() {
                inputs.push(out.node_out_id(node_id));
            }

            Result::<(), Error>::Ok(())
        };

        self.group_list
            .deep_iter(&[item_id], &mut evaluate)
            .unwrap();
    }
}
