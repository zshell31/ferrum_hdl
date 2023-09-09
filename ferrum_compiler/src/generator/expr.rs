use ferrum_netlist::{
    arena::with_arena,
    group_list::{Group, GroupKind, ItemId},
    net_list::NodeId,
    node::{
        BinOp, BinOpNode, BitNotNode, ConstNode, IsNode, ModInst, Mux2Node, Node,
        NotNode, PassNode, Splitter,
    },
    params::Outputs,
    sig_ty::PrimTy,
};
use rustc_ast::{BorrowKind, LitKind, Mutability};
use rustc_hir::{def::Res, BinOpKind, Expr, ExprKind, Path, QPath, StmtKind, UnOp};
use rustc_span::source_map::Spanned;

use super::{EvalContext, Generator};
use crate::{
    bitvec::ArrayDesc,
    blackbox::{self, bit_vec_trans, BitVecTransArgs, EvaluateExpr},
    error::{Error, SpanError, SpanErrorKind},
};

impl<'tcx> Generator<'tcx> {
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
}
