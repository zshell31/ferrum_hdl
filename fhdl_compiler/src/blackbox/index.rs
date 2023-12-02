use fhdl_netlist::{
    group::ItemId,
    net_list::NodeOutId,
    node::{BinOp, BinOpNode},
};
use rustc_hir::Expr;
use rustc_span::Span;

use super::{Blackbox, EvalExpr};
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct UnsignedIndex;

impl<'tcx> EvalExpr<'tcx> for UnsignedIndex {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let generics = generator.method_call_generics(expr, ctx)?;
        let ind: u128 = generics.as_const(1).unwrap();

        generator.index(rec, ind, ctx)
    }
}

fn is_max<'tcx>(
    generator: &mut Generator<'tcx>,
    rec: &'tcx Expr<'tcx>,
    ctx: &mut EvalContext<'tcx>,
    span: Span,
) -> Result<NodeOutId, Error> {
    let ty = generator.node_type(rec.hir_id, ctx);
    let sig_ty_info = generator.find_sig_ty_info(ty, ctx.generic_args, span)?;
    let n: u128 = sig_ty_info.ty_or_def_id.generic_const(0).unwrap();

    let ty = sig_ty_info.sig_ty.node_ty();
    let rec = generator.eval_expr(rec, ctx)?.node_out_id();

    let n = generator
        .net_list
        .const_val(ctx.module_id, ty, n.saturating_sub(1));

    Ok(generator
        .net_list
        .add_and_get_out(ctx.module_id, BinOpNode::new(ty, BinOp::Eq, rec, n, None)))
}

fn add_op<'tcx>(
    generator: &mut Generator<'tcx>,
    expr: &'tcx Expr<'tcx>,
    ctx: &mut EvalContext<'tcx>,
    bin_op: BinOp,
) -> Result<ItemId, Error> {
    utils::args!(expr as rec);

    let rec = generator.eval_expr(rec, ctx)?;
    let ty = generator.item_ty(rec).node_ty();

    let one = generator.net_list.const_one(ctx.module_id, ty);
    Ok(generator
        .net_list
        .add_and_get_out(
            ctx.module_id,
            BinOpNode::new(ty, bin_op, rec.node_out_id(), one, None),
        )
        .into())
}

pub struct IdxDefault;

impl<'tcx> EvalExpr<'tcx> for IdxDefault {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let ty = generator.node_type(expr.hir_id, ctx);
        let ty = generator
            .find_sig_ty(ty, ctx.generic_args, expr.span)?
            .node_ty();

        Ok(generator.net_list.const_zero(ctx.module_id, ty).into())
    }
}

pub struct IdxVal;

impl<'tcx> EvalExpr<'tcx> for IdxVal {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct IdxSucc;

impl<'tcx> EvalExpr<'tcx> for IdxSucc {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let is_max = is_max(generator, rec, ctx, expr.span)?;

        add_op(generator, expr, ctx, BinOp::Add)
    }
}

pub struct IdxPred;

impl<'tcx> EvalExpr<'tcx> for IdxPred {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        add_op(generator, expr, ctx, BinOp::Sub)
    }
}

pub struct IdxIsMax;

impl<'tcx> EvalExpr<'tcx> for IdxIsMax {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let ty = generator.node_type(rec.hir_id, ctx);
        let sig_ty_info = generator.find_sig_ty_info(ty, ctx.generic_args, expr.span)?;
        let n: u128 = sig_ty_info.ty_or_def_id.generic_const(0).unwrap();

        let ty = sig_ty_info.sig_ty.node_ty();
        let rec = generator.eval_expr(rec, ctx)?.node_out_id();

        let n = generator
            .net_list
            .const_val(ctx.module_id, ty, n.saturating_sub(1));

        Ok(generator
            .net_list
            .add_and_get_out(ctx.module_id, BinOpNode::new(ty, BinOp::Eq, rec, n, None))
            .into())
    }
}

pub struct IdxIsMin;

impl<'tcx> EvalExpr<'tcx> for IdxIsMin {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?;
        let ty = generator.item_ty(rec).node_ty();
        let zero = generator.net_list.const_zero(ctx.module_id, ty);

        Ok(generator
            .net_list
            .add_and_get_out(
                ctx.module_id,
                BinOpNode::new(ty, BinOp::Eq, rec.node_out_id(), zero, None),
            )
            .into())
    }
}
