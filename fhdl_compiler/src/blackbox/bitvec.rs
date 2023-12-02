use fhdl_netlist::{
    group::ItemId,
    net_list::{NodeId, NodeOutId},
    node::Splitter,
    sig_ty::{NodeTy, SignalTy},
    symbol::Symbol,
};
use rustc_hir::Expr;

use super::{Blackbox, EvaluateExpr};
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub fn bit_vec_trans<'tcx>(
    generator: &mut Generator<'tcx>,
    source: ItemId,
    ctx: &EvalContext<'tcx>,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        NodeOutId,
    ) -> Result<(NodeId, SignalTy), Error>,
) -> Result<ItemId, Error> {
    let bit_vec = generator.to_bitvec(ctx.module_id, source);

    let (trans, sig_ty) = trans(generator, ctx, bit_vec)?;
    let trans = generator.net_list[trans].only_one_out().node_out_id();

    let from = generator.from_bitvec(ctx.module_id, trans, sig_ty);

    Ok(from)
}

#[allow(dead_code)]
pub struct LoopArgs {
    pub input: NodeOutId,
    pub output: Option<Symbol>,
    pub loop_var: Symbol,
}

#[allow(dead_code)]
pub fn bit_vec_trans_in_loop<'tcx>(
    _generator: &mut Generator<'tcx>,
    _source: ItemId,
    _ctx: &EvalContext<'tcx>,
    _count: u128,
    _trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        LoopArgs,
    ) -> Result<SignalTy, Error>,
) -> Result<ItemId, Error> {
    todo!()
    // bit_vec_trans(generator, source, ctx, |generator, ctx, bit_vec| {
    //     let loop_var = Symbol::new("i");
    //     let output = None;

    //     let loop_id = generator
    //         .net_list
    //         .add(ctx.module_id, LoopStart::new(loop_var, count, None));

    //     let sig_ty = trans(generator, ctx, LoopArgs {
    //         input: bit_vec,
    //         output,
    //         loop_var,
    //     })?;
    //     let width = sig_ty.width();

    //     generator.net_list.add(ctx.module_id, LoopEnd {});

    //     if let NodeKind::LoopStart(node) = &mut generator.net_list[loop_id].kind {
    //         node.set_out(Some((NodeTy::BitVec(count * width), output)));
    //     }

    //     Ok((loop_id, SignalTy::mk_array(None, count, sig_ty)))
    // })
}

pub struct BitVecShrink;

impl<'tcx> EvaluateExpr<'tcx> for BitVecShrink {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?;

        let ty = generator.node_type(expr.hir_id, ctx);
        let width = generator
            .find_sig_ty(ty, ctx.generic_args, expr.span)?
            .width();

        let rec = generator.to_bitvec(ctx.module_id, rec);

        Ok(generator
            .net_list
            .add_and_get_out(
                ctx.module_id,
                Splitter::new(rec, [(NodeTy::BitVec(width), None)], None, false),
            )
            .into())
    }
}

pub struct BitVecSlice;

impl<'tcx> EvaluateExpr<'tcx> for BitVecSlice {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?.node_out_id();

        let generics = generator.method_call_generics(expr, ctx)?;

        let start = generics.as_const(1).unwrap();
        let width = generics.as_const(2).unwrap();

        let sym = generator.net_list[rec]
            .sym
            .map(|sym| Symbol::new_from_args(format_args!("{}_slice", sym)));

        Ok(generator
            .net_list
            .add_and_get_out(
                ctx.module_id,
                Splitter::new(rec, [(NodeTy::BitVec(width), sym)], Some(start), false),
            )
            .into())
    }
}

pub struct BitVecUnpack;

impl<'tcx> EvaluateExpr<'tcx> for BitVecUnpack {
    fn eval_expr(
        &self,
        _: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?.node_out_id();

        let ty = generator.node_type(expr.hir_id, ctx);
        let sig_ty = generator.find_sig_ty(ty, ctx.generic_args, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, rec, sig_ty))
    }
}
