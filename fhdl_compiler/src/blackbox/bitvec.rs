use fhdl_netlist::{net_list::NodeOutId, node::Splitter, symbol::Symbol};
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    error::Error,
    eval_context::EvalContext,
    generator::{item::Item, item_ty::ItemTy, Generator},
    utils,
};

pub fn bit_vec_trans<'tcx>(
    generator: &mut Generator<'tcx>,
    source: &Item<'tcx>,
    ctx: &EvalContext<'tcx>,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        NodeOutId,
    ) -> Result<(NodeOutId, ItemTy<'tcx>), Error>,
) -> Result<Item<'tcx>, Error> {
    let bit_vec = generator.to_bitvec(ctx.module_id, source, &ctx.locals);

    let (res, item_ty) = trans(generator, ctx, bit_vec.node_out_id())?;

    let from = generator.from_bitvec(ctx.module_id, res, item_ty);

    Ok(from)
}

pub struct BitVecShrink;

impl<'tcx> EvalExpr<'tcx> for BitVecShrink {
    // fn eval_expr(
    //     &self,
    //     generator: &mut Generator<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as rec);

    //     let rec = generator.eval_expr(rec, ctx)?;

    //     let ty = generator.node_type(expr.hir_id, ctx);
    //     let width = generator.find_sig_ty(ty, ctx, expr.span)?.width();

    //     let rec = generator.to_bitvec(ctx.module_id, rec);

    //     Ok(generator
    //         .netlist
    //         .add_and_get_out(
    //             ctx.module_id,
    //             Splitter::new(rec, [(NodeTy::BitVec(width), None)], None, false),
    //         )
    //         .into())
    // }
}

pub struct BitVecSlice;

impl<'tcx> EvalExpr<'tcx> for BitVecSlice {
    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);
        let rec = rec.node_out_id();

        let fn_ty = generator.type_of(ctx.fn_did, ctx.generic_args);

        let start = generator
            .generic_const(fn_ty, 1, ctx.generic_args, span)?
            .unwrap();

        let sym = generator.netlist[rec]
            .sym
            .map(|sym| Symbol::new_from_args(format_args!("{}_slice", sym)));

        Ok(Item::new(
            output_ty,
            generator.netlist.add_and_get_out(
                ctx.module_id,
                Splitter::new(rec, [(output_ty.to_bitvec(), sym)], Some(start), false),
            ),
        ))
    }
}

pub struct BitVecUnpack;

impl<'tcx> EvalExpr<'tcx> for BitVecUnpack {
    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);

        Ok(generator.from_bitvec(ctx.module_id, rec, output_ty))
    }
}
