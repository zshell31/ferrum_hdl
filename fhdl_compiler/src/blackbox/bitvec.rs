use fhdl_netlist::{net_list::NodeOutId, node::Splitter, symbol::Symbol};
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context},
    error::Error,
    utils,
};

pub fn bit_vec_trans<'tcx>(
    compiler: &mut Compiler<'tcx>,
    source: &Item<'tcx>,
    ctx: &Context<'tcx>,
    trans: impl FnOnce(
        &mut Compiler<'tcx>,
        &Context<'tcx>,
        NodeOutId,
    ) -> Result<(NodeOutId, ItemTy<'tcx>), Error>,
) -> Result<Item<'tcx>, Error> {
    let bit_vec = compiler.to_bitvec(ctx.module_id, source);

    let (res, item_ty) = trans(compiler, ctx, bit_vec.node_out_id())?;

    let from = compiler.from_bitvec(ctx.module_id, res, item_ty);

    Ok(from)
}

pub struct BitVecShrink;

impl<'tcx> EvalExpr<'tcx> for BitVecShrink {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as rec);

    //     let rec = compiler.eval_expr(rec, ctx)?;

    //     let ty = compiler.node_type(expr.hir_id, ctx);
    //     let width = compiler.find_sig_ty(ty, ctx, expr.span)?.width();

    //     let rec = compiler.to_bitvec(ctx.module_id, rec);

    //     Ok(compiler
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
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);
        let rec = rec.node_out_id();

        let fn_ty = compiler.type_of(ctx.fn_did, ctx.generic_args);

        let start = compiler
            .generic_const(fn_ty, 0, ctx.generic_args, span)?
            .unwrap();

        let sym = compiler.netlist[rec]
            .sym
            .map(|sym| Symbol::new_from_args(format_args!("{}_slice", sym)));

        Ok(Item::new(
            output_ty,
            compiler.netlist.add_and_get_out(
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
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);

        Ok(compiler.from_bitvec(ctx.module_id, rec, output_ty))
    }
}
