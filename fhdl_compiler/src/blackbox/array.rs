use fhdl_netlist::{group::ItemId, symbol::Symbol};
use rustc_hir::Expr;

use super::EvalExpr;
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub struct Reverse;

impl<'tcx> EvalExpr<'tcx> for Reverse {
    fn eval_expr(
        &self,
        _generator: &mut Generator<'tcx>,
        _expr: &'tcx Expr<'tcx>,
        _ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        todo!()
        // utils::args!(expr as rec);

        // let array_ty = generator
        //     .find_sig_ty(
        //         generator.node_type(rec.hir_id, ctx),
        //         ctx.generic_args,
        //         rec.span,
        //     )?
        //     .array_ty();
        // let item_ty = array_ty.item_ty();
        // let width = item_ty.width();
        // let count = array_ty.count();

        // let rec = generator.eval_expr(rec, ctx)?;

        // bitvec::bit_vec_trans_in_loop(
        //     generator,
        //     rec,
        //     ctx,
        //     count,
        //     |generator,
        //      ctx,
        //      LoopArgs {
        //          input,
        //          output,
        //          loop_var,
        //      }| {
        //         let bitvec_ty = generator.net_list[input].ty;

        //         generator.net_list.add(ctx.module_id, ExprNode::new(bitvec_ty, input, output, true, move |buffer, input, output| {
        //             buffer.write_template(format_args!("assign {output}[{count} - 1 - {loop_var}*{width} +: {width}] = {input}[{loop_var}*{width} +: {width}];"));
        //         }));

        //         Ok(*item_ty)
        //     },
        // )
    }
}

pub struct Map;

impl<'tcx> EvalExpr<'tcx> for Map {
    fn eval_expr(
        &self,
        _generator: &mut Generator<'tcx>,
        _expr: &'tcx Expr<'tcx>,
        _ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        todo!()
        // utils::args!(expr as rec, closure);
        // let span = closure.span;

        // let array_ty = generator
        //     .find_sig_ty(
        //         generator.node_type(rec.hir_id, ctx),
        //         ctx.generic_args,
        //         rec.span,
        //     )?
        //     .array_ty();
        // let item_ty = array_ty.item_ty();
        // let width = item_ty.width();
        // let count = array_ty.count();

        // let rec = generator.eval_expr(rec, ctx)?;

        // bitvec::bit_vec_trans_in_loop(
        //     generator,
        //     rec,
        //     ctx,
        //     count,
        //     move |generator,
        //           ctx,
        //           LoopArgs {
        //               input,
        //               output,
        //               loop_var,
        //           }| {
        //         let map_in = generator.idents.for_module(ctx.module_id).ident("map_in");

        //         let input = generator.net_list.add(ctx.module_id, ExprNode::new(PrimTy::BitVec(width), input, map_in, false, move |buffer, input, output| {
        //             buffer.write_template(format_args!("assign {output} = {input}[{loop_var}*{width} +: {width}];"));
        //         }));
        //         let input = generator.net_list[input]
        //             .kind
        //             .outputs()
        //             .only_one()
        //             .node_out_id(input);
        //         let input = generator.from_bitvec(ctx.module_id, input, *item_ty);

        //         let closure = generator.eval_expr(closure, ctx)?;
        //         generator
        //             .link_dummy_inputs(&[input], closure)
        //             .ok_or_else(|| {
        //                 SpanError::new(SpanErrorKind::ExpectedClosure, span)
        //             })?;

        //         let sig_ty = generator.item_ty(closure);
        //         let width = sig_ty.width();

        //         let closure_out = generator.maybe_to_bitvec(ctx.module_id, closure);
        //         let map_out = generator.idents.for_module(ctx.module_id).ident("map_out");
        //         generator.net_list[closure_out].sym = map_out;

        //         generator.net_list.add(ctx.module_id, ExprNode::new(PrimTy::BitVec(width), closure_out, output, true, move |buffer, input, output| {
        //             buffer.write_template(format_args!("assign {output}[{loop_var}*{width} +: {width}] = {input};"));
        //         }));

        //         Ok(sig_ty)
        //     },
        // )
    }
}

pub struct Index;

impl<'tcx> EvalExpr<'tcx> for Index {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, idx);

        let idx = generator.eval_expr(idx, ctx)?;

        generator.index(rec, idx, ctx)
    }
}

pub struct Make;

impl<'tcx> EvalExpr<'tcx> for Make {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as closure);
        let array_ty = generator
            .find_sig_ty(generator.node_type(expr.hir_id, ctx), ctx, expr.span)?
            .array_ty();
        let item_ty = array_ty.item_ty();
        let count = array_ty.count();

        let closure_id =
            generator.eval_closure(closure, Symbol::new("ArrayMap"), true, ctx)?;

        let inputs = &generator.closure(closure_id).inputs;

        assert_eq!(inputs.len(), 1);
        let input = inputs[0];
        let input_ty = generator.item_ty(input).to_bitvec();

        generator.make_array_group(array_ty, 0 .. count, |generator, idx| {
            let input = generator
                .netlist
                .const_val(ctx.module_id, input_ty, idx)
                .into();
            generator.instantiate_closure(closure_id, [input], item_ty, ctx)
        })
    }
}
