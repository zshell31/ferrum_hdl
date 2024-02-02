use super::EvalExpr;

pub struct Reverse;

impl<'tcx> EvalExpr<'tcx> for Reverse {
    // fn eval_expr(
    //     &self,
    //     _compiler: &mut compiler<'tcx>,
    //     _expr: &'tcx Expr<'tcx>,
    //     _ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     todo!()
    //     // utils::args!(expr as rec);

    //     // let array_ty = compiler
    //     //     .find_sig_ty(
    //     //         compiler.node_type(rec.hir_id, ctx),
    //     //         ctx.generic_args,
    //     //         rec.span,
    //     //     )?
    //     //     .array_ty();
    //     // let item_ty = array_ty.item_ty();
    //     // let width = item_ty.width();
    //     // let count = array_ty.count();

    //     // let rec = compiler.eval_expr(rec, ctx)?;

    //     // bitvec::bit_vec_trans_in_loop(
    //     //     compiler,
    //     //     rec,
    //     //     ctx,
    //     //     count,
    //     //     |compiler,
    //     //      ctx,
    //     //      LoopArgs {
    //     //          input,
    //     //          output,
    //     //          loop_var,
    //     //      }| {
    //     //         let bitvec_ty = compiler.net_list[input].ty;

    //     //         compiler.net_list.add(ctx.module_id, ExprNode::new(bitvec_ty, input, output, true, move |buffer, input, output| {
    //     //             buffer.write_template(format_args!("assign {output}[{count} - 1 - {loop_var}*{width} +: {width}] = {input}[{loop_var}*{width} +: {width}];"));
    //     //         }));

    //     //         Ok(*item_ty)
    //     //     },
    //     // )
    // }
}

pub struct Map;

impl<'tcx> EvalExpr<'tcx> for Map {
    // fn eval_expr(
    //     &self,
    //     _compiler: &mut compiler<'tcx>,
    //     _expr: &'tcx Expr<'tcx>,
    //     _ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     todo!()
    //     // utils::args!(expr as rec, closure);
    //     // let span = closure.span;

    //     // let array_ty = compiler
    //     //     .find_sig_ty(
    //     //         compiler.node_type(rec.hir_id, ctx),
    //     //         ctx.generic_args,
    //     //         rec.span,
    //     //     )?
    //     //     .array_ty();
    //     // let item_ty = array_ty.item_ty();
    //     // let width = item_ty.width();
    //     // let count = array_ty.count();

    //     // let rec = compiler.eval_expr(rec, ctx)?;

    //     // bitvec::bit_vec_trans_in_loop(
    //     //     compiler,
    //     //     rec,
    //     //     ctx,
    //     //     count,
    //     //     move |compiler,
    //     //           ctx,
    //     //           LoopArgs {
    //     //               input,
    //     //               output,
    //     //               loop_var,
    //     //           }| {
    //     //         let map_in = compiler.idents.for_module(ctx.module_id).ident("map_in");

    //     //         let input = compiler.net_list.add(ctx.module_id, ExprNode::new(PrimTy::BitVec(width), input, map_in, false, move |buffer, input, output| {
    //     //             buffer.write_template(format_args!("assign {output} = {input}[{loop_var}*{width} +: {width}];"));
    //     //         }));
    //     //         let input = compiler.net_list[input]
    //     //             .kind
    //     //             .outputs()
    //     //             .only_one()
    //     //             .node_out_id(input);
    //     //         let input = compiler.from_bitvec(ctx.module_id, input, *item_ty);

    //     //         let closure = compiler.eval_expr(closure, ctx)?;
    //     //         compiler
    //     //             .link_dummy_inputs(&[input], closure)
    //     //             .ok_or_else(|| {
    //     //                 SpanError::new(SpanErrorKind::ExpectedClosure, span)
    //     //             })?;

    //     //         let sig_ty = compiler.item_ty(closure);
    //     //         let width = sig_ty.width();

    //     //         let closure_out = compiler.maybe_to_bitvec(ctx.module_id, closure);
    //     //         let map_out = compiler.idents.for_module(ctx.module_id).ident("map_out");
    //     //         compiler.net_list[closure_out].sym = map_out;

    //     //         compiler.net_list.add(ctx.module_id, ExprNode::new(PrimTy::BitVec(width), closure_out, output, true, move |buffer, input, output| {
    //     //             buffer.write_template(format_args!("assign {output}[{loop_var}*{width} +: {width}] = {input};"));
    //     //         }));

    //     //         Ok(sig_ty)
    //     //     },
    //     // )
    // }
}

pub struct Make;

impl<'tcx> EvalExpr<'tcx> for Make {
    // fn eval_expr(
    //     &self,
    //     compiler: &mut compiler<'tcx>,
    //     expr: &'tcx Expr<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    // ) -> Result<ItemId, Error> {
    //     utils::args!(expr as closure);
    //     let array_ty = compiler
    //         .find_sig_ty(compiler.node_type(expr.hir_id, ctx), ctx, expr.span)?
    //         .array_ty();
    //     let item_ty = array_ty.item_ty();
    //     let count = array_ty.count();

    //     let closure_id =
    //         compiler.eval_closure(closure, Symbol::new("ArrayMap"), true, ctx)?;

    //     let inputs = &compiler.closure(closure_id).inputs;

    //     assert_eq!(inputs.len(), 1);
    //     let input = inputs[0];
    //     let input_ty = compiler.item_ty_old(input).to_bitvec();

    //     compiler.make_array_group(array_ty, 0 .. count, |compiler, idx| {
    //         let input = compiler
    //             .netlist
    //             .const_val(ctx.module_id, input_ty, idx)
    //             .into();
    //         compiler.instantiate_closure(closure_id, [input], item_ty, ctx)
    //     })
    // }
}
