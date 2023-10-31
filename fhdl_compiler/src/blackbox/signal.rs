use fhdl_blackbox::Blackbox;
use fhdl_netlist::{
    bvm::BitVecMask,
    group::ItemId,
    net_list::NodeOutId,
    node::{BinOp, Case, Const, DFF},
    sig_ty::{NodeTy, SignalTy},
};
use rustc_hir::Expr;
use rustc_span::Span;
use smallvec::SmallVec;

use super::EvaluateExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::Generator,
    scopes::SymIdent,
    utils,
};

pub struct SignalReg {
    pub has_en: bool,
}

fn make_err(blackbox: Blackbox, span: Span) -> Error {
    SpanError::new(SpanErrorKind::NotSynthBlackboxExpr(blackbox), span).into()
}

fn signal_value_ty<'tcx>(
    blackbox: Blackbox,
    generator: &mut Generator<'tcx>,
    expr: &'tcx Expr<'tcx>,
    ctx: &EvalContext<'tcx>,
) -> Result<SignalTy, Error> {
    let ty = generator.node_type(expr.hir_id, ctx);
    let value_ty =
        utils::subst_type(ty, 1).ok_or_else(|| make_err(blackbox, expr.span))?;
    generator.find_sig_ty(value_ty, ctx.generic_args, expr.span)
}

impl<'tcx> EvaluateExpr<'tcx> for SignalReg {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let args = utils::expected_call(expr)?;

        let blackbox = if !self.has_en {
            Blackbox::SignalReg
        } else {
            Blackbox::SignalRegEn
        };
        let sig_ty = signal_value_ty(blackbox, generator, expr, ctx)?;
        let (clk, rst, en, rst_val, comb) = match self.has_en {
            true => (&args[0], &args[1], Some(&args[2]), &args[3], &args[4]),
            false => (&args[0], &args[1], None, &args[2], &args[3]),
        };

        let clk = generator.eval_expr(clk, ctx)?.node_out_id();
        let rst = generator
            .eval_expr(rst, ctx)
            .map(|item_id| item_id.node_out_id())?;
        let en = en
            .map(|en| {
                generator
                    .eval_expr(en, ctx)
                    .map(|item_id| item_id.node_out_id())
            })
            .transpose()?;
        let rst_val = generator.eval_expr(rst_val, ctx)?;
        let rst_val = generator.to_bitvec(ctx.module_id, rst_val);
        let prim_ty = sig_ty.to_bitvec();

        let dff = generator.net_list.add(
            ctx.module_id,
            DFF::new(
                prim_ty,
                clk,
                rst,
                en,
                rst_val,
                None,
                if en.is_none() {
                    SymIdent::Dff
                } else {
                    SymIdent::DffEn
                },
            ),
        );

        let dff_out = generator.net_list[dff].only_one_out().node_out_id();
        let dff_out = generator.from_bitvec(ctx.module_id, dff_out, sig_ty);

        ctx.new_closure_inputs().push(dff_out);
        let comb = generator.eval_expr(comb, ctx)?;
        let comb_out = generator.to_bitvec(ctx.module_id, comb);

        generator.net_list.set_dff_data(dff, comb_out);

        Ok(dff_out)
    }
}

pub struct SignalLift;

impl<'tcx> EvaluateExpr<'tcx> for SignalLift {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as arg);

        generator.eval_expr(arg, ctx)
    }
}

pub struct SignalMap;

impl<'tcx> EvaluateExpr<'tcx> for SignalMap {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, comb);

        let rec = generator.eval_expr(rec, ctx)?;
        ctx.new_closure_inputs().push(rec);
        let comb = generator.eval_expr(comb, ctx)?;

        Ok(comb)
    }
}

pub struct SignalAndThen;

impl<'tcx> EvaluateExpr<'tcx> for SignalAndThen {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, comb);

        let rec = generator.eval_expr(rec, ctx)?;
        ctx.new_closure_inputs().push(rec);
        let comb = generator.eval_expr(comb, ctx)?;

        Ok(comb)
    }
}

pub struct SignalApply2;

impl<'tcx> EvaluateExpr<'tcx> for SignalApply2 {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as arg1, arg2, comb);

        let arg1 = generator.eval_expr(arg1, ctx)?;
        let arg2 = generator.eval_expr(arg2, ctx)?;
        ctx.new_closure_inputs().push(arg1).push(arg2);
        let comb = generator.eval_expr(comb, ctx)?;

        Ok(comb)
    }
}

pub struct SignalValue;

impl<'tcx> EvaluateExpr<'tcx> for SignalValue {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct SignalWatch;

impl<'tcx> EvaluateExpr<'tcx> for SignalWatch {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct SignalOp {
    pub op: BinOp,
}

impl<'tcx> EvaluateExpr<'tcx> for SignalOp {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as lhs, rhs);
        let ty = generator.node_type(expr.hir_id, ctx);

        generator.eval_bin_op(ty, self.op, lhs, rhs, ctx, expr.span)
    }
}

pub struct SignalFsm;

impl<'tcx> EvaluateExpr<'tcx> for SignalFsm {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as fsm);

        ctx.new_fsm();
        let res = generator.eval_expr(fsm, ctx)?;
        let states = ctx.take_fsm_states(expr.span).unwrap()?;
        if states.is_empty() {
            return Err(SpanError::new(SpanErrorKind::NoYieldInFsm, expr.span).into());
        }
        let len = states.len();
        let width = len as u128;
        let state_sel = generator.net_list.add_and_get_out(
            ctx.module_id,
            Const::new(NodeTy::BitVec(width), 0, SymIdent::Fsm),
        );

        let variants = states[0 .. len - 1]
            .iter()
            .enumerate()
            .map(|(idx, state)| {
                (
                    BitVecMask::new(idx as u128, width),
                    generator.to_bitvec(ctx.module_id, *state),
                )
            })
            .collect::<SmallVec<[(BitVecMask, NodeOutId); 8]>>();

        let default = states.last().unwrap();

        let sig_ty =
            signal_value_ty(Blackbox::SignalFsm, generator, expr, ctx)?.to_bitvec();

        // let case = generator.net_list.add_and_get_out(
        //     ctx.module_id,
        //     Case::new(sig_ty, state_sel, variants, default, ),
        // );

        todo!()
    }
}
