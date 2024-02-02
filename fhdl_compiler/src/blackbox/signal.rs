use fhdl_netlist::node::DFF;
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context, SymIdent},
    error::Error,
    utils,
};

pub struct SignalReg {
    pub has_en: bool,
}

impl<'tcx> EvalExpr<'tcx> for SignalReg {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let mod_id = ctx.module_id;

        let (clk, rst, en, rst_val, comb) = match self.has_en {
            true => (&args[0], &args[1], Some(&args[2]), &args[3], &args[4]),
            false => (&args[0], &args[1], None, &args[2], &args[3]),
        };

        let clk = clk.node_out_id();
        let rst = compiler.to_bitvec(mod_id, rst).node_out_id();
        let en = en.map(|en| compiler.to_bitvec(mod_id, en).node_out_id());
        let rst_val = compiler.to_bitvec(mod_id, rst_val).node_out_id();

        compiler.set_mod_name(comb, "SignalReg", ctx);

        let dff = compiler.netlist.add(
            ctx.module_id,
            DFF::new(
                output_ty.to_bitvec(),
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

        let dff_out = compiler.netlist[dff].only_one_out().node_out_id();
        let dff_out = compiler.from_bitvec(ctx.module_id, dff_out, output_ty);

        let comb = compiler.instantiate_closure(comb, &[dff_out.clone()], ctx, span)?;
        assert_eq!(comb.ty, output_ty);

        let comb_out = compiler.to_bitvec(ctx.module_id, &comb).node_out_id();
        compiler.netlist.set_dff_data(dff, comb_out);

        Ok(dff_out)
    }
}

pub struct SignalMap;

impl<'tcx> EvalExpr<'tcx> for SignalMap {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec, comb);

        compiler.set_mod_name(comb, "SignalMap", ctx);

        compiler.instantiate_closure(comb, &[rec.clone()], ctx, span)
    }
}

pub struct SignalAndThen;

impl<'tcx> EvalExpr<'tcx> for SignalAndThen {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec, comb);

        compiler.set_mod_name(comb, "SignalAndThen", ctx);

        compiler.instantiate_closure(comb, &[rec.clone()], ctx, span)
    }
}

pub struct SignalApply2;

impl<'tcx> EvalExpr<'tcx> for SignalApply2 {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as arg1, arg2, comb);

        compiler.set_mod_name(comb, "SignalApply2", ctx);

        compiler.instantiate_closure(comb, &[arg1.clone(), arg2.clone()], ctx, span)
    }
}
