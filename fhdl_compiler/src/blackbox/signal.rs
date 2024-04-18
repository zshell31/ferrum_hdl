use ferrum_hdl::domain::{Polarity, SyncKind};
use fhdl_netlist::node::{DFFArgs, TyOrData, DFF};
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Group, Item, ItemKind, ModuleExt},
        item_ty::ItemTy,
        Compiler, Context, SymIdent,
    },
    error::{Error, SpanError, SpanErrorKind},
};

pub struct SignalDff {
    pub comb: bool,
}

impl<'tcx> EvalExpr<'tcx> for SignalDff {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as clk, rst, en, init, comb, rst_kind, rst_pol);

        let clk = clk.port();
        let rst = ctx.module.to_bitvec(rst).port();
        let en = ctx.module.to_bitvec(en).port();
        let init = ctx.module.to_bitvec(init).port();

        let (dff_ty, comb_ty) = if self.comb {
            let struct_ty = output_ty.struct_ty();
            (struct_ty.by_idx(0), struct_ty.by_idx(1))
        } else {
            (output_ty, output_ty)
        };

        let rst_kind = ctx
            .module
            .to_const_val(rst_kind)
            .and_then(SyncKind::from_val)
            .ok_or_else(|| SpanError::new(SpanErrorKind::InvalidResetKind, span))?;

        let rst_pol = ctx
            .module
            .to_const_val(rst_pol)
            .and_then(Polarity::from_val)
            .ok_or_else(|| SpanError::new(SpanErrorKind::InvalidResetPolarity, span))?;

        let dff = ctx.module.add_and_get_port::<_, DFF>(DFFArgs {
            clk,
            rst: Some(rst),
            rst_kind,
            rst_pol,
            en: Some(en),
            init,
            data: TyOrData::Ty(dff_ty.to_bitvec()),
            sym: SymIdent::Reg.into(),
        });
        let dff_out = ctx.module.from_bitvec(dff, dff_ty);

        let comb = compiler.instantiate_closure(comb, &[dff_out.clone()], ctx, span)?;
        assert_eq!(comb.ty, comb_ty);
        ctx.module.assign_names_to_item("comb", &comb, false);

        let comb_out = ctx.module.to_bitvec(&comb).port();
        DFF::set_data(&mut ctx.module, dff.node, comb_out);

        Ok(if self.comb {
            Item::new(output_ty, ItemKind::Group(Group::new([dff_out, comb])))
        } else {
            dff_out
        })
    }
}

pub struct Map;

impl<'tcx> EvalExpr<'tcx> for Map {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec, comb);

        compiler.instantiate_closure(comb, &[rec.clone()], ctx, span)
    }
}

pub struct AndThen;

impl<'tcx> EvalExpr<'tcx> for AndThen {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec, comb);

        compiler.instantiate_closure(comb, &[rec.clone()], ctx, span)
    }
}

pub struct Apply2;

impl<'tcx> EvalExpr<'tcx> for Apply2 {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as arg1, arg2, comb);

        compiler.instantiate_closure(comb, &[arg1.clone(), arg2.clone()], ctx, span)
    }
}
