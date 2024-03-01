use std::iter;

use fhdl_netlist::node::{Splitter, SplitterArgs};
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ModuleExt},
        item_ty::ItemTy,
        Compiler, Context, SymIdent,
    },
    error::Error,
};

pub struct BitPackPack;

impl<'tcx> EvalExpr<'tcx> for BitPackPack {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        Ok(ctx.module.to_bitvec(rec))
    }
}

pub struct BitPackUnpack;

impl<'tcx> EvalExpr<'tcx> for BitPackUnpack {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        Ok(ctx.module.from_bitvec(rec, output_ty))
    }
}

pub struct BitPackMsb;

impl<'tcx> EvalExpr<'tcx> for BitPackMsb {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        let rec = ctx.module.to_bitvec(rec);
        let msb = ctx.module.add_and_get_port::<_, Splitter>(SplitterArgs {
            input: rec.port(),
            outputs: iter::once((output_ty.node_ty(), SymIdent::Msb.into())),
            start: None,
            rev: true,
        });

        Ok(ctx.module.from_bitvec(msb, output_ty))
    }
}
