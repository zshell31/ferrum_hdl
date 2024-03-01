use std::iter;

use fhdl_netlist::{
    node::{Splitter, SplitterArgs},
    symbol::Symbol,
};
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ModuleExt},
        item_ty::ItemTy,
        Compiler, Context,
    },
    error::Error,
};

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
        args!(args as rec);
        let rec = rec.port();

        let fn_ty = compiler.type_of(ctx.fn_did, ctx.generic_args);

        let start = compiler
            .generic_const(fn_ty, 0, ctx.generic_args, span)?
            .unwrap();

        let sym = ctx.module[rec]
            .sym
            .map(|sym| Symbol::new_from_args(format_args!("{}_slice", sym)));

        Ok(Item::new(
            output_ty,
            ctx.module.add_and_get_port::<_, Splitter>(SplitterArgs {
                input: rec,
                outputs: iter::once((output_ty.to_bitvec(), sym)),
                start: Some(start),
                rev: false,
            }),
        ))
    }
}

pub struct BitVecUnpack;

impl<'tcx> EvalExpr<'tcx> for BitVecUnpack {
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
