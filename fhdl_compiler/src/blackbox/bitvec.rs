use fhdl_netlist::{node::Splitter, symbol::Symbol};
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context},
    error::Error,
    utils,
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
