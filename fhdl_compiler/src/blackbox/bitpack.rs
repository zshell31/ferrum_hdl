use fhdl_netlist::node::Splitter;
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context, SymIdent},
    error::Error,
};

pub struct BitPackPack;

impl<'tcx> EvalExpr<'tcx> for BitPackPack {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        Ok(compiler.to_bitvec(ctx.module_id, rec))
    }
}

pub struct BitPackUnpack;

impl<'tcx> EvalExpr<'tcx> for BitPackUnpack {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        Ok(compiler.from_bitvec(ctx.module_id, rec, output_ty))
    }
}

pub struct BitPackMsb;

impl<'tcx> EvalExpr<'tcx> for BitPackMsb {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        let rec = compiler.to_bitvec(ctx.module_id, rec);
        let msb = compiler.netlist.add_and_get_out(
            ctx.module_id,
            Splitter::new(
                rec.node_out_id(),
                [(output_ty.node_ty(), SymIdent::Msb)],
                None,
                true,
            ),
        );

        Ok(compiler.from_bitvec(ctx.module_id, msb, output_ty))
    }
}
