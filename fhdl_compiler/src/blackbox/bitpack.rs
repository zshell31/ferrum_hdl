use fhdl_netlist::node::Splitter;
use rustc_span::Span;

use super::{bitvec, EvalExpr};
use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context, SymIdent},
    error::Error,
    utils,
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
        utils::args!(args as rec);

        Ok(compiler.to_bitvec(ctx.module_id, rec))
    }
}

pub struct BitPackRepack;

impl<'tcx> EvalExpr<'tcx> for BitPackRepack {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);

        let rec = compiler.to_bitvec(ctx.module_id, rec);
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
        utils::args!(args as rec);

        bitvec::bit_vec_trans(compiler, rec, ctx, |compiler, ctx, bit_vec| {
            Ok((
                compiler.netlist.add_and_get_out(
                    ctx.module_id,
                    Splitter::new(
                        bit_vec,
                        [(output_ty.node_ty(), SymIdent::Msb)],
                        None,
                        true,
                    ),
                ),
                output_ty,
            ))
        })
    }
}
