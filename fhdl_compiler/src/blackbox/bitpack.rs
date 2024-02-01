use fhdl_netlist::node::Splitter;
use rustc_span::Span;

use super::{bitvec, EvalExpr};
use crate::{
    error::Error,
    eval_context::EvalContext,
    generator::{item::Item, item_ty::ItemTy, Generator},
    scopes::SymIdent,
    utils,
};

pub struct BitPackPack;

impl<'tcx> EvalExpr<'tcx> for BitPackPack {
    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);

        Ok(generator.to_bitvec(ctx.module_id, rec, &ctx.locals))
    }
}

pub struct BitPackRepack;

impl<'tcx> EvalExpr<'tcx> for BitPackRepack {
    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);

        let rec = generator.to_bitvec(ctx.module_id, rec, &ctx.locals);
        Ok(generator.from_bitvec(ctx.module_id, rec, output_ty))
    }
}

pub struct BitPackMsb;

impl<'tcx> EvalExpr<'tcx> for BitPackMsb {
    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec);

        bitvec::bit_vec_trans(generator, rec, ctx, |generator, ctx, bit_vec| {
            Ok((
                generator.netlist.add_and_get_out(
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
