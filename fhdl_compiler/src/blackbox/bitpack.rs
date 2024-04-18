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

pub struct Pack;

impl<'tcx> EvalExpr<'tcx> for Pack {
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

pub struct Unpack;

impl<'tcx> EvalExpr<'tcx> for Unpack {
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
