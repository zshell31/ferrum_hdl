use rustc_middle::ty::Ty;
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ModuleExt},
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
        _: Ty<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        ctx.module.to_bitvec(rec, span)
    }
}

pub struct Unpack;

impl<'tcx> EvalExpr<'tcx> for Unpack {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: Ty<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        let output_ty = compiler.resolve_fn_out_ty(output_ty, span)?;
        ctx.module.from_bitvec(rec, output_ty, span)
    }
}
