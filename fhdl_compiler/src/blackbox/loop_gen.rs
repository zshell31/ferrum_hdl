use rustc_middle::ty::Ty;
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    blackbox::args,
    compiler::{item::Item, item_ty::ItemTyKind, Compiler, Context, LoopGen},
    error::{Error, SpanError, SpanErrorKind},
};

pub struct IntoIter;

impl<'tcx> EvalExpr<'tcx> for IntoIter {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: Ty<'tcx>,
        _: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        match rec.ty.kind() {
            ItemTyKind::Array(array_ty) => {
                let iter_item_ty = array_ty.ty();
                let group = rec.group();
                let iter = group.to_iter();
                let len = group.len();

                Ok(LoopGen::new(compiler, iter_item_ty, iter, len))
            }
            ItemTyKind::LoopGen => Ok(rec.clone()),
            _ => Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into()),
        }
    }
}

pub struct IterEnum;

impl<'tcx> EvalExpr<'tcx> for IterEnum {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: Ty<'tcx>,
        _: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        if let Some(loop_gen) = rec.loop_gen_opt() {
            Ok(loop_gen.enumerate(compiler))
        } else {
            Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
        }
    }
}

pub struct IterNext;

impl<'tcx> EvalExpr<'tcx> for IterNext {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: Ty<'tcx>,
        _: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        if let Some(loop_gen) = rec.loop_gen_opt() {
            Ok(loop_gen.next(compiler))
        } else {
            Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
        }
    }
}
