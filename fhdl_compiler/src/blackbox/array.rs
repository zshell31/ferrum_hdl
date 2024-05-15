use rustc_middle::ty::{List, Ty};
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Group, Item, ModuleExt},
        Compiler, Context,
    },
    error::Error,
};

pub struct Make {
    pub with_idx: bool,
}

impl<'tcx> EvalExpr<'tcx> for Make {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: Ty<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as closure);

        let array_ty = compiler.resolve_fn_out_ty(output_ty, span)?;
        let count = array_ty.array_ty().count();

        let idx_ty = if self.with_idx {
            let idx_ty = compiler.closure_inputs(&closure.ty)[0];
            Some(compiler.resolve_ty(idx_ty, List::empty(), span)?)
        } else {
            None
        };

        Ok(Item::new(
            array_ty,
            Group::try_new((0 .. count).map(|idx| match idx_ty {
                Some(idx_ty) => {
                    let idx = ctx.module.const_val(idx_ty.to_bitvec(), idx);
                    let idx = ctx.module.from_bitvec(idx, idx_ty, span)?;

                    compiler.instantiate_closure(closure, &[idx], ctx, span)
                }
                None => compiler.instantiate_closure(closure, &[], ctx, span),
            }))?,
        ))
    }
}

pub struct Map {
    pub with_idx: bool,
}

impl<'tcx> EvalExpr<'tcx> for Map {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: Ty<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec, closure);

        let array_ty = compiler.resolve_fn_out_ty(output_ty, span)?;

        let idx_ty = if self.with_idx {
            let idx_ty = compiler.closure_inputs(&closure.ty)[0];
            Some(compiler.resolve_ty(idx_ty, List::empty(), span)?)
        } else {
            None
        };

        Ok(Item::new(
            array_ty,
            Group::try_new(rec.group().to_iter().enumerate().map(|(idx, item)| {
                match idx_ty {
                    Some(idx_ty) => {
                        let idx = ctx.module.const_val(idx_ty.to_bitvec(), idx as u128);
                        let idx = ctx.module.from_bitvec(idx, idx_ty, span)?;

                        compiler.instantiate_closure(closure, &[idx, item], ctx, span)
                    }
                    None => compiler.instantiate_closure(closure, &[item], ctx, span),
                }
            }))?,
        ))
    }
}
