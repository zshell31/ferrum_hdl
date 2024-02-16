use rustc_middle::ty::List;
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    compiler::{
        item::{Group, Item},
        item_ty::ItemTy,
        Compiler, Context,
    },
    error::Error,
    utils,
};

pub struct Chain;

impl<'tcx> EvalExpr<'tcx> for Chain {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as init, closure);

        let array_ty = output_ty.struct_ty().by_idx(1);
        let count = array_ty.array_ty().count();

        let idx_ty = compiler.closure_inputs(&closure.ty)[0];
        let idx_ty = compiler.resolve_ty(idx_ty, List::empty(), span)?;

        let mut prev = init.clone();
        let array = Item::new(
            array_ty,
            Group::try_new((0 .. count).map(|idx| {
                let idx =
                    compiler
                        .netlist
                        .const_val(ctx.module_id, idx_ty.to_bitvec(), idx);
                let idx = compiler.from_bitvec(ctx.module_id, idx, idx_ty);

                let inputs = &[idx, prev.clone()];

                let outputs = compiler.instantiate_closure(closure, inputs, ctx, span)?;
                prev = outputs.by_idx(0).clone();

                let item = outputs.by_idx(1);
                Ok(item.clone())
            }))?,
        );

        Ok(Item::new(output_ty, Group::new([prev, array])))
    }
}
