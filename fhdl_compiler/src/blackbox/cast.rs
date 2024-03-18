use ferrum_hdl::{cast::CastFrom, unsigned::Unsigned};
use rustc_span::Span;
use tracing::error;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ItemKind},
        item_ty::{ItemTy, ItemTyKind},
        Compiler, Context, SymIdent,
    },
    error::{Error, SpanError, SpanErrorKind},
};

#[allow(dead_code)]
pub struct Conversion;

impl Conversion {
    pub fn convert<'tcx>(
        from: &Item<'tcx>,
        to_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        if from.ty == to_ty {
            return Ok(from.clone());
        }

        match (from.ty.kind(), to_ty.kind()) {
            (ItemTyKind::Node(from_ty_), ItemTyKind::Node(to_ty_))
                if from_ty_.is_unsigned() && to_ty_.is_unsigned() =>
            {
                assert_convert::<Unsigned<1>, Unsigned<1>>();
                assert_convert::<Unsigned<1>, Unsigned<2>>();
                Ok(Self::to_unsigned(from.clone(), to_ty, ctx))
            }
            _ => {
                error!("from {:?} => to {:?}", from.ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    fn to_unsigned<'tcx>(
        from: Item<'tcx>,
        to_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Item<'tcx> {
        Item::new(
            to_ty,
            ItemKind::Port(Compiler::trunc_or_extend(
                &mut ctx.module,
                from.port(),
                from.ty.node_ty(),
                to_ty.node_ty(),
                SymIdent::Cast.into(),
            )),
        )
    }
}

fn assert_convert<F, T: CastFrom<F>>() {}

impl<'tcx> EvalExpr<'tcx> for Conversion {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as from);

        Self::convert(from, output_ty, ctx, span)
    }
}
