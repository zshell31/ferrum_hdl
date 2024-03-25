use ferrum_hdl::{cast, signed::S, unsigned::U};
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
pub struct CastFrom;

impl CastFrom {
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
                assert_convert::<U<1>, U<1>>();
                assert_convert::<U<1>, U<2>>();
                Ok(Self::trunc_or_extend(from.clone(), to_ty, ctx, false))
            }
            (ItemTyKind::Node(from_ty_), ItemTyKind::Node(to_ty_))
                if from_ty_.is_signed() && to_ty_.is_signed() =>
            {
                assert_convert::<S<1>, S<1>>();
                assert_convert::<S<1>, S<2>>();
                Ok(Self::trunc_or_extend(from.clone(), to_ty, ctx, true))
            }
            _ => {
                error!("from {:?} => to {:?}", from.ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    fn trunc_or_extend<'tcx>(
        from: Item<'tcx>,
        to_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        is_sign: bool,
    ) -> Item<'tcx> {
        Item::new(
            to_ty,
            ItemKind::Port(Compiler::trunc_or_extend(
                &mut ctx.module,
                from.port(),
                from.ty.node_ty(),
                to_ty.node_ty(),
                SymIdent::Cast.into(),
                is_sign,
            )),
        )
    }
}

fn assert_convert<F, T: cast::CastFrom<F>>() {}

impl<'tcx> EvalExpr<'tcx> for CastFrom {
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
