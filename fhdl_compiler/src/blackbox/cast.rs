use ferrum_hdl::{cast::CastFrom, unsigned::Unsigned};
use fhdl_netlist::net_list::ModuleId;
use rustc_span::Span;
use tracing::error;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ItemKind},
        item_ty::{ItemTy, ItemTyKind},
        Compiler, Context,
    },
    error::{Error, SpanError, SpanErrorKind},
};

#[allow(dead_code)]
pub struct Conversion;

impl Conversion {
    pub fn convert<'tcx>(
        compiler: &mut Compiler<'tcx>,
        module_id: ModuleId,
        from: &Item<'tcx>,
        to_ty: ItemTy<'tcx>,
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
                Ok(Self::to_unsigned(module_id, from.clone(), to_ty, compiler))
            }
            _ => {
                error!("from {:?} => to {:?}", from.ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    fn to_unsigned<'tcx>(
        module_id: ModuleId,
        from: Item<'tcx>,
        to_ty: ItemTy<'tcx>,
        compiler: &mut Compiler<'tcx>,
    ) -> Item<'tcx> {
        Item::new(
            to_ty,
            ItemKind::Node(compiler.trunc_or_extend(
                module_id,
                from.node_out_id(),
                from.ty.node_ty(),
                to_ty.node_ty(),
            )),
        )
    }
}

fn assert_convert<F, T: CastFrom<F>>() {}

impl<'tcx> EvalExpr<'tcx> for Conversion {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as from);

        Self::convert(compiler, ctx.module_id, from, output_ty, span)
    }
}
