use std::iter;

use ferrum_hdl::{
    cast::CastFrom,
    unsigned::{u, Unsigned},
};
use fhdl_netlist::{net_list::ModuleId, node_ty::NodeTy};
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::{
        item::{Group, Item, ItemKind},
        item_ty::{ItemTy, ItemTyKind},
        Generator,
    },
    utils,
};

#[allow(dead_code)]
pub struct Conversion;

impl Conversion {
    pub fn convert<'tcx>(
        generator: &mut Generator<'tcx>,
        module_id: ModuleId,
        from: &Item<'tcx>,
        to_ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        if from.ty == to_ty {
            return Ok(from.clone());
        }

        match (from.ty.kind(), to_ty.kind()) {
            (ItemTyKind::Node(NodeTy::Unsigned(_)), ItemTyKind::Struct(struct_ty))
                if struct_ty.is_unsigned_short() && from.ty.width() == to_ty.width() =>
            {
                assert_convert::<Unsigned<1>, u<1>>();
                Ok(Item::new(
                    to_ty,
                    ItemKind::Group(Group::new(iter::once(from.clone()))),
                ))
            }
            (ItemTyKind::Struct(struct_ty), ItemTyKind::Node(NodeTy::Unsigned(_)))
                if struct_ty.is_unsigned_short() && from.ty.width() == to_ty.width() =>
            {
                assert_convert::<u<1>, Unsigned<1>>();

                let from = from.by_idx(0).clone();
                Ok(Self::to_unsigned(module_id, from, to_ty, generator))
            }
            (ItemTyKind::Node(from_ty_), ItemTyKind::Node(to_ty_))
                if from_ty_.is_unsigned() && to_ty_.is_unsigned() =>
            {
                Ok(Self::to_unsigned(module_id, from.clone(), to_ty, generator))
            }
            _ => {
                println!("from {:?} => to {:?}", from.ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    fn to_unsigned<'tcx>(
        module_id: ModuleId,
        from: Item<'tcx>,
        to_ty: ItemTy<'tcx>,
        generator: &mut Generator<'tcx>,
    ) -> Item<'tcx> {
        Item::new(
            to_ty,
            ItemKind::Node(generator.trunc_or_extend(
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
        generator: &mut Generator<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as from);

        Self::convert(generator, ctx.module_id, from, output_ty, span)
    }
}
