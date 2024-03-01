use std::iter;

use fhdl_netlist::{
    const_val::ConstVal,
    netlist::{IterMut, Module, Port},
    node::{Mux, MuxArgs, Splitter, SplitterArgs},
    node_ty::NodeTy,
};
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Item, ModuleExt},
        item_ty::{ItemTy, ItemTyKind},
        Compiler, Context, SymIdent,
    },
    error::{Error, SpanError, SpanErrorKind},
};

pub struct Index;

impl<'tcx> EvalExpr<'tcx> for Index {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec, idx);

        let item = match rec.ty.kind() {
            ItemTyKind::Node(NodeTy::Unsigned(count)) if *count != 0 => {
                match ctx.module.to_const_val(idx) {
                    Some(idx) => {
                        let rec = ctx.module.to_bitvec(rec).port();
                        Item::new(output_ty, bit(&mut ctx.module, rec, idx))
                    }
                    None => {
                        let rec = ctx.module.to_bitvec(rec).port();
                        make_mux(
                            &mut ctx.module,
                            idx,
                            *count,
                            output_ty,
                            |module, case| bit(module, rec, case),
                        )
                    }
                }
            }
            ItemTyKind::Array(array_ty) => match ctx.module.to_const_val(idx) {
                Some(idx) => rec.by_idx(idx as usize).clone(),
                None => make_mux(
                    &mut ctx.module,
                    idx,
                    array_ty.count(),
                    array_ty.ty(),
                    |module, case| module.to_bitvec(&rec.by_idx(case as usize)).port(),
                ),
            },
            _ => {
                return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
            }
        };

        Ok(item)
    }
}

fn bit(module: &mut Module, value: Port, bit: u128) -> Port {
    module.add_and_get_port::<_, Splitter>(SplitterArgs {
        input: value,
        outputs: iter::once((NodeTy::Bit, SymIdent::Bit.into())),
        start: Some(bit),
        rev: false,
    })
}

fn make_mux<'tcx>(
    module: &mut Module,
    idx: &Item<'tcx>,
    count: u128,
    output_ty: ItemTy<'tcx>,
    mk_variant: impl Fn(&mut Module, u128) -> Port,
) -> Item<'tcx> {
    let sel = module.to_bitvec(idx).port();
    let sel_width = idx.width();

    let mux = MuxArgs {
        ty: output_ty.to_bitvec(),
        sel,
        variants: IterMut::new(0 .. count, |module, case| {
            let variant = mk_variant(module, case);

            (ConstVal::new(case, sel_width), variant)
        }),
        default: None,
        sym: SymIdent::Mux.into(),
    };
    let mux = module.add_and_get_port::<_, Mux>(mux);

    module.from_bitvec(mux, output_ty)
}
