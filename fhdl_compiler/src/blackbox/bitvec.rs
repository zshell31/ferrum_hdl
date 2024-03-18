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

pub struct Slice {
    pub only_one: bool,
}

impl<'tcx> EvalExpr<'tcx> for Slice {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec, idx);

        let item = match rec.ty.kind() {
            ItemTyKind::Node(NodeTy::Unsigned(count)) if *count != 0 => {
                let (node_ty, count) = if self.only_one {
                    (NodeTy::Bit, *count)
                } else {
                    let slice_len = ctx.fn_generic_const(compiler, 0, span)?.unwrap();
                    let count = *count + 1 - slice_len;
                    (NodeTy::Unsigned(slice_len), count)
                };

                let rec = ctx.module.to_bitvec(rec).port();
                make_mux(&mut ctx.module, idx, count, output_ty, |module, case| {
                    slice(module, rec, case, node_ty)
                })
            }
            ItemTyKind::Array(array_ty) => {
                let group = rec.group();

                let (count, slice_len) = if self.only_one {
                    (array_ty.count(), 1)
                } else {
                    let slice_len = ctx.fn_generic_const(compiler, 0, span)?.unwrap();
                    assert_eq!(output_ty.array_ty().count(), slice_len);

                    (array_ty.count() + 1 - slice_len, slice_len)
                };

                make_mux(&mut ctx.module, idx, count, output_ty, |module, case| {
                    module
                        .to_bitvec(
                            &(if self.only_one {
                                group.by_idx(case as usize)
                            } else {
                                Item::new(
                                    output_ty,
                                    group.slice(case as usize, slice_len as usize),
                                )
                            }),
                        )
                        .port()
                })
            }
            _ => {
                return Err(Error::from(SpanError::new(
                    SpanErrorKind::NotSynthExpr,
                    span,
                )));
            }
        };

        Ok(item)
    }
}

fn slice(module: &mut Module, value: Port, idx: u128, node_ty: NodeTy) -> Port {
    module.add_and_get_port::<_, Splitter>(SplitterArgs {
        input: value,
        outputs: iter::once((node_ty, SymIdent::Bit.into())),
        start: Some(idx),
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
