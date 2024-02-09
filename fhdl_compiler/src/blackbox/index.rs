use fhdl_netlist::{
    const_val::ConstVal,
    net_list::{ModuleId, NodeOutId},
    node::{Mux, Splitter},
    node_ty::NodeTy,
};
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    compiler::{
        item::Item,
        item_ty::{ItemTy, ItemTyKind},
        Compiler, Context, SymIdent,
    },
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

pub struct Index;

impl<'tcx> EvalExpr<'tcx> for Index {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        utils::args!(args as rec, idx);
        let mod_id = ctx.module_id;

        let item = match rec.ty.kind() {
            ItemTyKind::Node(NodeTy::Unsigned(count)) if *count != 0 => {
                match compiler.to_const(idx) {
                    Some(idx) => {
                        let rec = compiler.to_bitvec(mod_id, rec).node_out_id();
                        Item::new(output_ty, bit(compiler, mod_id, rec, idx))
                    }
                    None => {
                        let rec = compiler.to_bitvec(mod_id, rec).node_out_id();
                        make_mux(
                            compiler,
                            mod_id,
                            idx,
                            *count,
                            output_ty,
                            |compiler, case| bit(compiler, mod_id, rec, case),
                        )
                    }
                }
            }
            ItemTyKind::Array(array_ty) => match compiler.to_const(idx) {
                Some(idx) => rec.by_idx(idx as usize).clone(),
                None => make_mux(
                    compiler,
                    mod_id,
                    idx,
                    array_ty.count(),
                    array_ty.ty(),
                    |compiler, case| {
                        compiler
                            .to_bitvec(mod_id, &rec.by_idx(case as usize))
                            .node_out_id()
                    },
                ),
            },
            _ => {
                return Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into());
            }
        };

        Ok(item)
    }
}

fn bit(
    compiler: &mut Compiler<'_>,
    mod_id: ModuleId,
    value: NodeOutId,
    bit: u128,
) -> NodeOutId {
    compiler.netlist.add_and_get_out(
        mod_id,
        Splitter::new(value, [(NodeTy::Bit, SymIdent::Bit)], Some(bit), false),
    )
}

fn make_mux<'tcx>(
    compiler: &mut Compiler<'tcx>,
    mod_id: ModuleId,
    idx: &Item<'tcx>,
    count: u128,
    output_ty: ItemTy<'tcx>,
    mk_variant: impl Fn(&mut Compiler<'tcx>, u128) -> NodeOutId,
) -> Item<'tcx> {
    let sel = compiler.to_bitvec(mod_id, idx).node_out_id();
    let sel_width = idx.width();

    let mux = Mux::new(
        output_ty.to_bitvec(),
        sel,
        (0 .. count).map(|case| {
            let variant = mk_variant(compiler, case);

            (ConstVal::new(case, sel_width), variant)
        }),
        None,
        SymIdent::Mux,
    );
    let mux = compiler.netlist.add_and_get_out(mod_id, mux);

    compiler.from_bitvec(mod_id, mux, output_ty)
}
