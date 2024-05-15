use fhdl_netlist::node::{DFFArgs, TyOrData, DFF};
use rustc_middle::ty::Ty;
use rustc_span::Span;

use super::{args, EvalExpr};
use crate::{
    compiler::{
        item::{Group, Item, ItemKind, ModuleExt},
        Compiler, Context, SymIdent,
    },
    error::Error,
};

pub struct RegEn {
    pub comb: bool,
}

impl<'tcx> EvalExpr<'tcx> for RegEn {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: Ty<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as state, init, en, comb);

        let output_ty = compiler.resolve_fn_out_ty(output_ty, span)?;

        let dom_id = state.ty.reg_ty().dom_id;
        let domain = compiler.find_domain_by_id(dom_id);

        let clk = ctx.module.clk();
        let rst = ctx.module.rst();
        let en = en.port();
        let init = ctx.module.to_bitvec(init, span)?.port();

        let (dff_ty, comb_ty) = if self.comb {
            let struct_ty = output_ty.struct_ty();
            (struct_ty.by_idx(0), struct_ty.by_idx(1))
        } else {
            (output_ty, output_ty)
        };

        let dff = ctx.module.add_and_get_port::<_, DFF>(DFFArgs {
            clk,
            rst: Some(rst),
            rst_kind: domain.rst_kind,
            rst_pol: domain.rst_pol,
            en: Some(en),
            init,
            data: TyOrData::Ty(dff_ty.to_bitvec()),
            sym: SymIdent::Reg.into(),
        });
        let dff_out = ctx.module.from_bitvec(dff, dff_ty, span)?;
        let comb = compiler.instantiate_closure(comb, &[dff_out.clone()], ctx, span)?;

        assert_eq!(comb.ty, comb_ty);
        ctx.module.assign_names_to_item("comb", &comb, false);

        let comb_out = ctx.module.to_bitvec(&comb, span)?.port();
        DFF::set_data(&mut ctx.module, dff.node, comb_out);

        Ok(if self.comb {
            Item::new(output_ty, ItemKind::Group(Group::new([dff_out, comb])))
        } else {
            dff_out
        })
    }
}
