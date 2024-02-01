use super::EvalExpr;

pub struct SignalReg {
    pub has_en: bool,
}

impl<'tcx> EvalExpr<'tcx> for SignalReg {
    // fn eval(
    //     &self,
    //     generator: &mut Generator<'tcx>,
    //     args: &[Item<'tcx>],
    //     output_ty: ItemTy<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    //     span: Span,
    // ) -> Result<Item<'tcx>, Error> {
    //     let mod_id = ctx.module_id;

    //     let (clk, rst, en, rst_val, comb) = match self.has_en {
    //         true => (&args[0], &args[1], Some(&args[2]), &args[3], &args[4]),
    //         false => (&args[0], &args[1], None, &args[2], &args[3]),
    //     };
    //     let clk = clk.node_out_id();
    //     let rst = generator.to_bitvec(mod_id, rst, &ctx.locals);
    //     let en = en.map(|en| generator.to_bitvec(mod_id, en, &ctx.locals));
    //     let rst_val = generator.to_bitvec(mod_id, rst_val, &ctx.locals);

    //     let comb = comb.module_id();
    //     generator.set_mod_name(comb, "SignalReg");

    //     let dff = generator.netlist.add(
    //         ctx.module_id,
    //         DFF::new(
    //             output_ty.to_bitvec(),
    //             clk,
    //             rst,
    //             en,
    //             rst_val,
    //             None,
    //             if en.is_none() {
    //                 SymIdent::Dff
    //             } else {
    //                 SymIdent::DffEn
    //             },
    //         ),
    //     );

    //     let dff_out = generator.netlist[dff].only_one_out().node_out_id();
    //     let dff_out = generator.from_bitvec(ctx.module_id, dff_out, output_ty);

    //     let comb = generator.instantiate_closure_(comb, &[dff_out], ctx, span)?;
    //     assert_eq!(generator.item_ty_old(comb), output_ty);

    //     let comb_out = generator.to_bitvec(ctx.module_id, comb, &ctx.locals);
    //     generator.netlist.set_dff_data(dff, comb_out);

    //     Ok(dff_out)
    // }
}

pub struct SignalMap;

impl<'tcx> EvalExpr<'tcx> for SignalMap {
    // fn eval(
    //     &self,
    //     generator: &mut Generator<'tcx>,
    //     args: &[Item<'tcx>],
    //     _: ItemTy<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    //     span: Span,
    // ) -> Result<Item<'tcx>, Error> {
    //     utils::args!(args as rec, comb);

    //     let rec = rec.item_id();
    //     let comb = comb.mod_id();
    //     generator.set_mod_name(comb, "SignalMap");

    //     generator.instantiate_closure_(comb, &[rec], ctx, span)
    // }
}

pub struct SignalAndThen;

impl<'tcx> EvalExpr<'tcx> for SignalAndThen {
    // fn eval(
    //     &self,
    //     generator: &mut Generator<'tcx>,
    //     args: &[Item<'tcx>],
    //     _: ItemTy<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    //     span: Span,
    // ) -> Result<Item<'tcx>, Error> {
    //     utils::args!(args as rec, comb);

    //     let rec = rec.item_id();
    //     let comb = comb.mod_id();
    //     generator.set_mod_name(comb, "SignalAndThen");

    //     generator.instantiate_closure_(comb, &[rec], ctx, span)
    // }
}

pub struct SignalApply2;

impl<'tcx> EvalExpr<'tcx> for SignalApply2 {
    // fn eval(
    //     &self,
    //     generator: &mut Generator<'tcx>,
    //     args: &[Item<'tcx>],
    //     _: ItemTy<'tcx>,
    //     ctx: &mut EvalContext<'tcx>,
    //     span: Span,
    // ) -> Result<Item<'tcx>, Error> {
    //     utils::args!(args as arg1, arg2, comb);

    //     let arg1 = arg1.item_id();
    //     let arg2 = arg2.item_id();
    //     let comb = comb.mod_id();
    //     generator.set_mod_name(comb, "SignalApply2");

    //     generator.instantiate_closure_(comb, &[arg1, arg2], ctx, span)
    // }
}
