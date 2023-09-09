use ferrum_netlist::sig_ty::SignalTy;
use rustc_middle::ty::{GenericArg, List, Ty, TyKind};
use rustc_span::Span;

use crate::{
    blackbox,
    error::{Error, SpanError, SpanErrorKind},
    generator::Generator,
};

impl<'tcx> Generator<'tcx> {
    pub fn evaluate_struct_ty(
        &mut self,
        ty: &Ty<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Option<SignalTy>, Error> {
        let tcx = self.tcx;
        match ty.kind() {
            TyKind::Adt(adt, adt_generics) if adt.is_struct() => {
                let fields = adt
                    .all_fields()
                    .map(|field| (field.ident(tcx), field.ty(tcx, adt_generics)))
                    .filter(|(_, ty)| match ty.kind() {
                        TyKind::Adt(adt, _) => {
                            let def_path = self.tcx.def_path(adt.did());
                            !blackbox::ignore_ty(&def_path)
                        }
                        _ => true,
                    })
                    .collect::<Vec<_>>();

                self.make_struct_sig_ty(fields, |generator, ty| {
                    generator.find_sig_ty(ty, generics, span)
                })
                .map(Some)
            }
            _ => Err(SpanError::new(SpanErrorKind::ExpectedStructType, span).into()),
        }
    }
}
