use fhdl_netlist::{
    arena::with_arena,
    group::{Group, ItemId},
    sig_ty::{ArrayTy, EnumTy, Named, SignalTy, SignalTyKind, StructTy},
    symbol::Symbol,
};
use rustc_middle::ty::{FieldDef, GenericArgsRef, Ty, TyKind};
use rustc_span::Span;

use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::Generator,
};

impl<'tcx> Generator<'tcx> {
    fn make_struct_ty_from_fields(
        &mut self,
        fields: impl IntoIterator<Item = &'tcx FieldDef>,
        adt_generics: GenericArgsRef<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let tcx = self.tcx;
        let fields = fields
            .into_iter()
            .map(|field| {
                (
                    Symbol::new(field.ident(tcx).as_str()),
                    field.ty(tcx, adt_generics),
                )
            })
            .filter_map(|(sym, ty)| {
                let check_ty = match ty.kind() {
                    TyKind::Adt(adt, _) => !self.ignore_ty(adt.did()),
                    _ => true,
                };
                if check_ty {
                    Some(
                        self.find_sig_ty(ty, ctx, span)
                            .map(|sig_ty| Named::new(sig_ty, sym)),
                    )
                } else {
                    None
                }
            });

        let tys = unsafe { with_arena().alloc_from_res_iter(fields)? };

        Ok(SignalTy::new(SignalTyKind::Struct(StructTy::new(tys))))
    }

    pub fn eval_adt_ty(
        &mut self,
        ty: &Ty<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        match ty.kind() {
            TyKind::Adt(adt, adt_generics) if adt.is_struct() => {
                self.make_struct_ty_from_fields(adt.all_fields(), adt_generics, ctx, span)
            }
            TyKind::Adt(adt, adt_generics) if adt.is_enum() => {
                let variants = unsafe {
                    with_arena().alloc_from_res_iter(adt.variants().iter().map(
                        |variant| {
                            self.make_struct_ty_from_fields(
                                variant.fields.iter(),
                                adt_generics,
                                ctx,
                                span,
                            )
                            .map(|ty| Named::new(ty, Symbol::new(variant.name.as_str())))
                        },
                    ))?
                };

                Ok(SignalTy::new(SignalTyKind::Enum(EnumTy::new(variants))))
            }
            _ => {
                println!("{:#?}", ty.kind());
                Err(SpanError::new(SpanErrorKind::ExpectedStructType, span).into())
            }
        }
    }

    pub fn make_tuple_ty<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<SignalTy, Error>,
    ) -> Result<StructTy, Error> {
        let ty = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().enumerate().map(
                |(ind, item)| {
                    f(self, item)
                        .map(|sig_ty| Named::new(sig_ty, Symbol::new_from_ind(ind)))
                },
            ))?
        };

        Ok(StructTy::new(ty))
    }

    pub fn make_array_group<T>(
        &mut self,
        ty: ArrayTy,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena()
                .alloc_from_res_iter(iter.into_iter().map(|item| f(self, item)))?
        };
        assert_eq!(group.len(), ty.count() as usize);

        Ok(
            Group::new_with_item_ids(SignalTy::new(SignalTyKind::Array(ty)), group)
                .into(),
        )
    }

    pub fn make_struct_group<T>(
        &mut self,
        ty: StructTy,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena()
                .alloc_from_res_iter(iter.into_iter().map(|item| f(self, item)))?
        };

        assert_eq!(group.len(), ty.len());
        for (item_id, ty) in group.iter().zip(ty.tys()) {
            if let Some(node_out_id) = item_id.node_out_id_opt() {
                if self.netlist[node_out_id].sym.is_none()
                    && ty
                        .name
                        .as_str()
                        .chars()
                        .next()
                        .map(|ch| !ch.is_ascii_digit())
                        .unwrap_or_default()
                {
                    self.netlist[node_out_id].sym = Some(ty.name);
                }
            }
        }

        Ok(
            Group::new_with_item_ids(SignalTy::new(SignalTyKind::Struct(ty)), group)
                .into(),
        )
    }
}
