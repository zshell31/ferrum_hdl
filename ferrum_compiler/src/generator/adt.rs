use ferrum_netlist::{
    arena::with_arena,
    group_list::{Group, ItemId, Named},
    sig_ty::{ArrayTy, SignalTy, StructTy},
    symbol::Symbol,
};
use rustc_middle::ty::{GenericArgsRef, Ty, TyKind};
use rustc_span::Span;
use smallvec::SmallVec;

use crate::{
    blackbox,
    error::{Error, SpanError, SpanErrorKind},
    generator::Generator,
};

impl<'tcx> Generator<'tcx> {
    pub fn evaluate_adt_ty(
        &mut self,
        ty: &Ty<'tcx>,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<SignalTy>, Error> {
        let tcx = self.tcx;
        match ty.kind() {
            TyKind::Adt(adt, adt_generics) if adt.is_struct() => {
                let fields = adt
                    .all_fields()
                    .map(|field| {
                        (
                            Some(Symbol::new(field.ident(tcx).as_str())),
                            field.ty(tcx, adt_generics),
                        )
                    })
                    .filter(|(_, ty)| match ty.kind() {
                        TyKind::Adt(adt, _) => {
                            let def_path = self.tcx.def_path(adt.did());
                            !blackbox::ignore_ty(&def_path)
                        }
                        _ => true,
                    })
                    .collect::<SmallVec<[_; 8]>>();

                self.make_struct_ty(fields, |generator, ty| {
                    generator.find_sig_ty(ty, generics, span)
                })
                .map(Some)
            }
            TyKind::Adt(adt, _) if adt.is_enum() => {
                println!("{:#?}", adt.variants());

                Err(SpanError::new(SpanErrorKind::ExpectedStructType, span).into())
            }
            _ => {
                println!("{:#?}", ty.kind());
                Err(SpanError::new(SpanErrorKind::ExpectedStructType, span).into())
            }
        }
    }

    pub fn make_array_group<T>(
        &mut self,
        ty: ArrayTy,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(
                iter.into_iter()
                    .map(|item| f(self, item).map(|item_id| Named::new(item_id, None))),
            )?
        };

        Ok(Group::new_with_item_ids(SignalTy::Array(ty), group).into())
    }

    pub fn make_tuple_group<T>(
        &mut self,
        ty: StructTy,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().enumerate().map(
                |(ind, item)| {
                    f(self, item).map(|item_id| {
                        Named::new(item_id, Some(Symbol::new_from_ind(ind)))
                    })
                },
            ))?
        };

        Ok(Group::new_with_item_ids(SignalTy::Struct(ty), group).into())
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
                        .map(|sig_ty| Named::new(sig_ty, Some(Symbol::new_from_ind(ind))))
                },
            ))?
        };

        Ok(StructTy::new(ty))
    }

    pub fn make_struct_group<T>(
        &mut self,
        ty: StructTy,
        iter: impl IntoIterator<Item = (Option<Symbol>, T)>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().map(|(sym, item)| {
                f(self, item).map(|item_id| Named::new(item_id, sym))
            }))?
        };

        Ok(Group::new_with_item_ids(SignalTy::Struct(ty), group).into())
    }

    pub fn make_struct_ty<T>(
        &mut self,
        iter: impl IntoIterator<Item = (Option<Symbol>, T)>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<SignalTy, Error>,
    ) -> Result<SignalTy, Error> {
        let ty =
            unsafe {
                with_arena().alloc_from_res_iter(iter.into_iter().map(
                    |(sym, item)| f(self, item).map(|sig_ty| Named::new(sig_ty, sym)),
                ))?
            };

        Ok(SignalTy::Struct(StructTy::new(ty)))
    }
}
