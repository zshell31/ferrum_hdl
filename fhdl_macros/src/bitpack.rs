use std::{borrow::Cow, iter};

use darling::{
    ast::{Data, Fields, Style},
    FromAttributes, FromDeriveInput, FromField, FromGenerics, FromVariant,
};
use either::Either;
use fhdl_const_func::clog2_len;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{GenericParam, Generics, Ident, Index, Type};

use crate::utils::{self, Bounds, TEither};

pub struct BitPackDerive {
    ident: Ident,
    generics: Generics,
    data: Data<BitPackVariant, BitPackField>,
    attrs: Bounds,
}

impl FromDeriveInput for BitPackDerive {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        let attrs = input
            .attrs
            .iter()
            .filter_map(|attr| {
                let ident = attr.path();
                if ident.is_ident("bitpack") {
                    Some(attr.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        Ok(Self {
            ident: input.ident.clone(),
            generics: Generics::from_generics(&input.generics)?,
            data: Data::try_from(&input.data)?,
            attrs: Bounds::from_attributes(&attrs)?,
        })
    }
}

#[derive(FromField)]
pub struct BitPackField {
    ident: Option<Ident>,
    ty: Type,
}

impl BitPackField {
    fn field_name(&self, idx: usize) -> Cow<'_, Ident> {
        self.ident.as_ref().map(Cow::Borrowed).unwrap_or_else(|| {
            Cow::Owned(Ident::new(&format!("_var_{idx}"), Span::call_site()))
        })
    }

    fn field(&self, idx: usize) -> TokenStream {
        self.ident
            .as_ref()
            .map(|ident| quote! { #ident })
            .unwrap_or_else(|| {
                let idx = Index::from(idx);
                quote! { #idx }
            })
    }
}

#[derive(FromVariant)]
pub struct BitPackVariant {
    ident: Ident,
    fields: Fields<BitPackField>,
}

impl BitPackDerive {
    pub fn into_tokens(self) -> TokenStream {
        let impl_bit_size = self.impl_bit_size();
        let impl_bit_pack = self.impl_bit_pack();

        quote! {
            #impl_bit_size

            #impl_bit_pack
        }
    }

    pub fn impl_bit_size(&self) -> TokenStream {
        let ident = &self.ident;
        let bit_size = match &self.data {
            Data::Enum(variants) => {
                if variants.is_empty() {
                    quote! { 0 }
                } else {
                    let discr_width = clog2_len(variants.len());

                    let bits = variants
                        .iter()
                        .map(|variant| {
                            if variant.fields.is_empty() {
                                quote! { 0 }
                            } else {
                                let bits = variant.fields.iter().map(|field| {
                                    let ty = &field.ty;

                                    quote! {
                                        <#ty as ::ferrum_hdl::bitpack::BitSize>::BITS
                                    }
                                });

                                quote! {
                                    ( #(#bits)+* )
                                }
                            }
                        })
                        .collect::<Vec<_>>();

                    let data_width = match bits.len() {
                        0 => quote! { 0 },
                        1 => bits[0].clone(),
                        2 => {
                            let n = &bits[0];
                            let m = &bits[1];

                            quote! {
                                ::ferrum_hdl::const_functions::max(#n, #m)
                            }
                        }
                        _ => {
                            let n = &bits[0];
                            let m = &bits[1];

                            let mut max =
                                quote! { ::ferrum_hdl::const_functions::max(#n, #m) };

                            (2 .. bits.len()).for_each(|i| {
                            let m = &bits[i];
                            max = quote! { ::ferrum_hdl::const_functions::max(#max, #m) };
                        });

                            max
                        }
                    };

                    quote! {
                        #discr_width + #data_width
                    }
                }
            }
            Data::Struct(fields) => {
                if fields.is_empty() {
                    quote! { 0 }
                } else {
                    let bits = fields.iter().map(|field| {
                        let ty = &field.ty;

                        quote! {
                            <#ty as ::ferrum_hdl::bitpack::BitSize>::BITS
                        }
                    });

                    quote! { #(#bits)+* }
                }
            }
        };

        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let predicates = predicates.chain(
            self.generics
                .type_params()
                .filter(|type_param| !self.attrs.contains_ty_param(type_param))
                .map(|type_param| {
                    let ident = &type_param.ident;

                    TEither::TS(quote! {
                        #ident: ::ferrum_hdl::bitpack::BitSize
                    })
                }),
        );

        let predicates = predicates.chain(
            self.attrs
                .0
                .iter()
                .filter(|bound| !Bounds::is_array_constr(bound))
                .map(TEither::AsIs),
        );

        let where_clause = utils::into_where_clause(predicates);

        quote! {
            #[automatically_derived]
            impl #impl_generics ::ferrum_hdl::bitpack::BitSize for #ident #ty_generics
            #where_clause
            {
                const BITS: usize = #bit_size;

            }
        }
    }

    pub fn impl_bit_pack(&self) -> TokenStream {
        let ident = &self.ident;

        fn make_names<'f>(
            fields: impl IntoIterator<Item = &'f BitPackField> + 'f,
        ) -> impl Iterator<Item = Cow<'f, Ident>> + 'f {
            fields
                .into_iter()
                .enumerate()
                .map(|(idx, field)| field.field_name(idx))
        }

        let pack = match &self.data {
            Data::Enum(variants) => {
                let discr_width = clog2_len(variants.len());

                let branches = variants.iter().enumerate().map(|(idx, variant)| {
                    let variant_ident = &variant.ident;

                    let discr_expr = quote! {
                        bitvec = bitvec << #discr_width;
                        bitvec = bitvec | Self::Packed::from(#idx);
                    };

                    let offset_expr = quote! {
                        let mut offset = <Self as BitSize>::BITS - #discr_width;
                    };

                    let names = make_names(variant.fields.iter());
                    let exprs = variant.fields.iter().enumerate().map(|(idx, field)| {
                        let ty = &field.ty;
                        let name = field.field_name(idx);

                        quote! {
                            offset = offset.saturating_sub(<#ty as BitSize>::BITS);
                            bitvec = bitvec << <#ty as BitSize>::BITS;
                            bitvec = bitvec | #name.pack().cast::< Self::Packed >();
                        }
                    });

                    let final_expr = quote! {
                        bitvec = bitvec << offset;
                    };

                    match variant.fields.style {
                        Style::Unit => {
                            quote! {
                                Self::#variant_ident() => {
                                    #offset_expr
                                    #discr_expr;
                                    #final_expr;
                                    bitvec

                                }
                            }
                        }
                        Style::Tuple => {
                            quote! {
                                Self::#variant_ident( #(#names),* ) => {
                                    #offset_expr
                                    #discr_expr;
                                    #(#exprs)*;
                                    #final_expr;
                                    bitvec
                                }
                            }
                        }
                        Style::Struct => {
                            quote! {
                                Self::#variant_ident{ #(#names),* } => {
                                    #offset_expr
                                    #discr_expr;
                                    #(#exprs)*;
                                    #final_expr;
                                    bitvec
                                }
                            }
                        }
                    }
                });

                quote! {
                    let bitvec = match self {
                        #(#branches),*
                    };
                }
            }
            Data::Struct(fields) => {
                let exprs = fields.iter().enumerate().map(|(idx, field)| {
                    let ty = &field.ty;
                    let field = field.field(idx);

                    quote! {
                        bitvec = bitvec << <#ty as BitSize>::BITS;
                        bitvec = bitvec | self.#field.pack().cast::< Self::Packed >();

                    }
                });

                quote! { #(#exprs)* }
            }
        };

        fn make_exprs<'f>(
            fields: impl IntoIterator<Item = &'f BitPackField> + 'f,
        ) -> impl Iterator<Item = TokenStream> + 'f {
            fields.into_iter().enumerate().map(|(idx, field)| {
                let ty = &field.ty;
                let name = field.field_name(idx);

                quote! {
                    offset -= <#ty as BitSize>::BITS;
                    let #name = <#ty as BitPack>::unpack((packed.clone() >> offset).cast::< <#ty as BitPack>::Packed >());
                }
            })
        }

        let unpack = match &self.data {
            Data::Enum(variants) => {
                let discr_width = clog2_len(variants.len());
                let variant_expr = quote! {
                    let mut offset = <Self as BitSize>::BITS - #discr_width;
                    let variant: usize = usize::from((packed.clone() >> offset).cast::<BitVec<#discr_width>>());
                };

                let branches = variants.iter().enumerate().map(|(idx, variant)| {
                    let ident = &variant.ident;
                    let names = make_names(variant.fields.iter());
                    let exprs = make_exprs(variant.fields.iter());

                    match variant.fields.style {
                        Style::Unit => {
                            quote! {
                                #idx => Self::#ident()
                            }
                        }
                        Style::Tuple => {
                            quote! {
                                #idx => {
                                    #(#exprs)*
                                    Self::#ident( #(#names),* )
                                }
                            }
                        }
                        Style::Struct => {
                            quote! {
                                #idx => {
                                    #(#exprs)*
                                    Self::#ident{ #(#names),* }
                                }
                            }
                        }
                    }
                });

                quote! {
                    #variant_expr
                    match variant {
                        #(#branches),*
                        _ => {
                            panic!("invalid discriminant {variant}");
                        }
                    }
                }
            }
            Data::Struct(fields) => {
                let offset_expr = quote! {
                    let mut offset = <Self as BitSize>::BITS;
                };

                let names = make_names(fields.iter());
                let exprs = make_exprs(fields.iter());

                let res = match fields.style {
                    Style::Unit => quote! { #ident },
                    Style::Tuple => quote! { #ident( #(#names),* ) },
                    Style::Struct => quote! { #ident{ #(#names),* } },
                };

                quote! {
                    #offset_expr
                    #(#exprs)*
                    #res
                }
            }
        };

        let add_array_constr =
            self.generics.params.iter().any(|param| {
                matches!(param, GenericParam::Type(_) | GenericParam::Const(_))
            });

        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let predicates = predicates.chain(
            self.generics
                .type_params()
                .filter(|type_param| !self.attrs.contains_ty_param(type_param))
                .map(|type_param| {
                    let ident = &type_param.ident;

                    TEither::TS(quote! {
                        #ident: ::ferrum_hdl::bitpack::BitSize
                            + ::ferrum_hdl::bitpack::BitPack<
                                Packed = ::ferrum_hdl::bitvec::BitVec<{ < #ident as ::ferrum_hdl::bitpack::BitSize >::BITS }>
                            >
                    })
                }),
        );

        let predicates = predicates.chain(self.attrs.0.iter().map(TEither::AsIs));

        let predicates = if !add_array_constr {
            Either::Left(predicates)
        } else {
            Either::Right(predicates.chain(iter::once(TEither::TS(quote! {
                [(); <#ident #ty_generics as ::ferrum_hdl::bitpack::BitSize>::BITS]:
            }))))
        };

        let where_clause = utils::into_where_clause(predicates);

        quote! {
            #[automatically_derived]
            #[allow(unreachable_code)]
            impl #impl_generics ::ferrum_hdl::bitpack::BitPack for #ident #ty_generics
            #where_clause
            {
                type Packed = ::ferrum_hdl::bitvec::BitVec<{ <#ident #ty_generics as ::ferrum_hdl::bitpack::BitSize>::BITS }>;

                fn pack(self) -> Self::Packed {
                    use ::ferrum_hdl::bitpack::{BitSize, IsPacked};
                    use ::ferrum_hdl::cast::Cast;

                    let mut bitvec = Self::Packed::zero();

                    #pack

                    bitvec
                }

                fn unpack(mut packed: Self::Packed) -> Self {
                    use ::ferrum_hdl::bitpack::{BitPack, BitSize, IsPacked};
                    use ::ferrum_hdl::bitvec::BitVec;
                    use ::ferrum_hdl::cast::Cast;

                    #unpack
                }

            }

        }
    }
}
