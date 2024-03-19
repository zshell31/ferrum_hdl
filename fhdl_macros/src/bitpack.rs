use std::{borrow::Cow, iter};

use darling::{ast::Style, FromDeriveInput, FromGenerics};
use either::Either;
use fhdl_const_func::{clog2, clog2_len};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{GenericParam, Generics, Ident, Index};

use crate::utils::{self, AdtData, Bounds, Field, TEither, Variant};

pub struct BitPackDerive {
    ident: Ident,
    generics: Generics,
    data: AdtData,
    bounds: Bounds,
}

impl Field {
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

impl FromDeriveInput for BitPackDerive {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        Ok(Self {
            ident: input.ident.clone(),
            generics: Generics::from_generics(&input.generics)?,
            data: AdtData::try_from(&input.data)?,
            bounds: Bounds::from_attrs(&input.attrs, "bitpack")?,
        })
    }
}

fn discr_width(variants: &[Variant]) -> usize {
    let max_discr = variants
        .iter()
        .fold(0, |max_discr, variant| match variant.discr() {
            Some(discr) => {
                if max_discr <= discr {
                    discr
                } else {
                    max_discr
                }
            }
            None => max_discr,
        });

    if max_discr == 0 {
        clog2_len(variants.len())
    } else {
        clog2(max_discr)
    }
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
            AdtData::Enum(variants) => {
                if variants.is_empty() {
                    quote! { 0 }
                } else {
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

                    let discr_width = discr_width(variants);
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
            AdtData::Struct(fields) => {
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

        let predicates =
            self.bounds
                .extend_predicates(predicates, &self.generics, true, |tparam| {
                    let ident = &tparam.ident;

                    TEither::TS(quote! {
                        #ident: ::ferrum_hdl::bitpack::BitSize
                    })
                });

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
            fields: impl IntoIterator<Item = &'f Field> + 'f,
        ) -> impl Iterator<Item = Cow<'f, Ident>> + 'f {
            fields
                .into_iter()
                .enumerate()
                .map(|(idx, field)| field.field_name(idx))
        }

        let pack = match &self.data {
            AdtData::Enum(variants) => {
                let discr_width = discr_width(variants);

                let mut idx = 0;
                let branches = variants.iter().map(|variant| {
                    let variant_ident = &variant.ident;

                    let branch = variant.branch(&mut idx);
                    let discr_expr = quote! {
                        bitvec = bitvec << #discr_width;
                        bitvec = bitvec | Self::Packed::cast_from(#branch);
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
                                Self::#variant_ident => {
                                    #offset_expr
                                    #discr_expr;
                                    #final_expr;
                                    bitvec

                                }
                            }
                        }
                        Style::Tuple => {
                            quote! {
                                Self::#variant_ident( #(#names,)* ) => {
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
                                Self::#variant_ident{ #(#names,)* } => {
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
                        #(#branches,)*
                    };
                }
            }
            AdtData::Struct(fields) => {
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
            fields: impl IntoIterator<Item = &'f Field> + 'f,
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
            AdtData::Enum(variants) => {
                let discr_width = discr_width(variants);

                let variant_expr = quote! {
                    let mut offset = <Self as BitSize>::BITS - #discr_width;
                    let variant: usize = usize::cast_from((packed.clone() >> offset).cast::<BitVec<#discr_width>>());
                };

                let mut idx = 0;
                let branches = variants.iter().map(|variant| {
                    let ident = &variant.ident;
                    let names = make_names(variant.fields.iter());
                    let exprs = make_exprs(variant.fields.iter());

                    let branch = variant.branch(&mut idx);

                    match variant.fields.style {
                        Style::Unit => {
                            quote! {
                                #branch => Self::#ident
                            }
                        }
                        Style::Tuple => {
                            quote! {
                                #branch => {
                                    #(#exprs)*
                                    Self::#ident( #(#names,)* )
                                }
                            }
                        }
                        Style::Struct => {
                            quote! {
                                #branch => {
                                    #(#exprs)*
                                    Self::#ident{ #(#names,)* }
                                }
                            }
                        }
                    }
                });

                quote! {
                    #variant_expr
                    match variant {
                        #(#branches,)*
                        _ => {
                            panic!("invalid discriminant {variant}");
                        }
                    }
                }
            }
            AdtData::Struct(fields) => {
                let offset_expr = quote! {
                    let mut offset = <Self as BitSize>::BITS;
                };

                let names = make_names(fields.iter());
                let exprs = make_exprs(fields.iter());

                let res = match fields.style {
                    Style::Unit => quote! { #ident },
                    Style::Tuple => quote! { #ident( #(#names,)* ) },
                    Style::Struct => quote! { #ident{ #(#names,)* } },
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

        let predicates = self.bounds.extend_predicates(predicates, &self.generics, false, |tparam| {
            let ident = &tparam.ident;

            TEither::TS(quote! {
                #ident: ::ferrum_hdl::bitpack::BitSize
                    + ::ferrum_hdl::bitpack::BitPack<
                        Packed = ::ferrum_hdl::bitpack::BitVec<{ < #ident as ::ferrum_hdl::bitpack::BitSize >::BITS }>
                    >
            })
        });

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
                type Packed = ::ferrum_hdl::bitpack::BitVec<{ <#ident #ty_generics as ::ferrum_hdl::bitpack::BitSize>::BITS }>;

                fn pack(self) -> Self::Packed {
                    use ::ferrum_hdl::bitpack::{BitSize, IsPacked};
                    use ::ferrum_hdl::cast::{Cast, CastFrom};

                    let mut bitvec = Self::Packed::zero();

                    #pack

                    bitvec
                }

                fn unpack(mut packed: Self::Packed) -> Self {
                    use ::ferrum_hdl::bitpack::{BitPack, BitSize, IsPacked, BitVec};
                    use ::ferrum_hdl::cast::{Cast, CastFrom};

                    #unpack
                }

            }

        }
    }
}
