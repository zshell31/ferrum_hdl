use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    Ident, Index, LitInt,
};

pub struct ImplTupleTraits {
    count: usize,
    tparams: Vec<Ident>,
    indexes: Vec<Index>,
}

impl Parse for ImplTupleTraits {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let count = input.parse::<LitInt>()?;
        let count = count.base10_parse::<usize>()?;

        let tparams = make_tuple_params("T", count).collect();

        let indexes = (0 .. count).map(Index::from).collect();

        Ok(Self {
            count,
            tparams,
            indexes,
        })
    }
}

fn make_tuple_params(prefix: &str, count: usize) -> impl Iterator<Item = Ident> + '_ {
    (0 .. count).map(move |idx| Ident::new(&format!("{prefix}{idx}"), Span::call_site()))
}

impl ImplTupleTraits {
    pub fn into_tokens(self) -> TokenStream {
        let impl_signal_value = self.impl_signal_value();
        let impl_unbundle = self.impl_unbundle();
        let impl_bundle = self.impl_bundle();
        let impl_eval = self.impl_eval();
        let impl_cast_from = self.impl_cast_from();
        let impl_bit_size = self.impl_bit_size();
        let impl_bit_pack = self.impl_bit_pack();

        quote! {
            #impl_signal_value

            #impl_unbundle

            #impl_bundle

            #impl_eval

            #impl_cast_from

            #impl_bit_size

            #impl_bit_pack
        }
    }

    fn impl_signal_value(&self) -> TokenStream {
        let t = &self.tparams;

        quote! {
            impl<#( #t: SignalValue, )*> SignalValue for ( #( #t, )* ) {}
        }
    }

    fn impl_unbundle(&self) -> TokenStream {
        let t = &self.tparams;
        let n = &self.indexes;

        quote! {
            impl<D: ClockDomain, #( #t: SignalValue, )*> Unbundle for Signal<D, ( #( #t, )* )> {
                type Unbundled = ( #( Signal<D, #t>, )* );

                fn unbundle(self) -> Self::Unbundled {
                    (
                        #(
                            self.clone().map(|values| values.#n),
                        )*
                    )
                }
            }
        }
    }

    fn impl_bundle(&self) -> TokenStream {
        let t = &self.tparams;
        let n = &self.indexes;

        quote! {
            impl<D: ClockDomain, #( #t: SignalValue, )*> Bundle for ( #( Signal<D, #t>, )* ) {
                type Bundled = Signal<D, ( #( #t, )* )>;

                fn bundle(mut self) -> Self::Bundled {
                    Signal::new(move |ctx| {
                        (
                            #(
                                self.#n.next(ctx),
                            )*
                        )
                    })
                }
            }
        }
    }

    fn impl_eval(&self) -> TokenStream {
        let t = &self.tparams;
        let n = &self.indexes;

        quote! {
            impl<D: ClockDomain, #( #t: SignalValue, )*> Eval<D> for ( #( Signal<D, #t>, )* ) {
                type Value = ( #( #t, )* );

                fn next(&mut self, ctx: &mut EvalCtx) -> Self::Value {
                    (
                        #(
                            self.#n.next(ctx),
                        )*
                    )
                }
            }
        }
    }

    fn impl_cast_from(&self) -> TokenStream {
        let t = &self.tparams;
        let u = make_tuple_params("U", self.count).collect::<Vec<_>>();

        let n = &self.indexes;

        quote! {
            impl< #( #u, #t: CastFrom<#u>, )* > CastFrom< ( #(#u,)* ) > for ( #(#t,)* ) {
                #[fhdl_macros::synth(inline)]
                fn cast_from(from: ( #( #u, )* ) ) -> ( #( #t, )* ) {
                    (
                        #(
                            #t::cast_from(from.#n),
                        )*
                    )

                }
            }
        }
    }

    fn impl_bit_size(&self) -> TokenStream {
        let t = &self.tparams;

        let tparam = &t[0];
        let mut terms = quote! { <#tparam as BitSize>::BITS };
        for tparam in t.iter().skip(1) {
            terms.extend(quote! { + <#tparam as BitSize>::BITS });
        }

        quote! {
            impl< #( #t: BitSize, )* > BitSize for ( #( #t, )* ) {
                const BITS: usize = #terms;
            }
        }
    }

    fn impl_bit_pack(&self) -> TokenStream {
        let t = &self.tparams;
        let n = &self.indexes;

        let names = (0 .. self.count)
            .map(|name| Ident::new(&format!("_var_{name}"), Span::call_site()))
            .collect::<Vec<_>>();

        let offset_expr = quote! {
            let mut offset = <Self as BitSize>::BITS;
        };

        let unpack = self.tparams.iter().zip(names.iter()).map(|(t, name)|
            quote! {
                offset -= <#t as BitSize>::BITS;
                let #name = <#t as BitPack>::unpack((packed.clone() >> offset).cast::< <#t as BitPack>::Packed >());
            }
        );

        let self_ = quote! { ( #( #t, )* ) };

        quote! {
            impl< #( #t, )* > BitPack for ( #( #t, )* )
            where
                #(
                    #t: BitPack<Packed = BitVec<{ <#t as BitSize>::BITS }> >,
                )*
                [(); <#self_ as BitSize>::BITS]:
            {
                type Packed = BitVec<{ <#self_ as BitSize>::BITS }>;

                fn pack(self) -> Self::Packed {
                    let mut bitvec = Self::Packed::zero();

                    #(
                        bitvec = bitvec << <#t as BitSize>::BITS;
                        bitvec = bitvec | self.#n.pack().cast::<Self::Packed>();
                    )*

                    bitvec
                }

                fn unpack(mut packed: Self::Packed) -> Self {
                    #offset_expr

                    #(#unpack)*;

                    (
                        #(#names,)*
                    )
                }
            }
        }
    }
}
