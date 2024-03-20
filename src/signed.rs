use std::{cmp::Ordering, io};

use fhdl_macros::{blackbox, blackbox_ty, synth};
use num_bigint::{BigInt, Sign};
use vcd::IdCode;

use crate::{
    bit::{self, Bit},
    bitpack::BitSize,
    cast::{Cast, CastFrom},
    prelude::{
        Assert, BitPack, BitVec, IsTrue, SignalValue, TraceTy, TraceVars, Traceable,
        Tracer, U,
    },
};

#[derive(Debug, Clone)]
#[blackbox_ty(Signed)]
pub enum S<const N: usize> {
    Short(i128),
    Long(BigInt),
}

impl<const N: usize> S<N>
where
    Assert<{ N <= 128 }>: IsTrue,
{
    pub const fn from(val: i128) -> Self {
        Self::Short(val)
    }
}

impl<const N: usize> S<N> {
    fn from_short(val: i128) -> Self {
        match N.cmp(&128) {
            Ordering::Less => {
                let mask = (1 << N) - 1;
                Self::Short(val & mask)
            }
            Ordering::Equal => Self::Short(val),
            Ordering::Greater => {
                let mask = (BigInt::from(1_u8) << N) - 1_u8;
                Self::Long(BigInt::from(val) & mask)
            }
        }
    }

    fn from_long(val: BigInt) -> Self {
        match N.cmp(&128) {
            Ordering::Less => {
                let mask = (1 << N) - 1;
                Self::Short(i128::try_from(val).unwrap() & mask)
            }
            Ordering::Equal => Self::Short(i128::try_from(val).unwrap()),
            Ordering::Greater if val.bits() as usize == N => Self::Long(val),
            _ => {
                let mask = (BigInt::from(1_u8) << N) - 1_u8;
                Self::Long(val & mask)
            }
        }
    }
}

fn bit_to_sign(bit: Bit) -> Sign {
    match bit {
        bit::H => Sign::Minus,
        bit::L => Sign::Plus,
    }
}

impl<const N: usize> SignalValue for S<N> {}

impl<const N: usize> BitSize for S<N> {
    const BITS: usize = N;
}

impl<const N: usize> BitPack for S<N> {
    type Packed = BitVec<N>;

    fn pack(self) -> Self::Packed {
        match self {
            Self::Short(val) => BitVec::Short(val as u128),
            Self::Long(val) => BitVec::Long(val.into_parts().1),
        }
    }

    fn unpack(packed: Self::Packed) -> Self {
        let msb = packed.bit_(N - 1);
        match packed {
            BitVec::Short(val) => Self::Short(val as i128),
            BitVec::Long(val) => {
                let sign = bit_to_sign(msb);
                Self::Long(BigInt::from_biguint(sign, val))
            }
        }
    }
}

impl<const N: usize> Default for S<N> {
    #[synth(inline)]
    #[inline]
    fn default() -> Self {
        0_i8.cast()
    }
}

macro_rules! impl_for_signed_prim_ty {
    ($( $prim:ty ),+) => {
        $(
            impl SignalValue for $prim {}

            impl <const N: usize> CastFrom<$prim> for S<N> {
                #[blackbox(CastFrom)]
                #[inline]
                fn cast_from(val: $prim) -> Self {
                    Self::from_short(val as i128)
                }
            }

            impl <const N: usize> CastFrom<$prim> for U<N> {
                #[synth(inline)]
                #[inline]
                fn cast_from(val: $prim) -> Self {
                    val.cast::<S<N>>().cast()
                }
            }

            impl <const N: usize> CastFrom<S<N>> for $prim {
                #[blackbox(CastFrom)]
                #[inline]
                fn cast_from(val: S<N>) -> Self {
                    match val {
                        S::Short(short) => short as $prim,
                        S::Long(long) => long.try_into().unwrap()
                    }
                }
            }

            impl <const N: usize> CastFrom<U<N>> for $prim {
                #[synth(inline)]
                #[inline]
                fn cast_from(val: U<N>) -> Self {
                    val.cast::<U<N>>().cast()
                }
            }

            impl BitSize for $prim {
                const BITS: usize = <$prim>::BITS as usize;
            }

            impl BitPack for $prim {
                type Packed = BitVec<{ <$prim as BitSize>::BITS }>;

                #[inline]
                fn pack(self) -> Self::Packed {
                    self.cast()
                }

                #[inline]
                fn unpack(bitvec: Self::Packed) -> Self {
                    bitvec.cast()
                }
            }

        )+
    };
}

impl_for_signed_prim_ty!(i8, i16, i32, i64, i128, isize);

impl<const N: usize, const M: usize> CastFrom<S<M>> for S<N> {
    #[blackbox(CastFrom)]
    fn cast_from(from: S<M>) -> S<N> {
        match from {
            S::<M>::Short(short) => S::<N>::from_short(short),
            S::<M>::Long(long) => S::<N>::from_long(long),
        }
    }
}

impl<const N: usize, const M: usize> CastFrom<S<N>> for U<M> {
    #[synth(inline)]
    #[inline]
    fn cast_from(from: S<N>) -> Self {
        let cast = from.repack::<U<N>>().cast();
        cast
    }
}

impl<const N: usize, const M: usize> CastFrom<U<N>> for S<M> {
    #[synth(inline)]
    #[inline]
    fn cast_from(from: U<N>) -> Self {
        let cast = from.repack::<S<N>>().cast();
        cast
    }
}

macro_rules! impl_trace_for_prims {
    ($( $prim:ty => $cast:ty ),+) => {
        $(
            impl Traceable for $prim
            {
                #[inline]
                fn add_vars(vars: &mut TraceVars) {
                    vars.add_ty(TraceTy::Bus(Self::BITS));
                }

                #[inline]
                fn trace(
                    &self,
                    id: &mut IdCode,
                    tracer: &mut Tracer,
                ) -> io::Result<()> {
                    (*self as $cast).trace(id, tracer)
                }
            }
        )+
    };
}

impl_trace_for_prims!(i8 => u8, i16 => u16, i32 => u32, i64 => u64, i128 => u128, isize => usize);

impl<const N: usize> Traceable for S<N> {
    #[inline]
    fn add_vars(vars: &mut TraceVars) {
        vars.add_ty(TraceTy::Bus(N as u32));
    }

    #[inline]
    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        self.clone().cast::<U<N>>().trace(id, tracer)
    }
}
