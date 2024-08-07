use std::{
    marker::PhantomData,
    ops::{BitAnd, BitOr, Shl, Shr},
};

pub use fhdl_macros::BitPack;
use fhdl_macros::{blackbox, synth};

use crate::{
    bit::Bit,
    cast::{Cast, CastFrom},
    const_functions::{assert_extend, assert_in_range, idx_range_len},
    const_helpers::ConstConstr,
    index::{idx_constr, Idx},
    signed::S,
    unsigned::U,
};

pub trait BitSize: Sized {
    const BITS: usize;
}

pub trait IsPacked:
    Sized
    + Clone
    + BitAnd
    + BitOr
    + Shl<usize>
    + Shr<usize>
    + CastFrom<Self>
    + CastFrom<usize>
{
    fn zero() -> Self;
}

pub type BitVec<const N: usize> = U<N>;

impl<const N: usize> IsPacked for BitVec<N> {
    #[inline]
    fn zero() -> Self {
        Self::cast_from(0_u8)
    }
}

impl<const N: usize> BitVec<N> {
    #[blackbox(BitPackUnpack)]
    pub fn unpack<T: BitPack<Packed = Self>>(self) -> T {
        T::unpack(self)
    }
}

pub trait BitPack: BitSize {
    type Packed: IsPacked;

    #[blackbox(BitPackPack)]
    fn pack(self) -> Self::Packed;

    #[blackbox(BitPackUnpack)]
    fn unpack(packed: Self::Packed) -> Self;

    #[synth(inline)]
    fn repack<T: BitPack<Packed = Self::Packed>>(self) -> T {
        let bitvec = self.pack();
        T::unpack(bitvec)
    }
}

impl<T> BitSize for PhantomData<T> {
    const BITS: usize = 0;
}

impl<T> BitPack for PhantomData<T> {
    type Packed = BitVec<0>;

    fn pack(self) -> Self::Packed {
        BitVec::<0>::zero()
    }

    fn unpack(_: Self::Packed) -> Self {
        PhantomData
    }
}

pub trait BitPackExt<const N: usize>: BitPack<Packed = BitVec<N>> + Clone {
    #[synth(inline)]
    #[inline]
    fn pack(&self) -> Self::Packed
    where
        Self: Clone,
    {
        self.clone().pack()
    }

    #[synth(inline)]
    #[inline]
    fn repack<T: BitPack<Packed = Self::Packed>>(&self) -> T
    where
        Self: Clone,
    {
        self.clone().repack()
    }

    #[blackbox(Index)]
    #[inline]
    fn bit(&self, idx: Idx<N>) -> Bit
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        self.pack().bit_(idx.cast())
    }

    #[synth(inline)]
    #[inline]
    fn bit_const<const START: usize>(&self) -> Bit
    where
        ConstConstr<{ idx_constr(N) }>:,
        ConstConstr<{ assert_in_range(N, START, 1) }>:,
    {
        let bit = self.bit(START.cast());
        bit
    }

    #[blackbox(Slice)]
    #[inline]
    fn slice<const LEN: usize>(&self, idx: Idx<{ idx_range_len(N, LEN) }>) -> U<LEN>
    where
        ConstConstr<{ idx_constr(idx_range_len(N, LEN)) }>:,
    {
        self.pack().slice_::<LEN>(idx.cast())
    }

    #[synth(inline)]
    #[inline]
    fn slice_const<const M: usize, const START: usize>(&self) -> U<M>
    where
        ConstConstr<{ idx_constr(idx_range_len(N, M)) }>:,
        ConstConstr<{ assert_in_range(N, START, M) }>:,
    {
        let slice = self.slice::<M>(START.cast());
        slice
    }

    #[synth(inline)]
    #[inline]
    fn msb(&self) -> Bit
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        let msb = self.bit(unsafe { Idx::from_usize(N - 1) });
        msb
    }

    #[synth(inline)]
    fn rotate_left(&self) -> Self
    where
        ConstConstr<{ idx_constr(N) }>:,
        ConstConstr<{ idx_constr(idx_range_len(N, N - 1)) }>:,
    {
        let packed = self.pack();
        let msb = packed.bit(unsafe { Idx::from_usize(N - 1) });
        let slice = packed.slice::<{ N - 1 }>(unsafe { Idx::from_usize(0) });

        let rotate = Self::unpack(
            (slice.cast::<U<N>>() << 1_u8.cast::<U<1>>()) | msb.cast::<U<N>>(),
        );
        rotate
    }

    #[synth(inline)]
    fn zero_extend<const M: usize, T: BitPack<Packed = BitVec<M>>>(&self) -> T
    where
        ConstConstr<{ assert_extend(N, M) }>:,
    {
        let zextend = self.pack().cast::<U<M>>().repack();
        zextend
    }

    #[synth(inline)]
    fn sign_extend<const M: usize, T: BitPack<Packed = BitVec<M>>>(&self) -> T
    where
        ConstConstr<{ assert_extend(N, M) }>:,
    {
        let sextend = self.pack().cast::<S<M>>().repack();
        sextend
    }
}

impl<const N: usize, T> BitPackExt<N> for T where T: BitPack<Packed = BitVec<N>> + Clone {}

#[cfg(test)]
mod tests {
    use crate::{
        array::Array,
        bit::{Bit, H, L},
        cast::Cast,
        prelude::BitPack,
        unsigned::U,
    };

    #[test]
    fn repack() {
        let u: U<6> = 0b011011_u8.cast();
        assert_eq!(
            u.clone().repack::<Array<3, U<2>>>(),
            [0b01_u8, 0b10, 0b11].cast::<Array<3, U<2>>>()
        );

        assert_eq!(
            u.repack::<Array<2, Array<1, Array<3, Bit>>>>(),
            [[[L, H, H]], [[L, H, H]]].cast::<Array<2, Array<1, Array<3, Bit>>>>()
        );
    }
}
