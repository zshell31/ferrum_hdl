use std::{
    fmt::{self, Binary, Display, LowerHex},
    ops::{BitAnd, BitOr, BitOrAssign, Not, Shl},
};

use ferrum_macros::{blackbox, blackbox_ty};

use crate::{
    bit::Bit,
    bit_pack::BitPack,
    const_helpers::{Assert, IsTrue},
    signal::SignalValue,
};

const fn bit_mask(n: usize) -> u128 {
    (1 << n) - 1
}

const fn msb_mask(n: u128) -> u128 {
    1 << (n - 1)
}

pub type BitVecInner = u128;

pub(crate) const fn msb_inner(inner: BitVecInner, width: usize) -> Bit {
    Bit::from_u128(inner & msb_mask(width as u128))
}

#[derive(Debug, Clone, Copy)]
#[blackbox_ty(BitVec)]
pub struct BitVec<const N: usize>(BitVecInner);

impl<const N: usize> BitVec<N> {
    const BIT_MASK: u128 = bit_mask(N);

    pub(crate) fn inner(&self) -> u128 {
        self.0 & Self::BIT_MASK
    }

    pub fn zero() -> Self {
        Self::from(0)
    }

    pub fn msb(&self) -> Bit {
        msb_inner(self.inner(), N)
    }

    #[blackbox(BitVecShrink)]
    pub fn shrink<const M: usize>(self) -> BitVec<M>
    where
        Assert<{ M < N }>: IsTrue,
    {
        BitVec::<M>::from(self.0)
    }

    #[blackbox(BitVecSlice)]
    pub fn slice<const S: usize, const M: usize>(self) -> BitVec<M>
    where
        Assert<{ M > 0 }>: IsTrue,
        Assert<{ S + M - 1 < N }>: IsTrue,
    {
        let mask = (1 << M) - 1;
        let inner = (self.inner() >> S) & mask;
        BitVec::<M>::from(inner)
    }

    #[blackbox(BitVecUnpack)]
    pub fn unpack<T: BitPack<Packed = Self>>(self) -> T {
        T::unpack(self)
    }
}

impl<const N: usize> SignalValue for BitVec<N> {}

impl<const N: usize> From<u128> for BitVec<N> {
    fn from(value: u128) -> Self {
        Self(value & Self::BIT_MASK)
    }
}

impl<const N: usize> Display for BitVec<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner(), f)
    }
}

impl<const N: usize> Binary for BitVec<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unsigned({:0width$b})", self.0, width = N)
    }
}

impl<const N: usize> LowerHex for BitVec<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.inner(), f)
    }
}

impl<const N: usize> PartialEq for BitVec<N> {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl<const N: usize> Eq for BitVec<N> {}

impl<const N: usize> BitAnd for BitVec<N> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self.0 & rhs.0).into()
    }
}

impl<const N: usize> BitOr for BitVec<N> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self.0 | rhs.0).into()
    }
}

impl<const N: usize> BitOrAssign for BitVec<N> {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = self.bitor(rhs)
    }
}

impl<const N: usize> Not for BitVec<N> {
    type Output = Self;

    fn not(self) -> Self::Output {
        (!self.0).into()
    }
}

impl<const N: usize> Shl<u128> for BitVec<N> {
    type Output = Self;

    fn shl(self, rhs: u128) -> Self::Output {
        Self::from(self.inner() << rhs)
    }
}
