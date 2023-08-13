use std::ops::{BitAnd, BitOr, Not};

use crate::const_asserts::{Assert, IsTrue};

pub const fn valid(n: usize) -> bool {
    n > 0 && n <= 128
}

const fn bit_mask(n: usize) -> u128 {
    (1 << n) - 1
}

pub struct BitVec<const N: usize>(u128)
where
    Assert<{ valid(N) }>: IsTrue;

impl<const N: usize> BitVec<N>
where
    Assert<{ valid(N) }>: IsTrue,
{
    const BIT_MASK: u128 = bit_mask(N);
}

impl<const N: usize> From<u128> for BitVec<N>
where
    Assert<{ valid(N) }>: IsTrue,
{
    fn from(value: u128) -> Self {
        Self(value & Self::BIT_MASK)
    }
}

impl<const N: usize> BitAnd for BitVec<N>
where
    Assert<{ valid(N) }>: IsTrue,
{
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self.0 & rhs.0).into()
    }
}

impl<const N: usize> BitOr for BitVec<N>
where
    Assert<{ valid(N) }>: IsTrue,
{
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self.0 | rhs.0).into()
    }
}

impl<const N: usize> Not for BitVec<N>
where
    Assert<{ valid(N) }>: IsTrue,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        (!self.0).into()
    }
}
