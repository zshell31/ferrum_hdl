use std::fmt::{self, Binary, Display, LowerHex};
use std::ops::{Add, Div, Mul, Sub};

use crate::bit::Bit;
use crate::bit_pack::BitPack;
use crate::const_asserts::{Assert, IsTrue};
use crate::signal::SignalValue;

pub const fn unsigned_guard(n: usize) -> bool {
    n > 0 && n <= 64
}

const fn bit_mask(n: usize) -> usize {
    (1 << n) - 1
}

const fn msb_mask(n: usize) -> usize {
    1 << (n - 1)
}

#[derive(Debug, Clone, Copy)]
pub struct Unsigned<const N: usize>(usize)
where
    Assert<{ unsigned_guard(N) }>: IsTrue;

impl<const N: usize> Display for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<const N: usize> Binary for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Binary::fmt(&self.0, f)
    }
}

impl<const N: usize> LowerHex for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.0, f)
    }
}

impl<const N: usize> SignalValue for Unsigned<N> where Assert<{ unsigned_guard(N) }>: IsTrue {}

impl<const N: usize> Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    const BIT_MASK: usize = bit_mask(N);
    const MSB_MASK: usize = msb_mask(N);
}

impl<const N: usize> BitPack<N> for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    fn msb(&self) -> Bit {
        (self.0 & Self::MSB_MASK).into()
    }
}

impl<const N: usize> From<usize> for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    fn from(value: usize) -> Self {
        Self(value & Self::BIT_MASK)
    }
}

impl<const N: usize> Add for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.0.wrapping_add(rhs.0).into()
    }
}

impl<const N: usize> Add<usize> for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        self.add(Self::from(rhs))
    }
}

impl<const N: usize> Sub for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0.wrapping_sub(rhs.0).into()
    }
}

impl<const N: usize> Sub<usize> for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        self.sub(Self::from(rhs))
    }
}

impl<const N: usize> Mul for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.0.wrapping_mul(rhs.0).into()
    }
}

impl<const N: usize> Mul<usize> for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn mul(self, rhs: usize) -> Self::Output {
        self.mul(Self::from(rhs))
    }
}

impl<const N: usize> Div for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self.0.wrapping_div(rhs.0).into()
    }
}

impl<const N: usize> Div<usize> for Unsigned<N>
where
    Assert<{ unsigned_guard(N) }>: IsTrue,
{
    type Output = Self;

    fn div(self, rhs: usize) -> Self::Output {
        self.div(Self::from(rhs))
    }
}
