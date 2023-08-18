use std::{
    fmt::{self, Binary, Display, LowerHex},
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    const_asserts::{Assert, IsTrue},
    signal::SignalValue,
};

pub const fn is_unsigned(n: u8) -> bool {
    n > 0 && n <= 128
}

const fn bit_mask(n: u8) -> u128 {
    // TODO: n == 128?
    if n == 128 {
        u128::MAX
    } else {
        (1 << n) - 1
    }
}

// const fn msb_mask(n: u8) -> u128 {
//     1 << (n - 1)
// }

#[derive(Debug, Clone, Copy)]
pub struct Unsigned<const N: u8>(u128)
where
    Assert<{ is_unsigned(N) }>: IsTrue;

impl<const N: u8> Display for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<const N: u8> Binary for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Binary::fmt(&self.0, f)
    }
}

impl<const N: u8> LowerHex for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.0, f)
    }
}

impl<const N: u8> SignalValue for Unsigned<N> where Assert<{ is_unsigned(N) }>: IsTrue {}

impl<const N: u8> Unsigned<N> where Assert<{ is_unsigned(N) }>: IsTrue {}

pub fn unsigned_value(value: u128, width: u8) -> u128 {
    value & bit_mask(width)
}

impl<const N: u8> From<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn from(value: u128) -> Self {
        Self(unsigned_value(value, N))
    }
}

impl<const N: u8> Add for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.0.wrapping_add(rhs.0).into()
    }
}

impl<const N: u8> Add<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn add(self, rhs: u128) -> Self::Output {
        self.add(Self::from(rhs))
    }
}

impl<const N: u8> Sub for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0.wrapping_sub(rhs.0).into()
    }
}

impl<const N: u8> Sub<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn sub(self, rhs: u128) -> Self::Output {
        self.sub(Self::from(rhs))
    }
}

impl<const N: u8> Mul for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.0.wrapping_mul(rhs.0).into()
    }
}

impl<const N: u8> Mul<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn mul(self, rhs: u128) -> Self::Output {
        self.mul(Self::from(rhs))
    }
}

impl<const N: u8> Div for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self.0.wrapping_div(rhs.0).into()
    }
}

impl<const N: u8> Div<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn div(self, rhs: u128) -> Self::Output {
        self.div(Self::from(rhs))
    }
}
