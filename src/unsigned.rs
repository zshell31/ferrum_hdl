use std::{
    fmt::{self, Binary, Display, LowerHex},
    mem,
    ops::{Add, Div, Mul, Sub},
};

use ferrum_netlist::sig_ty::{IsPrimTy, PrimTy};

use crate::{
    bit::Bit,
    bit_pack::BitPack,
    const_asserts::{Assert, IsTrue},
    signal::SignalValue,
    CastInner,
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

const fn msb_mask(n: u8) -> u128 {
    1 << (n - 1)
}

#[inline(always)]
pub fn unsigned_value(value: u128, width: u8) -> u128 {
    value & bit_mask(width)
}

pub fn u<const N: u8>(n: u128) -> Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    n.into()
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Unsigned<const N: u8>(u128)
where
    Assert<{ is_unsigned(N) }>: IsTrue;

impl<const N: u8> Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    pub(crate) fn new(n: u128) -> Self {
        Self(unsigned_value(n, N))
    }

    pub(crate) fn inner(self) -> u128 {
        unsigned_value(self.0, N)
    }
}

impl<const N: u8> CastInner<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn cast(self) -> u128 {
        unsafe { mem::transmute::<Unsigned<N>, u128>(self) }
    }
}

impl<const N: u8> CastInner<Unsigned<N>> for u128
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn cast(self) -> Unsigned<N> {
        unsafe { mem::transmute::<u128, Unsigned<N>>(self) }
    }
}

impl<const N: u8> Display for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner(), f)
    }
}

impl<const N: u8> Binary for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Binary::fmt(&self.inner(), f)
    }
}

impl<const N: u8> LowerHex for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.inner(), f)
    }
}

impl<const N: u8> SignalValue for Unsigned<N> where Assert<{ is_unsigned(N) }>: IsTrue {}

impl<const N: u8> IsPrimTy for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    const PRIM_TY: PrimTy = PrimTy::Unsigned(N);
}

impl<const N: u8> Unsigned<N> where Assert<{ is_unsigned(N) }>: IsTrue {}

impl<const N: u8> PartialEq for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl<const N: u8> PartialEq<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn eq(&self, other: &u128) -> bool {
        self.inner() == *other
    }
}

impl<const N: u8> Eq for Unsigned<N> where Assert<{ is_unsigned(N) }>: IsTrue {}

impl<const N: u8> BitPack<N> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn msb(&self) -> Bit {
        Bit::from_u128(self.inner() & msb_mask(N))
    }
}

impl<const N: u8> From<u128> for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn from(value: u128) -> Self {
        Self::new(value)
    }
}

impl<const N: u8> From<Unsigned<N>> for u128
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    fn from(value: Unsigned<N>) -> Self {
        value.inner()
    }
}

impl<const N: u8> Add for Unsigned<N>
where
    Assert<{ is_unsigned(N) }>: IsTrue,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.inner().wrapping_add(rhs.inner()).into()
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
        self.inner().wrapping_sub(rhs.inner()).into()
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
        self.inner().wrapping_mul(rhs.inner()).into()
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
        self.inner().wrapping_div(rhs.inner()).into()
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
