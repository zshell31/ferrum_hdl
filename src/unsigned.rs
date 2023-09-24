use std::{
    cmp::Ordering,
    fmt::{self, Binary, Display, LowerHex},
    ops::{Add, BitAnd, BitOr, Div, Mul, Shl, Shr, Sub},
};

use ferrum_netlist::sig_ty::PrimTy;

use crate::{
    bit_pack::{BitPack, BitSize},
    bit_vec::BitVec,
    cast::{CastInner, IsPrimTy},
    signal::SignalValue,
};

const fn bit_mask(n: u128) -> u128 {
    // TODO: n == 128?
    if n == 128 {
        u128::MAX
    } else {
        (1 << n) - 1
    }
}

#[inline(always)]
pub const fn unsigned_value(value: u128, width: u128) -> u128 {
    value & bit_mask(width)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Unsigned<const N: usize>(u128);

impl<const N: usize> Unsigned<N> {
    pub const fn new(n: u128) -> Self {
        Self(unsigned_value(n, N as u128))
    }
}

impl<const N: usize> CastInner<u128> for Unsigned<N> {
    fn cast_inner(self) -> u128 {
        self.0
    }
}

impl<const N: usize> CastInner<Unsigned<N>> for u128 {
    fn cast_inner(self) -> Unsigned<N> {
        Unsigned::<N>::new(self)
    }
}

impl<const N: usize> SignalValue for Unsigned<N> {}

impl<const N: usize> IsPrimTy for Unsigned<N> {
    const PRIM_TY: PrimTy = PrimTy::Unsigned(N as u128);
}

impl<const N: usize> BitSize for Unsigned<N> {
    const BITS: usize = N;
}

impl<const N: usize> BitPack for Unsigned<N> {
    type Packed = BitVec<N>;

    fn pack(&self) -> Self::Packed {
        BitVec::from(self.0)
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        Self::from(bitvec.inner())
    }
}

impl<const N: usize> const From<u128> for Unsigned<N> {
    fn from(value: u128) -> Self {
        Self::new(value)
    }
}

impl<const N: usize> Display for Unsigned<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unsigned({})", self.0)
    }
}

impl<const N: usize> Binary for Unsigned<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unsigned({:0width$b})", self.0, width = N)
    }
}

impl<const N: usize> LowerHex for Unsigned<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

impl<const N: usize> PartialEq<u128> for Unsigned<N> {
    fn eq(&self, other: &u128) -> bool {
        self.0 == *other
    }
}

impl<const N: usize> PartialOrd<u128> for Unsigned<N> {
    fn partial_cmp(&self, other: &u128) -> Option<Ordering> {
        Some(self.0.cmp(other))
    }
}

impl<const N: usize> BitAnd for Unsigned<N> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self.0 & rhs.0).into()
    }
}

impl<const N: usize> BitAnd<u128> for Unsigned<N> {
    type Output = Self;

    fn bitand(self, rhs: u128) -> Self::Output {
        self.bitand(Self::from(rhs))
    }
}

impl<const N: usize> BitOr for Unsigned<N> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self.0 | rhs.0).into()
    }
}

impl<const N: usize> BitOr<u128> for Unsigned<N> {
    type Output = Self;

    fn bitor(self, rhs: u128) -> Self::Output {
        self.bitor(Self::from(rhs))
    }
}

impl<const N: usize> Shl for Unsigned<N> {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        (self.0 << rhs.0).into()
    }
}

impl<const N: usize> Shl<u128> for Unsigned<N> {
    type Output = Self;

    fn shl(self, rhs: u128) -> Self::Output {
        self.shl(Self::from(rhs))
    }
}

impl<const N: usize> Shr for Unsigned<N> {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        (self.0 >> rhs.0).into()
    }
}

impl<const N: usize> Shr<u128> for Unsigned<N> {
    type Output = Self;

    fn shr(self, rhs: u128) -> Self::Output {
        self.shr(Self::from(rhs))
    }
}

impl<const N: usize> Add for Unsigned<N> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.0.wrapping_add(rhs.0).into()
    }
}

impl<const N: usize> Add<u128> for Unsigned<N> {
    type Output = Self;

    fn add(self, rhs: u128) -> Self::Output {
        self.add(Self::from(rhs))
    }
}

impl<const N: usize> Sub for Unsigned<N> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0.wrapping_sub(rhs.0).into()
    }
}

impl<const N: usize> Sub<u128> for Unsigned<N> {
    type Output = Self;

    fn sub(self, rhs: u128) -> Self::Output {
        self.sub(Self::from(rhs))
    }
}

impl<const N: usize> Mul for Unsigned<N> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.0.wrapping_mul(rhs.0).into()
    }
}

impl<const N: usize> Mul<u128> for Unsigned<N> {
    type Output = Self;

    fn mul(self, rhs: u128) -> Self::Output {
        self.mul(Self::from(rhs))
    }
}

impl<const N: usize> Div for Unsigned<N> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self.0.wrapping_div(rhs.0).into()
    }
}

impl<const N: usize> Div<u128> for Unsigned<N> {
    type Output = Self;

    fn div(self, rhs: u128) -> Self::Output {
        self.div(Self::from(rhs))
    }
}
