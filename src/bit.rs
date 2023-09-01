use std::{
    fmt::{self, Debug, Display},
    ops::{BitAnd, BitOr, Not},
};

use crate::{
    prim_ty::{IsPrimTy, PrimTy},
    signal::SignalValue,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bit(bool);

pub fn bit_value(value: bool) -> u128 {
    match value {
        false => 0,
        true => 1,
    }
}

impl Display for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self.0 {
            true => "H",
            false => "L",
        })
    }
}

impl Debug for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl SignalValue for Bit {}

impl IsPrimTy for Bit {
    const PRIM_TY: PrimTy = PrimTy::Bit;
}

impl Bit {
    pub(crate) fn from_u128(value: u128) -> Self {
        Self::from(value > 0)
    }
}

pub const H: Bit = Bit(true);
pub const L: Bit = Bit(false);

impl From<bool> for Bit {
    fn from(value: bool) -> Bit {
        Bit(value)
    }
}

impl From<Bit> for bool {
    fn from(bit: Bit) -> Self {
        bit.0
    }
}

impl Not for Bit {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::from(!self.0)
    }
}

impl BitAnd for Bit {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::from(self.0 && rhs.0)
    }
}

impl BitAnd<bool> for Bit {
    type Output = Self;

    fn bitand(self, rhs: bool) -> Self::Output {
        Self::from(self.0 && rhs)
    }
}

impl BitAnd<Bit> for bool {
    type Output = Bit;

    fn bitand(self, rhs: Bit) -> Self::Output {
        Bit::from(self && rhs.0)
    }
}

impl BitOr for Bit {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::from(self.0 || rhs.0)
    }
}

impl BitOr<bool> for Bit {
    type Output = Self;

    fn bitor(self, rhs: bool) -> Self::Output {
        Self::from(self.0 || rhs)
    }
}

impl BitOr<Bit> for bool {
    type Output = Bit;

    fn bitor(self, rhs: Bit) -> Self::Output {
        Bit::from(self || rhs.0)
    }
}
