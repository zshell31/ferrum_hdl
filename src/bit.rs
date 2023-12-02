use std::{
    fmt::{self, Binary, Debug, Display},
    ops::{BitAnd, BitOr, Not},
};

use fhdl_macros::{blackbox, blackbox_ty};

use crate::{
    bitpack::{BitPack, BitSize},
    bitvec::BitVec,
    cast::{Cast, CastFrom, IsPrimTy},
    signal::SignalValue,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[blackbox_ty(Bit)]
#[repr(transparent)]
pub struct Bit(pub bool);

pub const fn bit_value(value: bool) -> u128 {
    match value {
        false => 0,
        true => 1,
    }
}

impl SignalValue for Bit {}

impl IsPrimTy for Bit {}

impl IsPrimTy for bool {}

impl BitSize for Bit {
    const BITS: usize = 1;
}

impl BitPack for Bit {
    type Packed = BitVec<1>;

    fn pack(self) -> Self::Packed {
        bool::cast_from(self).pack()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        bool::unpack(bitvec).cast()
    }
}

impl BitSize for bool {
    const BITS: usize = 1;
}

impl BitPack for bool {
    type Packed = BitVec<1>;

    fn pack(self) -> Self::Packed {
        (match self {
            true => 1_u8,
            false => 0,
        })
        .cast()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        bitvec.is_non_zero()
    }
}

#[blackbox(BitH)]
pub const H: Bit = Bit(true);
#[blackbox(BitL)]
pub const L: Bit = Bit(false);

impl CastFrom<Bit> for Bit {
    fn cast_from(from: Bit) -> Self {
        from
    }
}

impl CastFrom<bool> for bool {
    fn cast_from(from: bool) -> Self {
        from
    }
}

impl CastFrom<bool> for Bit {
    fn cast_from(value: bool) -> Bit {
        Bit(value)
    }
}

impl CastFrom<Bit> for bool {
    fn cast_from(bit: Bit) -> Self {
        bit.0
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

impl Binary for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Not for Bit {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::cast_from(!self.0)
    }
}

impl BitAnd for Bit {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::cast_from(self.0 && rhs.0)
    }
}

impl BitAnd<bool> for Bit {
    type Output = Self;

    fn bitand(self, rhs: bool) -> Self::Output {
        Self::cast_from(self.0 && rhs)
    }
}

impl BitAnd<Bit> for bool {
    type Output = Bit;

    fn bitand(self, rhs: Bit) -> Self::Output {
        Bit::cast_from(self && rhs.0)
    }
}

impl BitOr for Bit {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::cast_from(self.0 || rhs.0)
    }
}

impl BitOr<bool> for Bit {
    type Output = Self;

    fn bitor(self, rhs: bool) -> Self::Output {
        Self::cast_from(self.0 || rhs)
    }
}

impl BitOr<Bit> for bool {
    type Output = Bit;

    fn bitor(self, rhs: Bit) -> Self::Output {
        Bit::cast_from(self || rhs.0)
    }
}
