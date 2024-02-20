use fhdl_macros::blackbox;

use crate::{
    bitpack::{BitPack, BitSize},
    bitvec::BitVec,
    cast::{Cast, CastFrom},
    signal::SignalValue,
};

pub type Bit = bool;

pub const fn bit_value(value: bool) -> u128 {
    match value {
        false => 0,
        true => 1,
    }
}

impl SignalValue for Bit {}

impl BitSize for Bit {
    const BITS: usize = 1;
}

impl CastFrom<Bit> for Bit {
    #[blackbox(CastFrom)]
    fn cast_from(from: Bit) -> Self {
        from
    }
}

impl BitPack for Bit {
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

pub const H: Bit = true;
pub const L: Bit = false;
