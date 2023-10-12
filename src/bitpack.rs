use std::ops::{BitAnd, BitOr, Shl, Shr};

use fhdl_macros::blackbox;

use crate::{bitvec::BitVec, cast::CastInner};

pub trait BitSize: Sized {
    const BITS: usize;
}

pub trait IsPacked:
    Sized + Clone + BitAnd + BitOr + Shl<usize> + Shr<usize> + CastInner<Self> + From<usize>
{
    fn zero() -> Self;
}

pub trait BitPack: BitSize {
    type Packed: IsPacked;

    #[blackbox(BitPackPack)]
    fn pack(self) -> Self::Packed;

    fn unpack(packed: Self::Packed) -> Self;

    #[blackbox(BitPackRepack)]
    fn repack<T: BitPack<Packed = Self::Packed>>(self) -> T {
        let bitvec = self.pack();
        T::unpack(bitvec)
    }
}

pub trait BitPackExt<const N: usize>
where
    Self: BitPack<Packed = BitVec<N>>,
{
    #[blackbox(BitPackMsb)]
    fn msb(self) -> bool {
        self.pack().msb()
    }
}

impl<const N: usize, T> BitPackExt<N> for T where T: BitPack<Packed = BitVec<N>> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        array::Array,
        bit::{Bit, H, L},
        cast::Cast,
        unsigned::Unsigned,
    };

    #[test]
    fn repack() {
        let u: Unsigned<6> = 0b011011_u8.into();
        assert_eq!(
            u.clone().repack::<Array<3, Unsigned<2>>>(),
            [0b01_u8, 0b10, 0b11].cast::<Array<3, Unsigned<2>>>()
        );

        assert_eq!(
            u.repack::<Array<2, Array<1, Array<3, Bit>>>>(),
            [[[L, H, H]], [[L, H, H]]].cast::<Array<2, Array<1, Array<3, Bit>>>>()
        );
    }
}
