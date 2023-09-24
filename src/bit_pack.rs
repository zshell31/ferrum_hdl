use crate::{
    bit::Bit,
    bit_vec::{msb_inner, BitVec, BitVecInner},
};

pub trait BitSize: Sized {
    const BITS: usize;
}

pub trait IsPacked {
    fn inner(&self) -> BitVecInner;

    fn msb(&self) -> Bit;
}

impl<const N: usize> IsPacked for BitVec<N> {
    fn inner(&self) -> BitVecInner {
        self.inner()
    }

    fn msb(&self) -> Bit {
        msb_inner(self.inner(), N)
    }
}

pub trait BitPack: BitSize {
    type Packed: IsPacked;

    fn pack(&self) -> Self::Packed;

    fn unpack(packed: Self::Packed) -> Self;

    fn repack<T: BitPack<Packed = Self::Packed>>(&self) -> T {
        let bitvec = self.pack();
        T::unpack(bitvec)
    }

    fn msb(&self) -> Bit {
        self.pack().msb()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        array::Array,
        bit::{H, L},
        cast::Cast,
        unsigned::Unsigned,
    };

    #[test]
    fn repack() {
        let u: Unsigned<6> = 0b011011.into();
        assert_eq!(
            u.repack::<Array<3, Unsigned<2>>>(),
            [0b01, 0b10, 0b11].cast::<Array<3, Unsigned<2>>>()
        );

        assert_eq!(
            u.repack::<Array<2, Array<1, Array<3, Bit>>>>(),
            [[[L, H, H]], [[L, H, H]]].cast::<Array<2, Array<1, Array<3, Bit>>>>()
        );
    }
}
