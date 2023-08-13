use crate::bit::Bit;

pub trait BitPack<const N: usize> {
    fn msb(&self) -> Bit;
}
