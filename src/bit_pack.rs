use crate::bit::Bit;

pub trait BitPack<const N: u8> {
    fn msb(&self) -> Bit;
}
