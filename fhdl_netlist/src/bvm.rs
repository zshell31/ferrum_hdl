use fhdl_const_func::mask;

use crate::{
    const_val::ConstVal,
    resolver::{Resolve, Resolver},
};

#[derive(Debug, Clone, Copy, Default)]
pub struct BitVecMask {
    pub val: u128,
    pub mask: u128,
}

impl BitVecMask {
    pub fn set_val(&mut self, val: u128, width: u128) {
        self.val |= val & mask(width);
    }

    pub fn set_mask(&mut self, width: u128) {
        self.mask |= mask(width);
    }

    pub fn shiftl(&mut self, width: u128) {
        self.val <<= width;
        self.mask <<= width;
    }

    pub fn shiftr(&mut self, width: u128) {
        self.val >>= width;
        self.mask >>= width;
    }

    pub fn to_bitstr(&self, width: u128, wildcard: char) -> String {
        let mut mask = 1 << (width - 1);

        (0 .. width)
            .map(|_| {
                let ch = if (self.mask & mask as u128) != 0 {
                    wildcard
                } else if (self.val & mask as u128) != 0 {
                    '1'
                } else {
                    '0'
                };

                mask >>= 1;
                ch
            })
            .collect()
    }

    pub fn is_match(&self, val: ConstVal) -> bool {
        let mask = mask(val.width);
        let current = (self.val & mask) & !self.mask;
        let val = (val.val & mask) & !self.mask;
        current == val
    }
}

impl<R: Resolver> Resolve<R> for BitVecMask {
    fn resolve(&self, _resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(*self)
    }
}
