use std::{
    cmp,
    fmt::{self, Display},
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};

use fhdl_const_func::mask;

use crate::sig_ty::Width;

// TODO: use long arithmetic
#[derive(Debug, Clone, Copy)]
pub struct ConstVal {
    pub val: u128,
    pub width: u128,
}

impl Display for ConstVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}'d{}", self.width, self.val_(self.width))
    }
}

impl From<ConstVal> for Width {
    fn from(value: ConstVal) -> Self {
        value.val.into()
    }
}

impl ConstVal {
    pub(crate) fn new(val: u128, width: u128) -> Self {
        let mask = mask(width);
        Self {
            val: val & mask,
            width,
        }
    }

    fn bin_op(val: u128, lhs: Self, rhs: Self) -> Self {
        let width = Self::width(&lhs, &rhs);
        Self::new(val, width)
    }

    fn width(lhs: &Self, rhs: &Self) -> u128 {
        assert_eq!(lhs.width, rhs.width);
        lhs.width
    }

    fn val_(&self, width: u128) -> u128 {
        let mask = mask(width);
        self.val & mask
    }

    pub(crate) fn shift(&mut self, new_val: Self) {
        let Self { val, width } = new_val;
        self.width += width;
        self.val <<= width;
        self.val |= val & mask(width);
    }
}

impl From<bool> for ConstVal {
    fn from(value: bool) -> Self {
        if value {
            ConstVal::new(1, 1)
        } else {
            ConstVal::new(0, 1)
        }
    }
}

impl PartialEq for ConstVal {
    fn eq(&self, other: &Self) -> bool {
        let width = Self::width(self, other);
        self.val_(width) == other.val_(width)
    }
}

impl Eq for ConstVal {}

impl Ord for ConstVal {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let width = Self::width(self, other);
        self.val_(width).cmp(&other.val_(width))
    }
}

impl PartialOrd for ConstVal {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Not for ConstVal {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::new(!self.val, self.width)
    }
}

impl Add for ConstVal {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val.wrapping_add(rhs.val), self, rhs)
    }
}

impl Sub for ConstVal {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val.wrapping_sub(rhs.val), self, rhs)
    }
}

impl Mul for ConstVal {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val.wrapping_mul(rhs.val), self, rhs)
    }
}

impl Div for ConstVal {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val.wrapping_div(rhs.val), self, rhs)
    }
}

impl Rem for ConstVal {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val.wrapping_rem(rhs.val), self, rhs)
    }
}

impl Shl for ConstVal {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let width = Self::width(&self, &rhs);
        Self::bin_op(self.val_(width) << rhs.val_(width), self, rhs)
    }
}

impl Shr for ConstVal {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        let width = Self::width(&self, &rhs);
        Self::bin_op(self.val_(width) >> rhs.val_(width), self, rhs)
    }
}

impl BitAnd for ConstVal {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val & rhs.val, self, rhs)
    }
}

impl BitOr for ConstVal {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val | rhs.val, self, rhs)
    }
}

impl BitXor for ConstVal {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::bin_op(self.val ^ rhs.val, self, rhs)
    }
}
