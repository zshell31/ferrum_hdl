use std::{
    cmp,
    fmt::{self, Display},
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};

use fhdl_const_func::mask;

// TODO: use long arithmetic
#[derive(Debug, Clone, Copy)]
pub struct ConstVal {
    val: u128,
    width: u128,
}

impl Display for ConstVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}'d{}", self.width, self.val())
    }
}

impl Default for ConstVal {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

impl From<ConstVal> for u128 {
    fn from(value: ConstVal) -> Self {
        value.val
    }
}

impl ConstVal {
    pub fn new(val: u128, width: u128) -> Self {
        assert!(width <= 128);

        let val = val_(val, width);
        Self { val, width }
    }

    #[inline]
    pub fn val(&self) -> u128 {
        val_(self.val, self.width)
    }

    #[inline]
    pub fn width(&self) -> u128 {
        self.width
    }

    pub fn shift(&mut self, new_val: Self) {
        let Self { val, width } = new_val;

        self.width += width;
        assert!(self.width <= 128);

        self.val <<= width;
        self.val |= val & mask(width);
    }

    #[inline]
    pub fn is_zero_sized(&self) -> bool {
        self.width == 0
    }

    pub fn sra(self, rhs: ConstVal) -> ConstVal {
        let width = op_width(&self, &rhs);
        bin_op(
            ((val_(self.val, width) as i128) >> val_(rhs.val, width)) as u128,
            self,
            rhs,
        )
    }
}

fn bin_op(val: u128, lhs: ConstVal, rhs: ConstVal) -> ConstVal {
    let width = op_width(&lhs, &rhs);
    ConstVal::new(val, width)
}

#[inline]
fn op_width(lhs: &ConstVal, rhs: &ConstVal) -> u128 {
    assert_eq!(lhs.width, rhs.width);
    lhs.width
}

fn val_(val: u128, width: u128) -> u128 {
    let mask = mask(width);
    val & mask
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
        let width = op_width(self, other);
        val_(self.val, width) == val_(other.val, width)
    }
}

impl Eq for ConstVal {}

impl Ord for ConstVal {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let width = op_width(self, other);
        val_(self.val, width).cmp(&val_(other.val, width))
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
        bin_op(
            self.val
                .checked_add(rhs.val)
                .expect("attempt to add with overflow"),
            self,
            rhs,
        )
    }
}

impl Sub for ConstVal {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        bin_op(
            self.val
                .checked_sub(rhs.val)
                .expect("attempt to subtract with overflow"),
            self,
            rhs,
        )
    }
}

impl Mul for ConstVal {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        bin_op(
            self.val
                .checked_mul(rhs.val)
                .expect("attempt to multiply with overflow"),
            self,
            rhs,
        )
    }
}

impl Div for ConstVal {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        bin_op(self.val / rhs.val, self, rhs)
    }
}

impl Rem for ConstVal {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        bin_op(self.val % rhs.val, self, rhs)
    }
}

impl Shl for ConstVal {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let width = op_width(&self, &rhs);
        bin_op(val_(self.val, width) << val_(rhs.val, width), self, rhs)
    }
}

impl Shr for ConstVal {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        let width = op_width(&self, &rhs);
        bin_op(val_(self.val, width) >> val_(rhs.val, width), self, rhs)
    }
}

impl BitAnd for ConstVal {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        bin_op(self.val & rhs.val, self, rhs)
    }
}

impl BitOr for ConstVal {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        bin_op(self.val | rhs.val, self, rhs)
    }
}

impl BitXor for ConstVal {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        bin_op(self.val ^ rhs.val, self, rhs)
    }
}
