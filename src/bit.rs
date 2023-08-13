use std::{
    fmt::{self, Display},
    ops::{BitAnd, BitOr, Not},
};

use ferrum_macros::blackbox;

// use rustc_ast::LitKind;
// use rustc_hir::Lit;
use crate::prim_ty::{IsPrimTy, PrimTy, PrimValue};
use crate::signal::SignalValue;
// use crate::traits::Synthesizable;

#[blackbox(Bit)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bit(bool);

impl IsPrimTy for Bit {
    fn prim_ty() -> PrimTy {
        PrimTy::Bit
    }

    fn width() -> u8 {
        1
    }
}

impl PrimValue for Bit {
    fn value(self) -> u128 {
        match self.0 {
            false => 0,
            true => 1,
        }
    }
}

// impl Synthesizable for Bit {
//     fn width() -> u128 {
//         1
//     }

//     fn synthesize_lit(lit: &Lit) -> Option<Expression> {
//         match lit.node {
//             LitKind::Bool(b) => Self::from(b).synthesize(),
//             _ => None,
//         }
//     }
// }

impl Display for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SignalValue for Bit {}

impl Bit {
    #[blackbox(Bit(expr = 1))]
    pub const HIGH: Bit = Bit(true);
    #[blackbox(Bit(expr = 0))]
    pub const LOW: Bit = Bit(false);

    // fn synthesize(self) -> Option<Expression> {
    //     match self.0 {
    //         false => Some(Expression::Literal(Literal { val: 0, width: 1 })),
    //         true => Some(Expression::Literal(Literal { val: 1, width: 1 })),
    //     }
    // }
}

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

impl From<usize> for Bit {
    fn from(value: usize) -> Bit {
        (value > 0).into()
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
