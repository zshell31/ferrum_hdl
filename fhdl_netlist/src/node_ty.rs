use std::{
    borrow::Cow,
    cmp,
    fmt::{self, Debug, Display},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeTy {
    Bit,
    Unsigned(u128),
    Signed(u128),
    Clock,
    ClockDomain,
}

impl Display for NodeTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Cow<'_, str> = match self {
            Self::Bit => "bit".into(),
            Self::Unsigned(n) => format!("unsigned[{n}]").into(),
            Self::Signed(n) => format!("signed[{n}]").into(),
            Self::Clock => "clock".into(),
            Self::ClockDomain => "clock_domain".into(),
        };

        f.write_str(s.as_ref())
    }
}

impl Debug for NodeTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl NodeTy {
    /// Just alias for Unsigned
    #[allow(non_snake_case)]
    #[inline]
    pub fn BitVec(width: u128) -> Self {
        Self::Unsigned(width)
    }

    pub fn is_bit(&self) -> bool {
        matches!(self, NodeTy::Bit)
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(self, Self::Unsigned(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, Self::Signed(_))
    }

    pub fn width(&self) -> u128 {
        match self {
            Self::Bit => 1,
            Self::Unsigned(n) => *n,
            Self::Signed(n) => *n,
            Self::Clock => 1,
            Self::ClockDomain => 1,
        }
    }

    pub fn is_zero_sized(&self) -> bool {
        match self {
            Self::Bit => false,
            Self::Unsigned(n) => *n == 0,
            Self::Signed(n) => *n == 0,
            Self::Clock | Self::ClockDomain => true,
        }
    }

    pub fn ty_for_bin_expr(lhs: NodeTy, rhs: NodeTy) -> Option<NodeTy> {
        use NodeTy::*;

        if lhs == rhs {
            return Some(lhs);
        }

        match (lhs, rhs) {
            (Bit, Bit) => Some(Bit),
            (Unsigned(n), Unsigned(m)) => Some(Unsigned(cmp::max(n, m))),
            _ => {
                println!("ty_for_bin_expr: lhs = {lhs} rhs = {rhs}");
                None
            }
        }
    }
}
