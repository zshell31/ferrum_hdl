use std::fmt::{Binary, Debug, Display, LowerHex};

use fhdl_const_func::{clog2_len, max_val};
use fhdl_macros::synth;

use crate::{
    cast::{Cast, CastFrom},
    const_helpers::{Assert, ConstConstr, IsTrue},
    signal::SignalValue,
    unsigned::Unsigned,
};

#[inline]
pub const fn idx_constr(n: usize) -> usize {
    assert!(n > 0);
    clog2_len(n)
}

#[derive(Clone)]
pub struct Idx<const N: usize>(Unsigned<{ idx_constr(N) }>)
where
    ConstConstr<{ idx_constr(N) }>:;

impl<const N: usize> Debug for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<const N: usize> Display for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<const N: usize> Binary for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Binary::fmt(&self.0, f)
    }
}

impl<const N: usize> LowerHex for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        LowerHex::fmt(&self.0, f)
    }
}

impl<const N: usize> Default for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    #[fhdl_tool::synth]
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> SignalValue for Idx<N> where ConstConstr<{ idx_constr(N) }>: {}

pub const fn idx_cast_constr(n: usize) -> bool {
    n.is_power_of_two() && {
        let bits = idx_constr(n);
        n == max_val(bits as u128) as usize + 1
    }
}

impl<const N: usize> CastFrom<Unsigned<{ idx_constr(N) }>> for Idx<N>
where
    Assert<{ idx_cast_constr(N) }>: IsTrue,
{
    // FIXME: check that impl method is synth, not trait method
    #[synth]
    fn cast_from(val: Unsigned<{ idx_constr(N) }>) -> Self {
        Idx(val)
    }
}

impl<const N: usize> From<usize> for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    fn from(value: usize) -> Self {
        Self(value.cast())
    }
}

impl<const N: usize> Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    #[synth]
    pub fn new() -> Self {
        Self(0_u8.cast())
    }

    #[synth]
    #[inline]
    pub fn val(&self) -> Unsigned<{ idx_constr(N) }> {
        self.0.clone()
    }

    pub fn as_usize(&self) -> usize {
        self.val().cast()
    }

    #[synth]
    pub fn succ(self) -> Self {
        if self.is_max() {
            Self(0_u8.cast())
        } else {
            Self(self.0 + 1_u8)
        }
    }

    #[synth]
    pub fn pred(self) -> Self {
        if self.is_min() {
            Self((N - 1).cast::<Unsigned<_>>())
        } else {
            Self(self.0 - 1_u8)
        }
    }

    #[synth]
    #[inline]
    pub fn is_max(&self) -> bool {
        self.0 == (N - 1).cast::<Unsigned<_>>()
    }

    #[synth]
    #[inline]
    pub fn is_min(&self) -> bool {
        self.0 == 0_u8.cast::<Unsigned<_>>()
    }

    #[synth]
    #[inline]
    pub fn from(val: Unsigned<{ idx_constr(N) }>) -> Self
    where
        Assert<{ idx_cast_constr(N) }>: IsTrue,
    {
        Self(val)
    }
}
