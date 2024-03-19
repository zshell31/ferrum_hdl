use std::fmt::{Binary, Debug, Display, LowerHex};

use fhdl_const_func::{clog2_len, max_val};
use fhdl_macros::synth;

use crate::{
    cast::{Cast, CastFrom},
    const_helpers::ConstConstr,
    signal::SignalValue,
    unsigned::U,
};

pub const fn idx_constr(n: usize) -> usize {
    assert!(n > 0);
    let len = clog2_len(n);
    assert!(len <= usize::BITS as usize);
    len
}

#[derive(Clone)]
pub struct Idx<const N: usize>(U<{ idx_constr(N) }>)
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
    #[synth(inline)]
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> SignalValue for Idx<N> where ConstConstr<{ idx_constr(N) }>: {}

#[inline(always)]
pub const fn is_power_of_two(n: usize) -> bool {
    n.is_power_of_two() && {
        let bits = idx_constr(n);
        n == max_val(bits as u128) as usize + 1
    }
}

impl<const N: usize> CastFrom<U<{ idx_constr(N) }>> for Idx<N> {
    #[synth(inline)]
    fn cast_from(val: U<{ idx_constr(N) }>) -> Self {
        if Self::IS_POWER_OF_TWO || val <= N.cast::<U<{ idx_constr(N) }>>() {
            Idx(val)
        } else {
            Idx(0_u8.cast())
        }
    }
}

impl<const N: usize> CastFrom<Idx<N>> for U<{ idx_constr(N) }>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    #[synth(inline)]
    fn cast_from(val: Idx<N>) -> Self {
        val.val()
    }
}

impl<const N: usize> CastFrom<Idx<N>> for usize
where
    ConstConstr<{ idx_constr(N) }>:,
{
    #[synth(inline)]
    fn cast_from(val: Idx<N>) -> Self {
        val.cast::<U<_>>().cast()
    }
}

impl<const N: usize> CastFrom<usize> for Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    #[synth(inline)]
    fn cast_from(val: usize) -> Self {
        val.cast::<U<_>>().cast()
    }
}

impl<const N: usize> Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    const IS_POWER_OF_TWO: bool = is_power_of_two(N);

    #[synth(inline)]
    pub fn new() -> Self {
        Self(0_u8.cast())
    }

    #[synth(inline)]
    pub(crate) unsafe fn from_usize(val: usize) -> Self {
        assert!(val < N);
        Self(val.cast())
    }

    #[synth(inline)]
    pub fn val(&self) -> U<{ idx_constr(N) }> {
        self.0.clone()
    }

    #[synth(inline)]
    pub fn succ(self) -> Self {
        if self.is_max() {
            Self(0_u8.cast())
        } else {
            Self(self.0 + 1)
        }
    }

    #[synth(inline)]
    pub fn pred(self) -> Self {
        if self.is_min() {
            Self((N - 1).cast::<U<_>>())
        } else {
            Self(self.0 - 1)
        }
    }

    #[synth(inline)]
    pub fn is_max(&self) -> bool {
        self.0 == (N - 1).cast::<U<_>>()
    }

    #[synth(inline)]
    pub fn is_min(&self) -> bool {
        self.0 == 0_u8.cast::<U<_>>()
    }

    #[synth(inline)]
    pub fn rev(&self) -> Self {
        let val = self.clone().cast::<U<_>>();
        let rev_val = N.cast::<U<_>>() - val - 1;
        Self(rev_val)
    }
}
