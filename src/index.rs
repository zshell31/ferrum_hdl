use std::fmt::{Binary, Debug, LowerHex};

use fhdl_const_func::clog2;
use fhdl_macros::{blackbox, blackbox_ty};

use crate::{
    cast::Cast, const_helpers::ConstConstr, signal::SignalValue, unsigned::Unsigned,
};

#[inline(always)]
pub const fn idx_constr(n: usize) -> usize {
    clog2(n)
}

#[derive(Clone)]
#[blackbox_ty(Index)]
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
    #[blackbox(IdxDefault)]
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> SignalValue for Idx<N> where ConstConstr<{ idx_constr(N) }>: {}

impl<const N: usize> Idx<N>
where
    ConstConstr<{ idx_constr(N) }>:,
{
    #[blackbox(IdxDefault)]
    #[fhdl_tool::synth]
    pub fn new() -> Self {
        Self(0_u8.cast())
    }

    #[blackbox(IdxVal)]
    pub fn val(self) -> Unsigned<{ idx_constr(N) }> {
        self.0
    }

    #[blackbox(IdxSucc)]
    pub fn succ(self) -> Self {
        if self.is_max() {
            Self(0_u8.cast())
        } else {
            Self(self.0 + 1_u8)
        }
    }

    #[blackbox(IdxPred)]
    pub fn pred(self) -> Self {
        if self.is_min() {
            Self((N - 1).cast::<Unsigned<_>>())
        } else {
            Self(self.0 - 1_u8)
        }
    }

    #[blackbox(IdxIsMax)]
    pub fn is_max(&self) -> bool {
        self.0 == (N - 1).cast::<Unsigned<_>>()
    }

    #[blackbox(IdxIsMin)]
    pub fn is_min(&self) -> bool {
        self.0 == 0_u8.cast::<Unsigned<_>>()
    }
}
