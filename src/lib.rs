#![allow(clippy::let_and_return)]
#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(generic_arg_infer)]
#![feature(cell_update)]
#![feature(register_tool)]
#![register_tool(fhdl_tool)]

pub mod array;
pub mod bit;
pub mod bitpack;
pub mod bitvec;
pub mod bundle;
pub mod cast;
pub mod const_functions;
pub mod const_helpers;
pub mod domain;
pub mod eval;
pub mod index;
pub mod signal;
mod tuples;
pub mod unsigned;

pub mod prelude {
    pub use fhdl_macros::synth;

    pub use crate::{
        array::{Array, ArrayExt},
        bit::Bit,
        bitpack::BitPack,
        bitvec::BitVec,
        bundle::{Bundle, Unbundle},
        cast::{Cast, CastFrom},
        const_functions::clog2,
        const_helpers::{Assert, ConstConstr, IsTrue},
        domain::{
            clk_divider, hz_to_period, Clock, ClockDomain, Polarity, SyncKind,
            TestDomain, MICROSECOND, MILLISECOND, NANOSECOND, PICOSECOND, SECOND, TD16,
            TD4, TD8,
        },
        eval::Eval,
        index::{idx_constr, Idx},
        signal::{
            dff, reg, reg0, reg_en, reg_en0, Enable, IntoSignal, Reset, Signal,
            SignalValue,
        },
        unsigned::{Unsigned, U},
    };
}
