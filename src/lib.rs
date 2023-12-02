#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]
#![feature(cell_update)]
#![feature(min_specialization)]
#![feature(register_tool)]
#![register_tool(fhdl_tool)]

pub mod array;
pub mod bit;
pub mod bitpack;
pub mod bitvec;
pub mod cast;
pub mod const_functions;
pub mod const_helpers;
pub mod domain;
pub mod signal;
pub mod signal_fn;
pub mod simulation;
mod tuples;
pub mod unsigned;
