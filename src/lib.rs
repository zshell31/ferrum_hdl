#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(generic_arg_infer)]
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
pub mod index;
pub mod signal;
pub mod signal_fn;
pub mod simulation;
mod tuples;
pub mod unsigned;
