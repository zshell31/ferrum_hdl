#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]
#![feature(cell_update)]
#![feature(min_specialization)]
#![feature(register_tool)]
#![register_tool(ferrum_tool)]

pub use ferrum_macros::p;

pub mod array;
pub mod bit;
pub mod bit_pack;
pub mod bit_vec;
pub mod cast;
pub mod const_functions;
pub mod const_helpers;
pub mod domain;
pub mod signal;
pub mod signal_fn;
pub mod simulation;
pub mod unsigned;
