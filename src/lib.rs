#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(register_tool)]
#![feature(type_alias_impl_trait)]
#![feature(const_trait_impl)]
#![feature(cell_update)]
#![feature(negative_impls)]
#![register_tool(ferrum_tool)]

pub mod array;
pub mod bit;
pub mod bit_pack;
pub mod bit_vec;
pub mod blackbox;
pub mod cast;
pub mod const_functions;
pub mod const_helpers;
pub mod domain;
pub mod signal;
pub mod signal_fn;
pub mod simulation;
pub mod unsigned;
