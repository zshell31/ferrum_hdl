#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(register_tool)]
#![register_tool(ferrum_tool)]

pub mod array;
pub mod bit;
pub mod bit_pack;
pub mod bit_vec;
pub mod blackbox;
pub mod const_asserts;
pub mod const_functions;
pub mod domain;
pub mod prim_ty;
pub mod signal;
pub mod traits;
pub mod unsigned;
