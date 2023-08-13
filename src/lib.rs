#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(register_tool)]
#![register_tool(ferrum_tool)]

// pub mod ast;
pub mod bit;
pub mod bit_pack;
pub mod bit_vec;
pub mod blackbox;
pub mod const_asserts;
pub mod const_functions;
pub mod domain;
pub mod net_list;
pub mod signal;
//pub mod synthesis;
pub mod prim_ty;
pub mod traits;
pub mod unsigned;
