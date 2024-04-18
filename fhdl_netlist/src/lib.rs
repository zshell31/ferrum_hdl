#![deny(unused_must_use)]
#![feature(iter_intersperse)]
#![feature(type_alias_impl_trait)]
#![feature(pattern)]

pub mod buffer;
pub mod cfg;
pub mod const_val;
pub mod netlist;
pub mod node;
pub mod node_ty;
pub mod symbol;
pub mod utils;
pub mod visitor;
pub mod with_id;
