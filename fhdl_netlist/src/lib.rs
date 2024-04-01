#![deny(unused_must_use)]
#![feature(iter_intersperse)]
#![feature(type_alias_impl_trait)]
#![feature(pattern)]

use std::hash::BuildHasherDefault;

use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHasher;

pub mod buffer;
pub mod cfg;
pub mod const_val;
pub mod cursor;
pub mod netlist;
pub mod node;
pub mod node_ty;
pub mod symbol;
pub mod utils;
pub mod visitor;

pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;
