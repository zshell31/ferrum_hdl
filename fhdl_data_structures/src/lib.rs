#![feature(iter_intersperse)]
use std::hash::BuildHasherDefault;

use indexmap::{IndexMap, IndexSet};

pub mod cursor;
pub mod graph;
pub mod index;
pub mod index_storage;
pub mod list;
pub mod tree;

pub type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;
pub use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
