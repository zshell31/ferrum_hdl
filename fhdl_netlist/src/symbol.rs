use std::fmt::{self, Arguments, Debug, Display};

use rustc_serialize::{Decodable, Decoder, Encodable, Encoder};

use crate::arena::with_arena;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(&'static str);

impl<E: Encoder> Encodable<E> for Symbol {
    fn encode(&self, e: &mut E) {
        self.as_str().encode(e);
    }
}

impl<D: Decoder> Decodable<D> for Symbol {
    fn decode(d: &mut D) -> Self {
        Self::new(d.read_str())
    }
}

impl !Sync for Symbol {}
impl !Send for Symbol {}

impl Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, r#""{}""#, self.as_str())
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Symbol {
    pub fn new(name: &str) -> Self {
        // TODO: check duplicates
        let s = unsafe { with_arena().alloc_str(name) };
        Self(s)
    }

    pub fn new_from_args(args: Arguments<'_>) -> Self {
        let s = unsafe { with_arena().alloc_args(args) };
        Self(s)
    }

    pub fn new_from_ind(ind: usize) -> Self {
        Self::new(&ind.to_string())
    }

    pub fn as_str(&self) -> &'static str {
        self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
