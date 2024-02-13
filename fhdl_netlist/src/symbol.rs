use std::{
    fmt::{self, Arguments, Debug, Display},
    str::pattern::{Pattern, Searcher},
};

use crate::arena::with_arena;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(&'static str);

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

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Symbol {
    pub fn new(name: impl AsRef<str>) -> Self {
        // TODO: check duplicates
        let s = unsafe { with_arena().alloc_str(name.as_ref()) };
        Self(s)
    }

    pub fn new_from_args(args: Arguments<'_>) -> Self {
        let s = unsafe { with_arena().alloc_args(args) };
        Self(s)
    }

    pub fn new_from_ind(ind: usize) -> Self {
        Self::new(ind.to_string())
    }

    pub fn as_str(&self) -> &'static str {
        self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    // Split string but prefix contains delimiter
    pub fn split_once_with_delim<'a, P: Pattern<'a>>(
        &'a self,
        delimiter: P,
    ) -> Option<(&'a str, &'a str)> {
        let s = self.as_str();
        let (start, _) = delimiter.into_searcher(s).next_match()?;

        unsafe { Some((s.get_unchecked(.. start), s.get_unchecked(start ..))) }
    }
}
