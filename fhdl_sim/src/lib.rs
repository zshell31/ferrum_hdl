#![allow(incomplete_features)]
#![feature(adt_const_params)]

pub struct Log(Vec<u8>);

impl Log {
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }
}

