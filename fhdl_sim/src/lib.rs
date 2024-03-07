#![allow(incomplete_features)]
#![feature(adt_const_params)]

#[cfg(feature = "trace")]
use std::io::{self, Write};

#[cfg(feature = "trace")]
pub use vcd::Writer as VcdWriter_;

#[derive(Debug, Clone, Copy)]
pub enum Clock<const SYM: &'static str> {
    Rising,
    Falling,
}

impl<const SYM: &'static str> Clock<SYM> {
    #[inline]
    pub fn is_rising(&self) -> bool {
        matches!(self, Self::Rising)
    }

    #[inline]
    pub fn is_falling(&self) -> bool {
        matches!(self, Self::Falling)
    }

    #[inline]
    pub fn invert(&mut self) {
        *self = match self {
            Self::Rising => Self::Falling,
            Self::Falling => Self::Rising,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Value<T, const SYM: &'static str>(pub T);

#[cfg(feature = "trace")]
pub struct VcdWriter<W: Write>(VcdWriter_<W>);

#[cfg(feature = "trace")]
impl<W: Write> VcdWriter<W> {
    pub fn new(writer: W) -> Self {
        Self(VcdWriter_::new(writer))
    }
}
