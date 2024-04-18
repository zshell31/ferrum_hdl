use std::{fmt::Debug, hash::Hash};

pub trait IndexType: Debug + Copy + Eq + Hash {
    const EMPTY: Self;

    fn new(idx: u32) -> Self;

    fn from_usize(idx: usize) -> Self;

    fn try_from_usize(idx: usize) -> Option<Self>;

    fn as_u32(&self) -> u32;

    #[inline]
    fn as_usize(&self) -> usize {
        self.as_u32() as usize
    }

    fn is_empty(&self) -> bool;

    fn into_opt(self) -> Option<Self>;
}

#[macro_export]
macro_rules! idx_ty {
    ($name:ident) => {
        idx_ty!($name, false);

        impl ::std::fmt::Debug for $name {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                ::std::fmt::Display::fmt(self, f)
            }
        }

        impl ::std::fmt::Display for $name {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                if !$crate::index::IndexType::is_empty(self) {
                    ::std::fmt::Display::fmt(&$crate::index::IndexType::as_u32(self), f)
                } else {
                    f.write_str("_")
                }
            }
        }
    };
    ($name:ident, $skip:literal) => {
        #[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        pub struct $name(Option<::std::num::NonZeroU32>);

        impl $crate::index::IndexType for $name {
            const EMPTY: Self = Self(None);

            #[inline]
            fn new(idx: u32) -> Self {
                Self(Some(unsafe {
                    ::std::num::NonZeroU32::new_unchecked(idx + 1)
                }))
            }

            #[inline]
            fn as_u32(&self) -> u32 {
                match self.0 {
                    Some(val) => val.get() - 1,
                    None => panic!("{} is empty", stringify!($name)),
                }
            }

            #[inline]
            fn from_usize(idx: usize) -> Self {
                match $crate::index::IndexType::try_from_usize(idx) {
                    Some(this) => this,
                    None => panic!("{} reached u32::MAX limit", stringify!($name)),
                }
            }

            #[inline]
            fn try_from_usize(idx: usize) -> Option<Self> {
                if idx < u32::MAX as usize {
                    Some(Self::new(idx as u32))
                } else {
                    None
                }
            }

            #[inline]
            fn is_empty(&self) -> bool {
                self.0.is_none()
            }

            #[inline]
            fn into_opt(self) -> Option<Self> {
                self.0.map(|_| self)
            }
        }
    };
}
