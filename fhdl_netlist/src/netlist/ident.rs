use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::{self, Debug, Display},
    hash::Hash,
    num::NonZeroU32,
    ops::{Deref, DerefMut},
};

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

macro_rules! idx_ty {
    ($name:ident) => {
        idx_ty!($name, false);

        impl Debug for $name {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(&self.to_string())
                    .finish()
            }
        }

        impl Display for $name {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if !self.is_empty() {
                    Display::fmt(&self.as_u32(), f)
                } else {
                    f.write_str("_")
                }
            }
        }
    };
    ($name:ident, $skip:literal) => {
        #[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        pub struct $name(Option<NonZeroU32>);

        impl IndexType for $name {
            const EMPTY: Self = Self(None);

            #[inline]
            fn new(idx: u32) -> Self {
                Self(Some(unsafe { NonZeroU32::new_unchecked(idx + 1) }))
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
                match Self::try_from_usize(idx) {
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

idx_ty!(ModuleId);
idx_ty!(NodeId);
idx_ty!(EdgeId);
idx_ty!(Symbol, true);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Port {
    pub node: NodeId,
    pub port: u32,
}

impl Display for Port {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.node.as_u32(), self.port)
    }
}

impl Debug for Port {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Port {
    pub fn new(node: NodeId, port: u32) -> Self {
        Self { node, port }
    }

    pub fn with(&self, node: NodeId) -> Self {
        Self {
            node,
            port: self.port,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PortPos {
    Input(usize),
    Output(usize),
}

#[derive(Debug, Clone, Copy)]
pub struct WithId<I, T> {
    pub id: I,
    pub inner: T,
}

impl<I: PartialEq, T> PartialEq for WithId<I, T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<I: Eq, T> Eq for WithId<I, T> {}

impl<I, T> WithId<I, T> {
    pub fn new(id: I, inner: T) -> Self {
        Self { id, inner }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> WithId<I, U>
    where
        I: Copy,
    {
        WithId {
            id: self.id,
            inner: (f)(self.inner),
        }
    }

    pub fn with<U>(&self, inner: U) -> WithId<I, U>
    where
        I: Copy,
    {
        WithId { id: self.id, inner }
    }

    pub fn as_ref(&self) -> WithId<I, &T>
    where
        I: Copy,
    {
        WithId {
            id: self.id,
            inner: &self.inner,
        }
    }

    pub fn as_mut(&mut self) -> WithId<I, &mut T>
    where
        I: Copy,
    {
        WithId {
            id: self.id,
            inner: &mut self.inner,
        }
    }

    pub fn as_deref(&self) -> WithId<I, &T::Target>
    where
        I: Copy,
        T: Deref,
    {
        WithId {
            id: self.id,
            inner: self.inner.deref(),
        }
    }

    pub fn as_deref_mut(&mut self) -> WithId<I, &mut T::Target>
    where
        I: Copy,
        T: DerefMut,
    {
        WithId {
            id: self.id,
            inner: self.inner.deref_mut(),
        }
    }
}

impl<I: Copy, T> WithId<I, &RefCell<T>> {
    pub fn borrow(&self) -> WithId<I, Ref<'_, T>> {
        self.map(|inner| inner.borrow())
    }

    pub fn borrow_mut(&self) -> WithId<I, RefMut<'_, T>> {
        self.map(|inner| inner.borrow_mut())
    }
}

impl<I: Copy, T> WithId<I, Option<T>> {
    pub fn as_opt(self) -> Option<WithId<I, T>> {
        let id = self.id;
        self.inner.map(|inner| WithId { id, inner })
    }
}

impl<I, T> Deref for WithId<I, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<I, T> DerefMut for WithId<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
