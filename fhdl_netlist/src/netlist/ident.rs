use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
    ops::{Deref, DerefMut},
};

pub trait IndexType: Debug + Copy + Eq + Hash {
    const EMPTY: Self;

    fn new(idx: u32) -> Self;

    #[inline]
    fn from_usize(idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);
        Self::new(idx as u32)
    }

    fn as_u32(&self) -> u32;

    #[inline]
    fn as_usize(&self) -> usize {
        self.as_u32() as usize
    }

    #[inline]
    fn is_empty(&self) -> bool {
        *self == Self::EMPTY
    }

    #[inline]
    fn into_opt(self) -> Option<Self> {
        if !self.is_empty() {
            Some(self)
        } else {
            None
        }
    }
}

macro_rules! idx_ty {
    ($name:ident) => {
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        pub struct $name(u32);

        impl Display for $name {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if self.is_empty() {
                    f.write_str("_")
                } else {
                    Display::fmt(&self.0, f)
                }
            }
        }

        impl IndexType for $name {
            const EMPTY: Self = Self(u32::MAX);
            #[inline]
            fn new(idx: u32) -> Self {
                Self(idx)
            }

            #[inline]
            fn as_u32(&self) -> u32 {
                self.0
            }
        }
    };
}

idx_ty!(ModuleId);
idx_ty!(NodeId);
idx_ty!(EdgeId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Port {
    pub node: NodeId,
    pub port: u32,
}

impl Display for Port {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.node.as_u32(), self.port)
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
