use std::{
    cell::{Ref, RefCell, RefMut},
    ops::{Deref, DerefMut},
};

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

impl<I: Copy, T> WithId<I, &mut T> {
    pub fn reborrow(&mut self) -> WithId<I, &mut T> {
        WithId {
            id: self.id,
            inner: &mut *self.inner,
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
