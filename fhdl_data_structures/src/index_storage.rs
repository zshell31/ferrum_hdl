use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use indexmap::map::Iter;

use crate::{index::IndexType, FxIndexMap};

#[derive(Debug, Clone)]
pub struct IndexStorage<I: IndexType, T> {
    pub raw: FxIndexMap<I, T>,
    pub last_idx: u32,
    _idx: PhantomData<I>,
}

impl<I: IndexType, T> IndexStorage<I, T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            raw: Default::default(),
            last_idx: 0,
            _idx: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            raw: FxIndexMap::with_capacity_and_hasher(capacity, Default::default()),
            last_idx: 0,
            _idx: PhantomData,
        }
    }

    pub fn last_idx(&self) -> I {
        I::new(self.last_idx)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn push(&mut self, value: T) -> I {
        let idx = I::new(self.last_idx);
        self.last_idx += 1;

        self.raw.insert(idx, value);
        idx
    }

    pub fn insert(&mut self, idx: I, value: T) {
        if self.last_idx <= idx.as_u32() {
            self.last_idx = idx.as_u32() + 1;
        }
        self.raw.insert(idx, value);
    }

    pub fn iter_with_id(&self) -> impl DoubleEndedIterator<Item = (I, &T)> + '_ {
        self.raw.iter().map(|(id, inner)| (*id, inner))
    }
}

impl<I: IndexType, T> Deref for IndexStorage<I, T> {
    type Target = FxIndexMap<I, T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

impl<I: IndexType, T> DerefMut for IndexStorage<I, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.raw
    }
}

impl<I: IndexType, T> Default for IndexStorage<I, T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<I: IndexType, T> Index<I> for IndexStorage<I, T> {
    type Output = T;

    #[inline]
    fn index(&self, idx: I) -> &Self::Output {
        &self.raw[&idx]
    }
}

impl<I: IndexType, T> IndexMut<I> for IndexStorage<I, T> {
    #[inline]
    fn index_mut(&mut self, idx: I) -> &mut Self::Output {
        &mut self.raw[&idx]
    }
}

impl<'a, I: IndexType, T> IntoIterator for &'a IndexStorage<I, T> {
    type Item = (&'a I, &'a T);
    type IntoIter = Iter<'a, I, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        (&self.raw).into_iter()
    }
}
