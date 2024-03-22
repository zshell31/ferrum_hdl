use std::{cmp, marker::PhantomData};

use super::Module;

pub trait Cursor: Sized {
    type Item;
    type Storage;

    fn next_(&mut self, storage: &Self::Storage) -> Option<Self::Item>;

    fn size(&self) -> usize {
        0
    }

    fn into_iter(self, storage: &Self::Storage) -> CursorIter<'_, Self> {
        CursorIter {
            storage,
            cursor: self,
        }
    }

    fn enumerate_(self) -> Enumerate<Self> {
        Enumerate {
            idx: 0,
            inner: self,
        }
    }
}

pub trait CursorMut: Sized {
    type Item;
    type Storage;

    fn next_mut(&mut self, storage: &mut Self::Storage) -> Option<Self::Item>;

    fn size(&self) -> usize {
        0
    }

    fn into_iter_mut(self, storage: &mut Self::Storage) -> CursorIterMut<'_, Self> {
        CursorIterMut {
            storage,
            cursor: self,
        }
    }

    fn peekable_(self) -> Peekable<Self> {
        Peekable {
            peeked: None,
            inner: self,
        }
    }
}

impl<I: Iterator> Cursor for I {
    type Item = I::Item;
    type Storage = Module;

    #[inline]
    fn next_(&mut self, _: &Module) -> Option<Self::Item> {
        Iterator::next(self)
    }

    fn size(&self) -> usize {
        self.size_hint().0
    }
}

impl<I: Iterator> CursorMut for I {
    type Item = I::Item;
    type Storage = Module;

    #[inline]
    fn next_mut(&mut self, _: &mut Module) -> Option<Self::Item> {
        Iterator::next(self)
    }

    fn size(&self) -> usize {
        self.size_hint().0
    }
}

pub struct IterMut<I, F, T> {
    iter: I,
    f: F,
    _item: PhantomData<T>,
}

impl<I, F, T> IterMut<I, F, T> {
    pub fn new<In>(iter: In, f: F) -> Self
    where
        In: IntoIterator<IntoIter = I>,
        F: FnMut(&mut Module, In::Item) -> T,
    {
        Self {
            iter: iter.into_iter(),
            f,
            _item: PhantomData,
        }
    }
}

impl<I, F, T> CursorMut for IterMut<I, F, T>
where
    I: Iterator,
    F: FnMut(&mut Module, I::Item) -> T,
{
    type Item = T;
    type Storage = Module;

    fn next_mut(&mut self, module: &mut Module) -> Option<Self::Item> {
        let item = self.iter.next()?;
        Some((self.f)(module, item))
    }

    fn size(&self) -> usize {
        self.iter.size_hint().0
    }
}

pub struct CursorIter<'s, C: Cursor> {
    storage: &'s C::Storage,
    cursor: C,
}

impl<'s, C: Cursor> Iterator for CursorIter<'s, C> {
    type Item = C::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next_(self.storage)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.cursor.size(), None)
    }
}

pub struct CursorIterMut<'s, C: CursorMut> {
    storage: &'s mut C::Storage,
    cursor: C,
}

impl<'s, C: CursorMut> Iterator for CursorIterMut<'s, C> {
    type Item = C::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next_mut(self.storage)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.cursor.size(), None)
    }
}

pub struct Enumerate<C> {
    idx: usize,
    inner: C,
}

impl<C: Cursor> Cursor for Enumerate<C> {
    type Item = (usize, C::Item);
    type Storage = C::Storage;

    fn next_(&mut self, storage: &Self::Storage) -> Option<Self::Item> {
        let idx = self.idx;
        self.idx += 1;
        self.inner.next_(storage).map(|item| (idx, item))
    }

    #[inline]
    fn size(&self) -> usize {
        self.inner.size()
    }
}

pub struct Peekable<C: CursorMut> {
    peeked: Option<Option<C::Item>>,
    inner: C,
}

impl<C: CursorMut> Peekable<C> {
    pub fn peek(&mut self, storage: &mut C::Storage) -> Option<&C::Item> {
        let inner = &mut self.inner;
        self.peeked
            .get_or_insert_with(|| inner.next_mut(storage))
            .as_ref()
    }
}

impl<C: CursorMut> CursorMut for Peekable<C> {
    type Item = C::Item;
    type Storage = C::Storage;

    fn next_mut(&mut self, storage: &mut Self::Storage) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(item) => item,
            None => self.inner.next_mut(storage),
        }
    }

    #[inline]
    fn size(&self) -> usize {
        self.inner.size()
    }
}

pub struct Chunks<C> {
    inner: C,
    total: usize,
    size: usize,
    idx: usize,
}

impl<C> Chunks<C> {
    pub fn new(inner: C, total: usize, size: usize) -> Self {
        assert!(size <= total);
        Self {
            inner,
            total,
            size,
            idx: 0,
        }
    }

    pub fn next_chunk(&mut self) -> Option<Chunk<'_, C>> {
        if self.idx < self.total {
            let size = cmp::max(self.total - self.idx, self.size);
            Some(Chunk {
                parent: self,
                size,
                idx: 0,
            })
        } else {
            None
        }
    }

    pub fn skip_chunk(&mut self, storage: &C::Storage) -> Option<()>
    where
        C: Cursor,
    {
        let mut chunk = self.next_chunk()?;
        while chunk.next_(storage).is_some() {}

        Some(())
    }
}

pub struct Chunk<'a, C> {
    parent: &'a mut Chunks<C>,
    size: usize,
    idx: usize,
}

impl<'a, C> Cursor for Chunk<'a, C>
where
    C: Cursor,
{
    type Item = C::Item;
    type Storage = C::Storage;

    fn next_(&mut self, storage: &Self::Storage) -> Option<Self::Item> {
        if self.idx < self.parent.size {
            self.parent.idx += 1;
            self.idx += 1;
            self.parent.inner.next_(storage)
        } else {
            None
        }
    }

    #[inline]
    fn size(&self) -> usize {
        self.size
    }
}
