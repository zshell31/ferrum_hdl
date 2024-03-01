use std::marker::PhantomData;

use super::Module;

pub trait Cursor: Sized {
    type Item;
    type Storage;

    fn next(&mut self, storage: &Self::Storage) -> Option<Self::Item>;

    fn size(&self) -> usize {
        0
    }

    fn into_iter(self, storage: &Self::Storage) -> CursorIter<'_, Self> {
        CursorIter {
            storage,
            cursor: self,
        }
    }
}

pub trait CursorMut: Sized {
    type Item;

    fn next(&mut self, module: &mut Module) -> Option<Self::Item>;

    fn size(&self) -> usize {
        0
    }
}

impl<I: Iterator> Cursor for I {
    type Item = I::Item;
    type Storage = Module;

    #[inline]
    fn next(&mut self, _: &Module) -> Option<Self::Item> {
        Iterator::next(self)
    }

    fn size(&self) -> usize {
        self.size_hint().0
    }
}

impl<I: Iterator> CursorMut for I {
    type Item = I::Item;

    #[inline]
    fn next(&mut self, _: &mut Module) -> Option<Self::Item> {
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

    fn next(&mut self, module: &mut Module) -> Option<Self::Item> {
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
        self.cursor.next(self.storage)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.cursor.size(), None)
    }
}
