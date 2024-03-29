use crate::netlist::Module;

pub trait Cursor: Sized {
    type Item;
    type Storage;

    fn next_(&mut self, storage: &Self::Storage) -> Option<Self::Item>;

    fn into_iter_(self, storage: &Self::Storage) -> CursorIter<'_, Self> {
        CursorIter {
            storage,
            cursor: self,
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
}
