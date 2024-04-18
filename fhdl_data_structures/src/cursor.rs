pub trait Cursor<S>: Sized {
    type Item;

    fn next_(&mut self, storage: &S) -> Option<Self::Item>;

    fn into_iter_(self, storage: &S) -> CursorIter<'_, S, Self> {
        CursorIter {
            storage,
            cursor: self,
        }
    }
}

impl<S, I: Iterator> Cursor<S> for I {
    type Item = I::Item;

    #[inline]
    fn next_(&mut self, _: &S) -> Option<Self::Item> {
        Iterator::next(self)
    }
}

pub struct CursorIter<'s, S, C: Cursor<S>> {
    storage: &'s S,
    cursor: C,
}

impl<'s, S, C: Cursor<S>> Iterator for CursorIter<'s, S, C> {
    type Item = C::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next_(self.storage)
    }
}
