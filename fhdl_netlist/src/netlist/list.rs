use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use derive_where::derive_where;

use super::{Cursor, IndexType};

pub trait ListItem<I: IndexType, D = ()> {
    fn next(&self) -> I;

    fn set_next(&mut self, next: I);

    fn prev(&self) -> I;

    fn set_prev(&mut self, prev: I);
}

pub trait ListStorage<D = ()>:
    Index<Self::Idx, Output = Self::Item> + IndexMut<Self::Idx>
{
    const EMPTY: Self::Idx = <Self::Idx as IndexType>::EMPTY;

    type Idx: IndexType;
    type Item: ListItem<Self::Idx, D>;

    #[inline]
    fn link(&mut self, prev: Self::Idx, next: Self::Idx) {
        self[prev].set_next(next);
        self[next].set_prev(prev);
    }
}

#[derive_where(Debug, Clone, Copy)]
pub struct List<S: ListStorage<D>, D = ()> {
    pub head: S::Idx,
    pub tail: S::Idx,
    _dir: PhantomData<D>,
}

impl<S: ListStorage<D>, D> Default for List<S, D> {
    fn default() -> Self {
        Self {
            head: S::EMPTY,
            tail: S::EMPTY,
            _dir: PhantomData,
        }
    }
}

#[derive_where(Clone, Copy)]
pub struct ListCursor<S: ListStorage<D>, D = ()> {
    next: S::Idx,
    _dir: PhantomData<D>,
}

impl<S: ListStorage<D>, D> ListCursor<S, D> {
    pub fn set_next(&mut self, next: S::Idx) {
        self.next = next;
    }
}

impl<S: ListStorage<D>, D> Cursor for ListCursor<S, D> {
    type Item = S::Idx;
    type Storage = S;

    fn next_(&mut self, storage: &Self::Storage) -> Option<Self::Item> {
        if !self.next.is_empty() {
            let res = self.next;
            self.next = storage[self.next].next();
            Some(res)
        } else {
            None
        }
    }
}

impl<S: ListStorage<D>, D> List<S, D> {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.head.is_empty()
    }

    pub fn cursor(&self) -> ListCursor<S, D> {
        ListCursor {
            next: self.head,
            _dir: PhantomData,
        }
    }

    pub fn add(&mut self, storage: &mut S, idx: S::Idx) {
        if !self.is_empty() {
            let tail = self.tail;
            storage.link(tail, idx);
            self.tail = idx;
        } else {
            self.head = idx;
            self.tail = idx;
        }
    }

    #[inline]
    pub fn insert(&mut self, storage: &mut S, prev_idx: S::Idx, idx: S::Idx) {
        self.insert_list(storage, prev_idx, idx, idx)
    }

    pub fn insert_list(
        &mut self,
        storage: &mut S,
        prev_idx: S::Idx,
        start: S::Idx,
        end: S::Idx,
    ) {
        if start.is_empty() || end.is_empty() {
            return;
        }

        match prev_idx.into_opt() {
            Some(prev_idx) => {
                let prev = &storage[prev_idx];

                match prev.next().into_opt() {
                    Some(next_idx) => {
                        storage.link(prev_idx, start);
                        storage.link(end, next_idx);
                    }
                    None => {
                        storage.link(prev_idx, start);
                        self.tail = end;
                    }
                }
            }
            None => {
                match self.head.into_opt() {
                    Some(head) => {
                        storage.link(end, head);
                    }
                    None => {
                        self.tail = end;
                    }
                };
                self.head = start;
            }
        }
    }

    pub fn remove(&mut self, storage: &mut S, idx: S::Idx) {
        let item = &mut storage[idx];
        let prev_idx = item.prev();
        let next_idx = item.next();

        item.set_prev(S::EMPTY);
        item.set_next(S::EMPTY);

        match (prev_idx.into_opt(), next_idx.into_opt()) {
            (Some(prev_idx), Some(next_idx)) => {
                storage.link(prev_idx, next_idx);
            }
            (Some(prev_idx), None) => {
                // tail
                self.tail = prev_idx;
                storage[prev_idx].set_next(S::EMPTY);
            }
            (None, Some(next_idx)) => {
                // head
                self.head = next_idx;
                storage[next_idx].set_prev(S::EMPTY);
            }
            (None, None) => {
                self.head = S::EMPTY;
                self.tail = S::EMPTY;
            }
        };
    }

    pub fn replace(&mut self, storage: &mut S, old_idx: S::Idx, new_idx: S::Idx) {
        let prev_idx = storage[old_idx].prev();
        self.remove(storage, old_idx);
        self.insert(storage, prev_idx, new_idx);
    }

    #[allow(dead_code)]
    pub(super) fn dump(&self, storage: &S) {
        println!("{}", self.dump_to_str(storage))
    }

    pub(super) fn dump_to_str(&self, storage: &S) -> String {
        format!(
            "[{}]",
            self.cursor()
                .into_iter(storage)
                .map(|idx| idx.as_usize().to_string())
                .intersperse(", ".to_string())
                .collect::<String>()
        )
    }
}
