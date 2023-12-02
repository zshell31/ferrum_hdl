use derive_where::derive_where;
use rustc_macros::{Decodable, Encodable};

use crate::net_list::Idx;

pub(crate) trait ListItem<I: Idx> {
    fn idx(&self) -> I;

    fn next(&self) -> Option<I>;

    fn set_next(&mut self, next: Option<I>);

    fn prev(&self) -> Option<I>;

    fn set_prev(&mut self, prev: Option<I>);
}

pub(crate) trait ListStorage<I: Idx> {
    type Item: ListItem<I>;

    fn item(&self, idx: I) -> &Self::Item;

    fn item_mut(&mut self, idx: I) -> &mut Self::Item;

    fn link(&mut self, prev: I, next: I) {
        self.set_next(prev, Some(next));
        self.set_prev(next, Some(prev));
    }

    fn set_next(&mut self, idx: I, next: Option<I>) {
        let item = self.item_mut(idx);
        item.set_next(next);
    }

    fn set_prev(&mut self, idx: I, prev: Option<I>) {
        let item = self.item_mut(idx);
        item.set_prev(prev);
    }
}

#[derive_where(Debug, Default)]
#[derive(Encodable, Decodable)]
pub(crate) struct List<I: Idx> {
    head: Option<I>,
    tail: Option<I>,
    last_idx: usize,
}

impl<I: Idx> List<I> {
    pub(crate) fn head(&self) -> Option<I> {
        self.head
    }

    pub(crate) fn tail(&self) -> Option<I> {
        self.tail
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.head().is_none()
    }

    pub(crate) fn next_idx(&mut self) -> I {
        let idx = I::new(self.last_idx);
        self.last_idx += 1;
        idx
    }

    pub(crate) fn last_idx(&self) -> usize {
        self.last_idx
    }

    pub(crate) fn shift_last_idx(&mut self, idx: usize) {
        assert!(self.last_idx <= idx);
        self.last_idx = idx;
    }

    pub(crate) fn add<S: ListStorage<I>>(&mut self, storage: &mut S, idx: I) {
        if !self.is_empty() {
            let tail = self.tail.unwrap();
            self.tail = Some(idx);
            storage.link(tail, idx);
        } else {
            self.head = Some(idx);
            self.tail = Some(idx);
        }
    }

    pub(crate) fn insert_head<S: ListStorage<I>>(&mut self, storage: &mut S, idx: I) {
        if !self.is_empty() {
            let head = self.head.unwrap();
            self.head = Some(idx);
            storage.link(idx, head);
        } else {
            self.head = Some(idx);
            self.tail = Some(idx);
        }
    }

    pub(crate) fn insert<S: ListStorage<I>>(
        &mut self,
        storage: &mut S,
        prev_idx: Option<I>,
        idx: I,
    ) {
        match prev_idx {
            Some(prev_idx) => {
                let prev = storage.item(prev_idx);
                match prev.next() {
                    Some(next_idx) => {
                        storage.link(prev_idx, idx);
                        storage.link(idx, next_idx);
                    }
                    None => {
                        assert_eq!(self.tail.unwrap(), prev_idx);
                        self.add(storage, idx);
                    }
                }
            }
            None => {
                self.insert_head(storage, idx);
            }
        }
    }

    pub(crate) fn replace<S: ListStorage<I>>(
        &mut self,
        storage: &mut S,
        idx: I,
        item: &mut S::Item,
    ) {
        assert_eq!(item.idx(), idx);
        let old_item = storage.item(idx);
        let prev_idx = old_item.prev();
        let next_idx = old_item.next();

        item.set_prev(prev_idx);
        item.set_next(next_idx);
    }

    pub(crate) fn remove<S: ListStorage<I>>(&mut self, storage: &mut S, idx: I) {
        let item = storage.item(idx);
        let prev_idx = item.prev();
        let next_idx = item.next();

        storage.set_prev(idx, None);
        storage.set_next(idx, None);

        match (prev_idx, next_idx) {
            (Some(prev_idx), Some(next_idx)) => {
                storage.link(prev_idx, next_idx);
            }
            (Some(prev_idx), None) => {
                // tail
                assert_eq!(self.tail.unwrap(), idx);
                self.tail = Some(prev_idx);
                storage.set_next(prev_idx, None);
            }
            (None, Some(next_idx)) => {
                // head
                assert_eq!(self.head.unwrap(), idx);
                self.head = Some(next_idx);
                storage.set_prev(next_idx, None);
            }
            (None, None) => {
                assert_eq!(self.tail.unwrap(), idx);
                assert_eq!(self.head.unwrap(), idx);
                self.head = None;
                self.tail = None;
            }
        };
    }
}
