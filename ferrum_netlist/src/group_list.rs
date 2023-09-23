use std::{
    iter::{self, Once},
    slice::Iter,
};

use either::Either;
use smallvec::SmallVec;

use crate::{arena::with_arena, net_list::NodeId, sig_ty::SignalTy, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemId {
    Node(NodeId),
    Group(Group),
}

impl IntoIterator for ItemId {
    type Item = NodeId;
    type IntoIter = Either<Once<NodeId>, GroupIter>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Node(node_id) => Either::Left(iter::once(node_id)),
            Self::Group(group) => Either::Right(group.into_iter()),
        }
    }
}

pub struct ItemIdIter(Either<Once<NodeId>, GroupIter>);

impl ItemId {
    pub fn node_id(self) -> NodeId {
        match self {
            Self::Node(node_id) => node_id,
            _ => panic!("expected node_id"),
        }
    }

    pub fn group(self) -> Group {
        match self {
            Self::Group(group) => group,
            _ => panic!("expected group"),
        }
    }
}

impl From<NodeId> for ItemId {
    fn from(node_id: NodeId) -> Self {
        Self::Node(node_id)
    }
}

impl From<Group> for ItemId {
    fn from(group: Group) -> Self {
        Self::Group(group)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Named<T> {
    pub inner: T,
    pub name: Option<Symbol>,
}

impl<T> Named<T> {
    pub fn new(inner: T, name: Option<Symbol>) -> Self {
        Self { inner, name }
    }

    pub fn is(&self, s: &str) -> bool {
        match &self.name {
            Some(name) => name.as_str() == s,
            None => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Group {
    pub sig_ty: SignalTy,
    item_ids: &'static [Named<ItemId>],
}

impl !Sync for Group {}
impl !Send for Group {}

impl Group {
    pub fn new(sig_ty: SignalTy, iter: impl IntoIterator<Item = Named<ItemId>>) -> Self {
        Self {
            sig_ty,
            item_ids: unsafe { with_arena().alloc_from_iter(iter) },
        }
    }

    pub fn new_with_item_ids(
        sig_ty: SignalTy,
        item_ids: &'static [Named<ItemId>],
    ) -> Self {
        Self { sig_ty, item_ids }
    }

    pub fn by_field(&self, field: &str) -> Option<ItemId> {
        self.item_ids
            .iter()
            .find(|named| named.is(field))
            .map(|named| named.inner)
    }

    pub fn item_ids(&self) -> &'static [Named<ItemId>] {
        self.item_ids
    }

    pub fn width(&self) -> u128 {
        self.sig_ty.width()
    }
}

impl<'a> IntoIterator for &'a Group {
    type Item = NodeId;

    type IntoIter = GroupIter;

    fn into_iter(self) -> Self::IntoIter {
        GroupIter::new(self)
    }
}

pub struct GroupIter {
    stack: SmallVec<[Iter<'static, Named<ItemId>>; 8]>,
}

impl GroupIter {
    fn new(group: &Group) -> Self {
        let mut stack = SmallVec::new();
        stack.push(group.item_ids.iter());
        Self { stack }
    }
}

impl Iterator for GroupIter {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let last = self.stack.last_mut();
        match last {
            Some(last) => match last.next() {
                Some(res) => match res.inner {
                    ItemId::Node(node_id) => Some(node_id),
                    ItemId::Group(group) => {
                        let iter = group.item_ids.iter();
                        self.stack.push(iter);
                        self.next()
                    }
                },
                None => {
                    self.stack.pop();
                    self.next()
                }
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem;

    use super::*;

    #[test]
    fn memsize() {
        println!("{}", mem::size_of::<ItemId>());
        println!("{}", mem::size_of::<Group>());
    }
}
