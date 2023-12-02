use std::{
    iter::{self, Once},
    slice::Iter,
};

use either::Either;
use smallvec::SmallVec;

use crate::{
    arena::with_arena,
    net_list::{NodeId, NodeOutId},
    sig_ty::SignalTy,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemId {
    Node(NodeOutId),
    Group(&'static Group),
}

impl IntoIterator for ItemId {
    type Item = NodeOutId;
    type IntoIter = Either<Once<NodeOutId>, GroupIter>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Node(node_out_id) => Either::Left(iter::once(node_out_id)),
            Self::Group(group) => Either::Right(group.into_iter()),
        }
    }
}

pub struct ItemIdIter(Either<Once<NodeId>, GroupIter>);

impl ItemId {
    pub fn node_out_id(self) -> NodeOutId {
        match self {
            Self::Node(node_out_id) => node_out_id,
            _ => panic!("expected node_id"),
        }
    }

    pub fn group(self) -> &'static Group {
        match self {
            Self::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Node(_) => 1,
            Self::Group(group) => group.len,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl From<NodeOutId> for ItemId {
    fn from(node_out_id: NodeOutId) -> Self {
        Self::Node(node_out_id)
    }
}

impl From<Group> for ItemId {
    fn from(group: Group) -> Self {
        Self::Group(unsafe { with_arena().alloc(group) })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Group {
    pub sig_ty: SignalTy,
    pub len: usize,
    item_ids: &'static [ItemId],
}

impl !Sync for Group {}
impl !Send for Group {}

impl Group {
    pub fn new(sig_ty: SignalTy, iter: impl IntoIterator<Item = ItemId>) -> Self {
        let item_ids = unsafe { with_arena().alloc_from_iter(iter) };
        let len = item_ids.iter().map(|item_id| item_id.len()).sum();

        Self {
            sig_ty,
            len,
            item_ids,
        }
    }

    pub fn new_with_item_ids(sig_ty: SignalTy, item_ids: &'static [ItemId]) -> Self {
        let len = item_ids.iter().map(|item_id| item_id.len()).sum();

        Self {
            sig_ty,
            len,
            item_ids,
        }
    }

    pub fn by_field(&self, field: &str) -> Option<ItemId> {
        let struct_ty = self.sig_ty.struct_ty();
        struct_ty
            .get_idx_by_field(field)
            .map(|idx| self.item_ids[idx])
    }

    pub fn item_ids(&self) -> &'static [ItemId] {
        self.item_ids
    }

    pub fn width(&self) -> u128 {
        self.sig_ty.width()
    }
}

impl<'a> IntoIterator for &'a Group {
    type Item = NodeOutId;

    type IntoIter = GroupIter;

    fn into_iter(self) -> Self::IntoIter {
        GroupIter::new(self)
    }
}

pub struct GroupIter {
    len: usize,
    stack: SmallVec<[Iter<'static, ItemId>; 8]>,
}

impl GroupIter {
    fn new(group: &Group) -> Self {
        let mut stack = SmallVec::new();
        stack.push(group.item_ids.iter());
        Self {
            len: group.len,
            stack,
        }
    }
}

impl Iterator for GroupIter {
    type Item = NodeOutId;

    fn next(&mut self) -> Option<Self::Item> {
        let last = self.stack.last_mut();
        match last {
            Some(last) => match last.next() {
                Some(res) => match res {
                    ItemId::Node(node_out_id) => Some(*node_out_id),
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

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}
