use std::ops::{Index, IndexMut};

use crate::{arena::with_arena, net_list::NodeId, sig_ty::SignalTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GroupId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ItemId {
    Node(NodeId),
    Group(GroupId),
}

impl ItemId {
    pub fn node_id(&self) -> NodeId {
        match self {
            Self::Node(node_id) => *node_id,
            Self::Group(_) => panic!("expected node_id ({:?})", self),
        }
    }

    pub fn group_id(&self) -> GroupId {
        match self {
            Self::Group(group_id) => *group_id,
            Self::Node(_) => panic!("expected group_id ({:?})", self),
        }
    }
}

impl From<NodeId> for ItemId {
    fn from(node_id: NodeId) -> Self {
        Self::Node(node_id)
    }
}

impl From<GroupId> for ItemId {
    fn from(group_id: GroupId) -> Self {
        Self::Group(group_id)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GroupKind {
    Prim,
    Group,
    Array,
}

impl From<SignalTy> for GroupKind {
    fn from(sig_ty: SignalTy) -> Self {
        match sig_ty {
            SignalTy::Prim(_) => Self::Prim,
            SignalTy::Group(_) => Self::Group,
            SignalTy::Array(..) => Self::Array,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Group {
    pub kind: GroupKind,
    pub item_ids: &'static [ItemId],
}

impl !Sync for Group {}
impl !Send for Group {}

impl Group {
    pub fn new(kind: GroupKind, iter: impl IntoIterator<Item = ItemId>) -> Self {
        Self {
            kind,
            item_ids: unsafe { with_arena().alloc_from_iter(iter) },
        }
    }

    pub fn new_with_item_ids(kind: GroupKind, item_ids: &'static [ItemId]) -> Self {
        Self { kind, item_ids }
    }

    pub fn by_field(&self, field: &str) -> Option<ItemId> {
        match field.parse::<usize>() {
            Ok(ind) => self.item_ids.get(ind).copied(),
            Err(_) => None,
        }
    }

    pub fn len(&self) -> usize {
        self.item_ids.len()
    }

    pub fn is_empty(&self) -> bool {
        self.item_ids.is_empty()
    }
}

#[derive(Debug)]
pub struct GroupList {
    groups: Vec<Group>,
}

impl !Sync for GroupList {}
impl !Send for GroupList {}

impl Index<GroupId> for GroupList {
    type Output = Group;

    fn index(&self, index: GroupId) -> &Self::Output {
        &self.groups[index.0]
    }
}

impl IndexMut<GroupId> for GroupList {
    fn index_mut(&mut self, index: GroupId) -> &mut Self::Output {
        &mut self.groups[index.0]
    }
}

impl Default for GroupList {
    fn default() -> Self {
        Self::new()
    }
}

impl GroupList {
    pub fn new() -> Self {
        Self {
            groups: Vec::with_capacity(16),
        }
    }

    pub fn add_group(&mut self, group: Group) -> GroupId {
        let group_id = GroupId(self.groups.len());
        self.groups.push(group);
        group_id
    }

    pub fn len(&self, item_ids: &[ItemId]) -> usize {
        item_ids
            .iter()
            .map(|&item_id| self.len_inner(item_id))
            .sum()
    }

    fn len_inner(&self, item_id: ItemId) -> usize {
        match item_id {
            ItemId::Node(_) => 1,
            ItemId::Group(group_id) => {
                let group = &self[group_id];
                group
                    .item_ids
                    .iter()
                    .map(|&item_id| self.len_inner(item_id))
                    .sum()
            }
        }
    }

    pub fn deep_iter<E, F: FnMut(usize, NodeId) -> Result<(), E>>(
        &self,
        item_ids: &[ItemId],
        f: &mut F,
    ) -> Result<(), E> {
        let mut ind = 0;

        #[allow(clippy::explicit_counter_loop)]
        for item_id in item_ids {
            self.deep_iter_inner(*item_id, &mut ind, f)?;
        }

        Ok(())
    }

    fn deep_iter_inner<E, F: FnMut(usize, NodeId) -> Result<(), E>>(
        &self,
        item_id: ItemId,
        ind: &mut usize,
        f: &mut F,
    ) -> Result<(), E> {
        match item_id {
            ItemId::Node(node_id) => {
                f(*ind, node_id)?;
                *ind += 1;
            }
            ItemId::Group(group_id) => {
                let group = &self[group_id];

                for item_id in group.item_ids.iter().copied() {
                    self.deep_iter_inner(item_id, ind, &mut *f)?;
                }
            }
        }

        Ok(())
    }

    // pub fn combine<F: FnMut(NodeId, NodeId) -> NodeId>(
    //     &mut self,
    //     item_id1: ItemId,
    //     item_id2: ItemId,
    //     f: &mut F,
    // ) -> Option<ItemId> {
    //     match (item_id1, item_id2) {
    //         (ItemId::Node(node_id1), ItemId::Node(node_id2)) => {
    //             Some(f(node_id1, node_id2).into())
    //         }
    //         (ItemId::Group(group_id1), ItemId::Group(group_id2)) => {
    //             let item_ids1 = self[group_id1].item_ids;
    //             let item_ids2 = self[group_id2].item_ids;

    //             if item_ids1.len() != item_ids2.len() {
    //                 return None;
    //             }

    //             let group = item_ids1
    //                 .iter()
    //                 .zip(item_ids2.iter())
    //                 .map(|(item_id1, item_id2)| self.combine(*item_id1, *item_id2, f))
    //                 .collect::<Option<Vec<ItemId>>>()?;

    //             Some(self.add_group(Group::new(group)).into())
    //         }
    //         _ => None,
    //     }
    // }
}
