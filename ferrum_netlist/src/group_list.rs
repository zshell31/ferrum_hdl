use std::ops::{Index, IndexMut};

use crate::{arena::with_arena, net_list::NodeId, sig_ty::SignalTy, symbol::Symbol};

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
    Group,
    Array,
}

impl From<SignalTy> for GroupKind {
    fn from(sig_ty: SignalTy) -> Self {
        match sig_ty {
            SignalTy::Group(_) => Self::Group,
            SignalTy::Array(..) => Self::Array,
            SignalTy::Prim(_) => panic!("expected non-primtive type"),
        }
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

#[derive(Debug, Clone, Copy)]
pub struct Group {
    pub kind: GroupKind,
    pub item_ids: &'static [Named<ItemId>],
}

impl !Sync for Group {}
impl !Send for Group {}

impl Group {
    pub fn new(kind: GroupKind, iter: impl IntoIterator<Item = Named<ItemId>>) -> Self {
        Self {
            kind,
            item_ids: unsafe { with_arena().alloc_from_iter(iter) },
        }
    }

    pub fn new_with_item_ids(
        kind: GroupKind,
        item_ids: &'static [Named<ItemId>],
    ) -> Self {
        Self { kind, item_ids }
    }

    pub fn by_field(&self, field: &str) -> Option<ItemId> {
        self.item_ids
            .iter()
            .find(|named| named.is(field))
            .map(|named| named.inner)
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
                    .map(|named| self.len_inner(named.inner))
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

                for named in group.item_ids.iter() {
                    self.deep_iter_inner(named.inner, ind, &mut *f)?;
                }
            }
        }

        Ok(())
    }
}
