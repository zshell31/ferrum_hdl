use std::ops::{Index, IndexMut};

use crate::net_list::NodeId;

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

#[derive(Debug)]
pub struct Group {
    groups: Vec<ItemId>,
}

impl Group {
    pub fn new(groups: impl IntoIterator<Item = ItemId>) -> Self {
        Self {
            groups: groups.into_iter().collect(),
        }
    }

    pub fn by_field(&self, field: &str) -> Option<ItemId> {
        match field.parse::<usize>() {
            Ok(ind) => self.groups.get(ind).copied(),
            Err(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct GroupList {
    groups: Vec<Group>,
}

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

    pub fn shadow_iter<E>(
        &self,
        group_id: GroupId,
        mut f: impl FnMut(usize, ItemId) -> Result<(), E>,
    ) -> Result<(), E> {
        let group = &self[group_id];

        for (ind, item_id) in group.groups.iter().copied().enumerate() {
            f(ind, item_id)?;
        }

        Ok(())
    }

    pub fn deep_iter<E, F: FnMut(NodeId) -> Result<(), E>>(
        &self,
        item_id: ItemId,
        f: &mut F,
    ) -> Result<(), E> {
        match item_id {
            ItemId::Node(node_id) => {
                f(node_id)?;
            }
            ItemId::Group(group_id) => {
                let group = &self[group_id];

                for item_id in group.groups.iter().copied() {
                    self.deep_iter(item_id, &mut *f)?;
                }
            }
        }

        Ok(())
    }
}
