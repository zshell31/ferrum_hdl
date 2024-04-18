use std::ops::{Index, IndexMut};

use crate::{
    cursor::Cursor,
    index::IndexType,
    index_storage::IndexStorage,
    list::{List, ListCursor, ListItem, ListStorage},
};

#[derive(Debug)]
pub struct TreeNode<I: IndexType, T> {
    pub data: T,
    parent: I,
    next: I,
    prev: I,
    children: List<Tree<I, T>>,
}

impl<I: IndexType, T> TreeNode<I, T> {
    fn new(data: T, parent: I) -> Self {
        Self {
            data,
            next: I::EMPTY,
            prev: I::EMPTY,
            parent,
            children: List::default(),
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<I> {
        self.parent.into_opt()
    }

    #[inline]
    pub fn children(&self) -> ListCursor<Tree<I, T>> {
        self.children.cursor()
    }

    #[inline]
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }
}

impl<I: IndexType, T> ListItem<I> for TreeNode<I, T> {
    fn next(&self) -> I {
        self.next
    }

    fn set_next(&mut self, next: I) {
        self.next = next;
    }

    fn prev(&self) -> I {
        self.prev
    }

    fn set_prev(&mut self, prev: I) {
        self.prev = prev;
    }
}

#[derive(Debug)]
pub struct Tree<I: IndexType, T> {
    nodes: IndexStorage<I, TreeNode<I, T>>,
    root: I,
}

impl<I: IndexType, T> Index<I> for Tree<I, T> {
    type Output = TreeNode<I, T>;

    #[inline]
    fn index(&self, idx: I) -> &Self::Output {
        &self.nodes[idx]
    }
}

impl<I: IndexType, T> IndexMut<I> for Tree<I, T> {
    #[inline]
    fn index_mut(&mut self, idx: I) -> &mut Self::Output {
        &mut self.nodes[idx]
    }
}

impl<I: IndexType, T> ListStorage for Tree<I, T> {
    type Idx = I;

    type Item = TreeNode<I, T>;
}

impl<I: IndexType, T> Default for Tree<I, T> {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            root: I::EMPTY,
        }
    }
}

impl<I: IndexType, T> Tree<I, T> {
    pub fn new_with_root(root_data: T) -> Self {
        let mut data = IndexStorage::with_capacity(1);
        let root = data.push(TreeNode::new(root_data, I::EMPTY));

        Self { nodes: data, root }
    }

    #[inline]
    pub fn root_id(&self) -> Option<I> {
        self.root.into_opt()
    }

    #[inline]
    pub fn root(&self) -> Option<&T> {
        self.root.into_opt().map(|root| &self.nodes[root].data)
    }

    #[inline]
    pub fn root_mut(&mut self) -> Option<&mut T> {
        self.root.into_opt().map(|root| &mut self.nodes[root].data)
    }

    #[inline]
    pub fn children(&self, node_id: I) -> ListCursor<Self> {
        self.nodes[node_id].children()
    }

    pub fn add(&mut self, parent_id: I, data: T) -> I {
        let child = self.nodes.push(TreeNode::new(data, parent_id));
        let mut children = self.nodes[parent_id].children;
        children.add(self, child);
        self.nodes[parent_id].children = children;

        child
    }

    pub fn remove(&mut self, node_id: I) {
        self.remove_children(node_id);

        if let Some(parent_id) = self.nodes[node_id].parent() {
            let mut children = self.nodes[parent_id].children;
            children.remove(self, node_id);
            self.nodes[parent_id].children = children;
        }

        self.nodes.swap_remove(&node_id);
    }

    pub fn remove_children(&mut self, node_id: I) {
        let mut children = self.nodes[node_id].children();
        while let Some(child_id) = children.next_(self) {
            self.remove(child_id);
        }
    }

    pub fn prune_all_except_root(&mut self) {
        let root = self.nodes.swap_remove(&self.root);
        self.nodes.clear();
        *self = match root {
            Some(root) => Self::new_with_root(root.data),
            None => Self::default(),
        };
    }
}
