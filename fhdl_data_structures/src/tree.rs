use std::ops::{Index, IndexMut};

use crate::{
    cursor::Cursor,
    index::IndexType,
    index_storage::IndexStorage,
    list::{List, ListCursor, ListItem, ListStorage},
};

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

    pub fn parent(&self) -> Option<I> {
        self.parent.into_opt()
    }

    pub fn children(&self) -> ListCursor<Tree<I, T>> {
        self.children.cursor()
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

pub struct Tree<I: IndexType, T> {
    data: IndexStorage<I, TreeNode<I, T>>,
    root: I,
}

impl<I: IndexType, T> Index<I> for Tree<I, T> {
    type Output = TreeNode<I, T>;

    fn index(&self, idx: I) -> &Self::Output {
        &self.data[idx]
    }
}

impl<I: IndexType, T> IndexMut<I> for Tree<I, T> {
    fn index_mut(&mut self, idx: I) -> &mut Self::Output {
        &mut self.data[idx]
    }
}

impl<I: IndexType, T> ListStorage for Tree<I, T> {
    type Idx = I;

    type Item = TreeNode<I, T>;
}

impl<I: IndexType, T> Default for Tree<I, T> {
    fn default() -> Self {
        Self {
            data: Default::default(),
            root: I::EMPTY,
        }
    }
}

impl<I: IndexType, T> Tree<I, T> {
    pub fn new_with_root(root_data: T) -> Self {
        let mut data = IndexStorage::with_capacity(1);
        let root = data.push(TreeNode::new(root_data, I::EMPTY));

        Self { data, root }
    }

    #[inline]
    pub fn root_id(&self) -> Option<I> {
        self.root.into_opt()
    }

    #[inline]
    pub fn root(&self) -> Option<&T> {
        self.root.into_opt().map(|root| &self.data[root].data)
    }

    #[inline]
    pub fn root_mut(&mut self) -> Option<&mut T> {
        self.root.into_opt().map(|root| &mut self.data[root].data)
    }

    pub fn add(&mut self, parent_id: I, data: T) -> I {
        let child = self.data.push(TreeNode::new(data, parent_id));
        let mut children = self.data[parent_id].children;
        children.add(self, child);
        self.data[parent_id].children = children;

        child
    }

    pub fn remove(&mut self, node_id: I) {
        self.remove_children(node_id);

        if let Some(parent_id) = self.data[node_id].parent() {
            let mut children = self.data[parent_id].children;
            children.remove(self, node_id);
            self.data[parent_id].children = children;
        }

        self.data.shift_remove(&node_id);
    }

    pub fn remove_children(&mut self, node_id: I) {
        let mut children = self.data[node_id].children();
        while let Some(child_id) = children.next_(self) {
            self.remove(child_id);
        }
    }

    pub fn prune_all_except_root(&mut self) {
        let root = self.data.swap_remove(&self.root);
        self.data.clear();
        *self = match root {
            Some(root) => Self::new_with_root(root.data),
            None => Self::default(),
        };
    }
}
