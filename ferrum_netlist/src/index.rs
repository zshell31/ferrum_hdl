use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeIndex(pub(crate) usize, pub(crate) u8);

pub trait Index {
    fn index(&mut self, out: u8) -> NodeIndex;

    fn node_id<A>(&mut self, out: u8) -> NodeId<A> {
        self.index(out).into()
    }
}

#[derive(Debug)]
pub struct NodeId<A>(NodeIndex, PhantomData<A>);

impl<A> Clone for NodeId<A> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<A> Copy for NodeId<A> {}

impl<A> NodeId<A> {
    pub fn index(&self) -> NodeIndex {
        self.0
    }
}

impl<A> From<NodeIndex> for NodeId<A> {
    fn from(ind: NodeIndex) -> Self {
        Self(ind, PhantomData)
    }
}
