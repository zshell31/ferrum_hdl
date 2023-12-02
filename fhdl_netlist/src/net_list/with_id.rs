use std::ops::{Deref, DerefMut};

use super::{ident::IsId, NodeInId, NodeOutId};

#[derive(Debug, Clone, Copy)]
pub struct WithId<Id: IsId, T>(pub Id, pub T);

impl<Id: IsId, T> WithId<Id, T> {
    pub(crate) fn new(id: Id, inner: T) -> Self {
        Self(id, inner)
    }

    pub fn id(&self) -> Id {
        self.0
    }

    pub fn into_inner(self) -> T {
        self.1
    }
}

impl<Id: IsId, T> Deref for WithId<Id, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<Id: IsId, T> DerefMut for WithId<Id, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T> WithId<NodeInId, T> {
    #[inline(always)]
    pub fn node_in_id(&self) -> NodeInId {
        self.id()
    }
}

impl<T> WithId<NodeOutId, T> {
    #[inline(always)]
    pub fn node_out_id(&self) -> NodeOutId {
        self.id()
    }
}
