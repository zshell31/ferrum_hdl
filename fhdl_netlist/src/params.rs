use std::iter;

use either::Either;

use crate::{
    net_list::{NodeId, NodeOutId},
    node::NodeOutput,
};

#[allow(clippy::len_without_is_empty)]
pub trait Inputs {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_;

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_;

    fn len(&self) -> usize;
}

impl Inputs for () {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        iter::empty()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        iter::empty()
    }

    fn len(&self) -> usize {
        0
    }
}

impl Inputs for NodeOutId {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [*self].into_iter()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        [self].into_iter()
    }

    fn len(&self) -> usize {
        1
    }
}

impl Inputs for (NodeOutId, NodeOutId) {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [self.0, self.1].into_iter()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        [&mut self.0, &mut self.1].into_iter()
    }

    fn len(&self) -> usize {
        2
    }
}

impl Inputs for (NodeOutId, NodeOutId, NodeOutId) {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [self.0, self.1, self.2].into_iter()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        [&mut self.0, &mut self.1, &mut self.2].into_iter()
    }

    fn len(&self) -> usize {
        3
    }
}

impl Inputs for [NodeOutId] {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        self.iter().copied()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        self.iter_mut()
    }

    fn len(&self) -> usize {
        self.len()
    }
}

#[derive(Debug)]
pub struct NodeOutWithId<'a> {
    pub out: &'a NodeOutput,
    pub ind: usize,
}

impl<'a> NodeOutWithId<'a> {
    pub fn node_out_id(&self, node_id: NodeId) -> NodeOutId {
        NodeOutId::new(node_id, self.ind)
    }
}

pub struct NodeOutWithIdMut<'a> {
    pub out: &'a mut NodeOutput,
    pub ind: usize,
}

impl<'a> NodeOutWithIdMut<'a> {
    pub fn node_out_id(&self, node_id: NodeId) -> NodeOutId {
        NodeOutId::new(node_id, self.ind)
    }
}

#[allow(clippy::len_without_is_empty)]
pub trait Outputs {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_;

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_;

    fn by_ind(&self, ind: usize) -> NodeOutWithId<'_>;

    fn by_ind_mut(&mut self, ind: usize) -> NodeOutWithIdMut<'_>;

    fn len(&self) -> usize;

    fn only_one(&self) -> NodeOutWithId<'_> {
        assert_eq!(self.len(), 1);
        self.by_ind(0)
    }

    fn only_one_mut(&mut self) -> NodeOutWithIdMut<'_> {
        assert_eq!(self.len(), 1);
        self.by_ind_mut(0)
    }
}

macro_rules! no_output {
    ($out:ident) => {
        panic!("there is no output with the index {}", $out)
    };
}

impl Outputs for () {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
        iter::empty()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
        iter::empty()
    }

    fn by_ind(&self, _ind: usize) -> NodeOutWithId<'_> {
        panic!("no outputs")
    }

    fn by_ind_mut(&mut self, _ind: usize) -> NodeOutWithIdMut<'_> {
        panic!("no outputs")
    }

    fn len(&self) -> usize {
        0
    }
}

impl Outputs for NodeOutput {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
        [self]
            .into_iter()
            .enumerate()
            .map(move |(ind, out)| NodeOutWithId { out, ind })
    }

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
        [self]
            .into_iter()
            .enumerate()
            .map(move |(ind, out)| NodeOutWithIdMut { out, ind })
    }

    fn by_ind(&self, ind: usize) -> NodeOutWithId<'_> {
        match ind {
            0 => NodeOutWithId { out: self, ind },
            _ => no_output!(ind),
        }
    }

    fn by_ind_mut(&mut self, ind: usize) -> NodeOutWithIdMut<'_> {
        match ind {
            0 => NodeOutWithIdMut { out: self, ind },
            _ => no_output!(ind),
        }
    }

    fn len(&self) -> usize {
        1
    }
}

impl Outputs for Option<NodeOutput> {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
        match self {
            Some(output) => Either::Left(output.items()),
            None => Either::Right(iter::empty()),
        }
    }

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
        match self {
            Some(output) => Either::Left(output.items_mut()),
            None => Either::Right(iter::empty()),
        }
    }

    fn by_ind(&self, ind: usize) -> NodeOutWithId<'_> {
        match self {
            Some(output) => output.by_ind(ind),
            None => no_output!(ind),
        }
    }

    fn by_ind_mut(&mut self, ind: usize) -> NodeOutWithIdMut<'_> {
        match self {
            Some(output) => output.by_ind_mut(ind),
            None => no_output!(ind),
        }
    }

    fn len(&self) -> usize {
        match self {
            Some(output) => output.len(),
            None => 0,
        }
    }
}

impl Outputs for [NodeOutput] {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
        self.iter()
            .enumerate()
            .map(move |(ind, out)| NodeOutWithId { out, ind })
    }

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
        self.iter_mut()
            .enumerate()
            .map(move |(ind, out)| NodeOutWithIdMut { out, ind })
    }

    fn by_ind(&self, ind: usize) -> NodeOutWithId<'_> {
        match ind {
            n if n < self.len() => NodeOutWithId {
                out: &self[ind],
                ind,
            },
            _ => no_output!(ind),
        }
    }

    fn by_ind_mut(&mut self, ind: usize) -> NodeOutWithIdMut<'_> {
        match ind {
            n if n < self.len() => NodeOutWithIdMut {
                out: &mut self[ind],
                ind,
            },
            _ => no_output!(ind),
        }
    }

    fn len(&self) -> usize {
        self.len()
    }
}
