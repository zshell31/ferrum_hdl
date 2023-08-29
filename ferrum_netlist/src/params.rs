use crate::{
    net_list::{NodeId, NodeOutId, OutId},
    node::NodeOutput,
};

#[allow(clippy::len_without_is_empty)]
pub trait Inputs {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_;

    fn len(&self) -> usize;
}

impl Inputs for () {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [].into_iter()
    }

    fn len(&self) -> usize {
        0
    }
}

impl Inputs for NodeOutId {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [*self].into_iter()
    }

    fn len(&self) -> usize {
        1
    }
}

impl Inputs for (NodeOutId, NodeOutId) {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [self.0, self.1].into_iter()
    }

    fn len(&self) -> usize {
        2
    }
}

impl Inputs for (NodeOutId, NodeOutId, NodeOutId) {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [self.0, self.1, self.2].into_iter()
    }

    fn len(&self) -> usize {
        3
    }
}

impl Inputs for Vec<NodeOutId> {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        self.iter().copied()
    }

    fn len(&self) -> usize {
        self.len()
    }
}

pub struct NodeOutWithId<'a> {
    pub out: &'a NodeOutput,
    pub id: OutId,
}

impl<'a> NodeOutWithId<'a> {
    pub fn node_out_id(&self, node_id: NodeId) -> NodeOutId {
        NodeOutId::new(node_id, self.id)
    }
}

pub struct NodeOutWithIdMut<'a> {
    pub out: &'a mut NodeOutput,
    pub id: OutId,
}

impl<'a> NodeOutWithIdMut<'a> {
    pub fn node_out_id(&self, node_id: NodeId) -> NodeOutId {
        NodeOutId::new(node_id, self.id)
    }
}

#[allow(clippy::len_without_is_empty)]
pub trait Outputs {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_;

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_;

    fn by_ind(&self, out: OutId) -> &NodeOutput;

    fn by_ind_mut(&mut self, out: OutId) -> &mut NodeOutput;

    fn len(&self) -> usize;

    fn only_one(&self) -> NodeOutWithId<'_> {
        assert_eq!(self.len(), 1);
        NodeOutWithId {
            out: self.by_ind(0),
            id: 0,
        }
    }

    fn only_one_mut(&mut self) -> NodeOutWithIdMut<'_> {
        assert_eq!(self.len(), 1);
        NodeOutWithIdMut {
            out: self.by_ind_mut(0),
            id: 0,
        }
    }
}

macro_rules! no_output {
    ($out:ident) => {
        panic!("there is no output with the index {}", $out)
    };
}

impl Outputs for NodeOutput {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
        [self]
            .into_iter()
            .enumerate()
            .map(move |(id, out)| NodeOutWithId {
                out,
                id: id as OutId,
            })
    }

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
        [self]
            .into_iter()
            .enumerate()
            .map(move |(id, out)| NodeOutWithIdMut {
                out,
                id: id as OutId,
            })
    }

    fn by_ind(&self, out: OutId) -> &NodeOutput {
        match out {
            0 => self,
            _ => no_output!(out),
        }
    }

    fn by_ind_mut(&mut self, out: OutId) -> &mut NodeOutput {
        match out {
            0 => self,
            _ => no_output!(out),
        }
    }

    fn len(&self) -> usize {
        1
    }
}

impl Outputs for Vec<NodeOutput> {
    fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
        self.iter().enumerate().map(move |(id, out)| NodeOutWithId {
            out,
            id: id as OutId,
        })
    }

    fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
        self.iter_mut()
            .enumerate()
            .map(move |(id, out)| NodeOutWithIdMut {
                out,
                id: id as OutId,
            })
    }

    fn by_ind(&self, out: OutId) -> &NodeOutput {
        match out {
            n if n < self.len() => &self[out],
            _ => no_output!(out),
        }
    }

    fn by_ind_mut(&mut self, out: OutId) -> &mut NodeOutput {
        match out {
            n if n < self.len() => &mut self[out],
            _ => no_output!(out),
        }
    }

    fn len(&self) -> usize {
        self.len()
    }
}
