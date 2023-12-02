use std::{
    fmt::{self, Debug},
    hash::Hash,
};

use rustc_macros::{Decodable, Encodable};

pub trait IsId: Debug + Copy + Eq + Hash {}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct ModuleId(u32);

impl IsId for ModuleId {}

impl ModuleId {
    pub(crate) fn new(idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);

        Self(idx as u32)
    }

    pub(crate) fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct NodeId(ModuleId, u32);

impl Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeId({:?}, {})", self.module_id(), self.idx())
    }
}

impl IsId for NodeId {}

impl NodeId {
    pub(crate) fn new(mod_id: ModuleId, idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);

        Self(mod_id, idx as u32)
    }

    pub fn module_id(&self) -> ModuleId {
        self.0
    }

    pub(crate) fn idx(&self) -> usize {
        self.1 as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct NodeOutId(NodeId, u32);

impl IsId for NodeOutId {}

impl Debug for NodeOutId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeOutId({:?}, {})", self.node_id(), self.out_id())
    }
}

impl NodeOutId {
    pub(crate) fn new(node_id: NodeId, idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);

        Self(node_id, idx as u32)
    }

    pub fn node_id(&self) -> NodeId {
        self.0
    }

    pub fn out_id(&self) -> usize {
        self.1 as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
pub struct NodeInId(NodeId, u32);

impl IsId for NodeInId {}

impl Debug for NodeInId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeInId({:?}, {})", self.node_id(), self.in_id())
    }
}

impl NodeInId {
    pub(crate) fn new(node_id: NodeId, idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);

        Self(node_id, idx as u32)
    }

    pub fn node_id(&self) -> NodeId {
        self.0
    }

    pub fn in_id(&self) -> usize {
        self.1 as usize
    }
}
