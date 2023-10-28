use std::{
    fmt::{self, Debug},
    hash::Hash,
};

const MOD_ID_OFFSET: u8 = 40;
const MOD_ID_MASK: u64 = (1 << 24) - 1;

const NODE_ID_OFFSET: u8 = 8;
const NODE_ID_MASK: u64 = (1 << 32) - 1;

const OUT_ID_MASK: u64 = (1 << 8) - 1;

pub trait IsId: Debug + Copy + Eq + Hash {}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

impl IsId for ModuleId {}

impl ModuleId {
    pub(crate) fn new(idx: usize) -> Self {
        assert!(idx <= MOD_ID_MASK as usize);

        Self(idx as u32)
    }

    pub(crate) fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u64);

impl Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeId({:?}, {})", self.module_id(), self.idx())
    }
}

impl IsId for NodeId {}

impl NodeId {
    pub(crate) fn new(mod_id: ModuleId, idx: usize) -> Self {
        assert!(idx <= NODE_ID_MASK as usize);

        Self(
            ((mod_id.0 as u64) << MOD_ID_OFFSET)
                | (((idx as u64) & NODE_ID_MASK) << NODE_ID_OFFSET),
        )
    }

    pub fn module_id(&self) -> ModuleId {
        ModuleId(((self.0 >> MOD_ID_OFFSET) & MOD_ID_MASK) as u32)
    }

    pub(crate) fn idx(&self) -> usize {
        ((self.0 >> NODE_ID_OFFSET) & NODE_ID_MASK) as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeOutId(u64);

impl IsId for NodeOutId {}

impl Debug for NodeOutId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeOutId({:?}, {})", self.node_id(), self.out_id())
    }
}

impl NodeOutId {
    pub(crate) fn new(node_id: NodeId, idx: usize) -> Self {
        assert!(idx <= OUT_ID_MASK as usize);

        Self(node_id.0 | ((idx as u64) & OUT_ID_MASK))
    }

    pub fn node_id(&self) -> NodeId {
        NodeId(self.0 & !OUT_ID_MASK)
    }

    pub fn out_id(&self) -> usize {
        (self.0 & OUT_ID_MASK) as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeInId(u64);

impl IsId for NodeInId {}

impl Debug for NodeInId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeInId({:?}, {})", self.node_id(), self.out_id())
    }
}

impl NodeInId {
    pub(crate) fn new(node_id: NodeId, idx: usize) -> Self {
        assert!(idx <= OUT_ID_MASK as usize);

        Self(node_id.0 | ((idx as u64) & OUT_ID_MASK))
    }

    pub fn node_id(&self) -> NodeId {
        NodeId(self.0 & !OUT_ID_MASK)
    }

    pub fn out_id(&self) -> usize {
        (self.0 & OUT_ID_MASK) as usize
    }
}
