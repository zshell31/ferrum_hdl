use std::fmt::{self, Debug};

const MOD_ID_OFFSET: u8 = 40;
const MOD_ID_MASK: u64 = (1 << 24) - 1;

const NODE_ID_OFFSET: u8 = 8;
const NODE_ID_MASK: u64 = (1 << 32) - 1;

const OUT_ID_MASK: u64 = (1 << 8) - 1;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(u32);

impl Debug for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ModuleId({})", self.0)
    }
}

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
        if self.is_none() {
            write!(f, "None")
        } else {
            write!(f, "NodeId({:?}, {})", self.module_id(), self.idx_())
        }
    }
}

impl NodeId {
    pub(crate) fn new(mod_id: ModuleId, mut idx: usize) -> Self {
        idx += 1;
        assert!(idx <= NODE_ID_MASK as usize);

        Self(
            ((mod_id.0 as u64) << MOD_ID_OFFSET)
                | (((idx as u64) & NODE_ID_MASK) << NODE_ID_OFFSET),
        )
    }

    pub(crate) fn none() -> Self {
        Self(0)
    }

    pub(crate) fn is_none(&self) -> bool {
        self.0 == 0
    }

    pub(crate) fn into_opt(self) -> Option<Self> {
        if self.is_none() {
            None
        } else {
            Some(self)
        }
    }

    pub(crate) fn from_opt(value: Option<Self>) -> Self {
        match value {
            Some(value) if !value.is_none() => value,
            _ => Self::none(),
        }
    }

    pub fn module_id(&self) -> ModuleId {
        ModuleId(((self.0 >> MOD_ID_OFFSET) & MOD_ID_MASK) as u32)
    }

    pub(crate) fn idx(&self) -> Option<usize> {
        if self.is_none() {
            None
        } else {
            Some(self.idx_())
        }
    }

    fn idx_(&self) -> usize {
        (((self.0 >> NODE_ID_OFFSET) & NODE_ID_MASK) as usize).saturating_sub(1)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeOutId(u64);

impl Debug for NodeOutId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeOutId({:?}, {})", self.node_id(), self.out_id())
    }
}

impl NodeOutId {
    pub(crate) fn new(node_id: NodeId, idx: usize) -> Self {
        assert!(idx <= OUT_ID_MASK as usize);
        assert!(!node_id.is_none());

        Self(node_id.0 | ((idx as u64) & OUT_ID_MASK))
    }

    pub fn node_id(&self) -> NodeId {
        NodeId(self.0 & !OUT_ID_MASK)
    }

    pub fn out_id(&self) -> usize {
        (self.0 & OUT_ID_MASK) as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn node_id() {
        assert!(NodeId::none().is_none());
        assert!(NodeId::none().idx().is_none());
        assert_eq!(NodeId::new(ModuleId::new(0), 0).idx(), Some(0));
    }
}
