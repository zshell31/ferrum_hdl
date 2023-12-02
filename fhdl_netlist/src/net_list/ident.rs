use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use rustc_macros::{Decodable, Encodable};

pub trait Idx: Debug + Default + Copy + Eq + Hash + 'static {
    fn new(idx: usize) -> Self;

    fn idx(self) -> usize;
}

pub trait IsId: Debug + Copy + PartialEq + Hash + 'static {}

impl Idx for u32 {
    #[inline(always)]
    fn new(idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);
        idx as u32
    }

    #[inline(always)]
    fn idx(self) -> usize {
        self as usize
    }
}

macro_rules! idx_type {
    ($name:ident) => {
        #[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
        #[repr(transparent)]
        pub struct $name(u32);

        impl Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({})", stringify!($name), self.idx())
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Display::fmt(&self.0, f)
            }
        }

        impl From<u32> for $name {
            #[inline(always)]
            fn from(idx: u32) -> Self {
                Self(idx)
            }
        }

        impl Idx for $name {
            #[inline(always)]
            fn new(idx: usize) -> Self {
                Self(<u32 as Idx>::new(idx))
            }

            #[inline(always)]
            fn idx(self) -> usize {
                <u32 as Idx>::idx(self.0)
            }
        }

        impl IsId for $name {}
    };
}

macro_rules! composite_type {
    ($name:ident($get_base:ident => $base:ty, $get_idx:ident $( => $idx:ty )? )) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Encodable, Decodable)]
        pub struct $name($base, u32);

        impl Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({:?}, {})", stringify!($name), self.$get_base(), self.$get_idx())
            }
        }

        impl IsId for $name {}

        impl $name {
            #[inline(always)]
            pub fn new(base_id: $base, idx: usize) -> Self {
                Self(base_id, <u32 as Idx>::new(idx))
            }

            #[inline(always)]
            pub fn $get_base(&self) -> $base {
                self.0
            }

            #[inline(always)]
            pub fn $get_idx(&self) -> usize {
                <u32 as Idx>::idx(self.1)
            }

            $(
                #[inline(always)]
                pub fn combine(base_id: $base, idx: $idx) -> Self {
                    Self::new(base_id, idx.idx())
                }

                #[inline(always)]
                pub fn split(self) -> ($base, $idx) {
                    (self.$get_base(), self.1.into())
            }
            )?
        }
    };
}

idx_type!(ModuleId);
idx_type!(NodeIdx);
idx_type!(TempNodeId);
idx_type!(TyId);
idx_type!(ParamId);

composite_type!(NodeId(module_id => ModuleId, idx => NodeIdx));
composite_type!(NodeOutId(node_id => NodeId, idx => u32));
composite_type!(NodeInId(node_id => NodeId, idx => u32));
composite_type!(NodeOutIdx(node_idx => NodeIdx, idx => u32));
composite_type!(NodeInIdx(node_idx => NodeIdx, idx => u32));

impl From<NodeId> for NodeIdx {
    fn from(id: NodeId) -> Self {
        let (_, node_idx) = id.split();
        node_idx
    }
}

impl NodeId {
    pub(crate) fn make(module_id: ModuleId, node_idx: NodeIdx) -> Self {
        Self::combine(module_id, node_idx)
    }
}

impl From<NodeOutId> for NodeOutIdx {
    fn from(id: NodeOutId) -> Self {
        let (node_id, idx) = id.split();
        let (_, node_idx) = node_id.split();
        Self::combine(node_idx, idx)
    }
}

impl NodeOutId {
    pub(crate) fn make(module_id: ModuleId, node_out_idx: NodeOutIdx) -> Self {
        let (node_idx, idx) = node_out_idx.split();
        let node_id = NodeId::combine(module_id, node_idx);
        Self::combine(node_id, idx)
    }

    pub fn with_module_id(self, module_id: ModuleId) -> Self {
        let node_out_idx = self.into();
        Self::make(module_id, node_out_idx)
    }
}

impl From<NodeInId> for NodeInIdx {
    fn from(id: NodeInId) -> Self {
        let (node_id, idx) = id.split();
        let (_, node_idx) = node_id.split();
        Self::combine(node_idx, idx)
    }
}
