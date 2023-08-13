use ferrum::prim_ty::{IsPrimTy, PrimTy};

use crate::{
    index::{Index, NodeId, NodeIndex},
    net_kind::NetKind,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct NodeOutput {
    pub ty: PrimTy,
    pub sym: Symbol,
    pub kind: NetKind,
}

pub trait Outputs<I: Index> {
    type NodeIds: Copy;
    type Symbols: Copy;

    fn make_node_ids(index: &mut I) -> Self::NodeIds;

    fn make_node_idx(index: &mut I) -> Vec<NodeIndex>;
}

pub trait IsOneOutput {}

impl<T1> IsOneOutput for (T1,) {}

impl<I: Index, T1: IsPrimTy> Outputs<I> for (T1,) {
    type NodeIds = (NodeId<T1>,);
    type Symbols = (Symbol,);

    fn make_node_ids(index: &mut I) -> Self::NodeIds {
        (index.node_id::<T1>(0),)
    }

    fn make_node_idx(index: &mut I) -> Vec<NodeIndex> {
        let (node_id,) = Self::make_node_ids(index);
        vec![node_id.index()]
    }
}

impl<I: Index, T1, T2> Outputs<I> for (T1, T2) {
    type NodeIds = (NodeId<T1>, NodeId<T2>);
    type Symbols = (Symbol, Symbol);

    fn make_node_ids(index: &mut I) -> Self::NodeIds {
        (index.node_id::<T1>(0), index.node_id::<T2>(1))
    }

    fn make_node_idx(index: &mut I) -> Vec<NodeIndex> {
        let (node_id1, node_id2) = Self::make_node_ids(index);
        vec![node_id1.index(), node_id2.index()]
    }
}
