use ferrum::prim_ty::{IsPrimTy, PrimTy};

use crate::{
    index::{Index, NodeIndex},
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
    type Symbols: Copy;

    fn make_node_idx(index: &mut I) -> Vec<NodeIndex>;
}

pub trait IsOneOutput {}

impl<T1> IsOneOutput for (T1,) {}

impl<I: Index, T1: IsPrimTy> Outputs<I> for (T1,) {
    type Symbols = (Symbol,);

    fn make_node_idx(index: &mut I) -> Vec<NodeIndex> {
        let (node_id,) = (index.index(0),);
        vec![node_id]
    }
}

impl<I: Index, T1, T2> Outputs<I> for (T1, T2) {
    type Symbols = (Symbol, Symbol);

    fn make_node_idx(index: &mut I) -> Vec<NodeIndex> {
        let (node_id1, node_id2) = (index.index(0), index.index(1));
        vec![node_id1, node_id2]
    }
}
