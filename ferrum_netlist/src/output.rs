use ferrum::prim_ty::PrimTy;

use crate::{
    net_kind::NetKind,
    net_list::{NetList, NodeId},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct NodeOutput {
    pub ty: PrimTy,
    pub sym: Symbol,
    pub kind: NetKind,
}

pub trait Outputs {
    type Symbols: Copy;

    fn make_node_ids(net_list: &mut NetList) -> Vec<NodeId>;
}

pub trait IsOneOutput {}

impl<T1> IsOneOutput for (T1,) {}

impl<T1> Outputs for (T1,) {
    type Symbols = (Symbol,);

    fn make_node_ids(net_list: &mut NetList) -> Vec<NodeId> {
        let (node_id,) = (net_list.index(0),);
        vec![node_id]
    }
}

impl<T1, T2> Outputs for (T1, T2) {
    type Symbols = (Symbol, Symbol);

    fn make_node_ids(net_list: &mut NetList) -> Vec<NodeId> {
        let (node_id1, node_id2) = (net_list.index(0), net_list.index(1));
        vec![node_id1, node_id2]
    }
}
