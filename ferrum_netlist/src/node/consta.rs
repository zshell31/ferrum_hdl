use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{
    index::{Index, NodeIndex},
    net_kind::NetKind,
    output::NodeOutput,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct ConstNode {
    pub value: u128,
    pub out: NodeOutput,
}

impl ConstNode {
    pub fn new(ty: PrimTy, value: u128, sym: Symbol) -> Self {
        Self {
            value,
            out: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<ConstNode> for Node {
    fn from(node: ConstNode) -> Self {
        Self::Const(node)
    }
}

impl<I: Index> IsNode<I> for ConstNode {
    type Outputs = (DummyTy,);

    fn node_output(&self, out: u8) -> &NodeOutput {
        match out {
            0 => &self.out,
            _ => unreachable!(),
        }
    }

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput {
        match out {
            0 => &mut self.out,
            _ => unreachable!(),
        }
    }

    fn inputs(&self) -> impl Iterator<Item = NodeIndex> {
        [].into_iter()
    }
}
