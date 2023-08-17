use std::ops::Not;

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{
    index::{Index, NodeIndex},
    net_kind::NetKind,
    output::NodeOutput,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct BitNotNode {
    pub input: NodeIndex,
    pub out: NodeOutput,
}

impl BitNotNode {
    pub fn new(ty: PrimTy, input: NodeIndex, sym: Symbol) -> Self {
        Self {
            input,
            out: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<BitNotNode> for Node {
    fn from(node: BitNotNode) -> Self {
        Self::BitNot(node)
    }
}

impl<I: Index> IsNode<I> for BitNotNode {
    type Outputs = (<DummyTy as Not>::Output,);

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
        [self.input].into_iter()
    }
}
