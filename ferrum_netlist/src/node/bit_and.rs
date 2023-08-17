use std::ops::BitAnd;

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{
    index::{Index, NodeIndex},
    net_kind::NetKind,
    output::NodeOutput,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct BitAndNode {
    pub input1: NodeIndex,
    pub input2: NodeIndex,
    pub out: NodeOutput,
}

impl BitAndNode {
    pub fn new(ty: PrimTy, input1: NodeIndex, input2: NodeIndex, sym: Symbol) -> Self {
        Self {
            input1,
            input2,
            out: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<BitAndNode> for Node {
    fn from(node: BitAndNode) -> Self {
        Self::BitAnd(node)
    }
}

impl<I: Index> IsNode<I> for BitAndNode {
    type Outputs = (<DummyTy as BitAnd>::Output,);

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
        [self.input1, self.input2].into_iter()
    }
}
