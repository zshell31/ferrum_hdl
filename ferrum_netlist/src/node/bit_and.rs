use std::ops::BitAnd;

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{net_kind::NetKind, net_list::NodeId, output::NodeOutput, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct BitAndNode {
    pub input1: NodeId,
    pub input2: NodeId,
    pub out: NodeOutput,
}

impl BitAndNode {
    pub fn new(ty: PrimTy, input1: NodeId, input2: NodeId, sym: Symbol) -> Self {
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

impl IsNode for BitAndNode {
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

    fn inputs(&self) -> impl Iterator<Item = NodeId> {
        [self.input1, self.input2].into_iter()
    }
}
