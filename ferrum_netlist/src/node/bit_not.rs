use std::ops::Not;

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{net_kind::NetKind, net_list::NodeId, output::NodeOutput, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct BitNotNode {
    pub input: NodeId,
    pub out: NodeOutput,
}

impl BitNotNode {
    pub fn new(ty: PrimTy, input: NodeId, sym: Symbol) -> Self {
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

impl IsNode for BitNotNode {
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

    fn inputs(&self) -> impl Iterator<Item = NodeId> {
        [self.input].into_iter()
    }
}
