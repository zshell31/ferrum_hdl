use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{net_kind::NetKind, net_list::NodeId, output::NodeOutput, symbol::Symbol};

// input wire a[N:0];
// output wire b[N:0];
// assign b = a;
#[derive(Debug, Clone, Copy)]
pub struct PassNode {
    pub input: NodeId,
    pub out: NodeOutput,
}

impl PassNode {
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

impl From<PassNode> for Node {
    fn from(node: PassNode) -> Self {
        Self::Pass(node)
    }
}

impl IsNode for PassNode {
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

    fn inputs(&self) -> impl Iterator<Item = NodeId> {
        [self.input].into_iter()
    }
}
