use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{
    index::{Index, NodeIndex},
    net_kind::NetKind,
    output::NodeOutput,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct Mux2Node {
    pub sel: NodeIndex,
    pub input1: NodeIndex,
    pub input2: NodeIndex,
    pub out: NodeOutput,
}

impl Mux2Node {
    pub fn new(
        ty: PrimTy,
        sel: NodeIndex,
        input1: NodeIndex,
        input2: NodeIndex,
        sym: Symbol,
    ) -> Self {
        Self {
            sel,
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

impl From<Mux2Node> for Node {
    fn from(node: Mux2Node) -> Self {
        Self::Mux2(node)
    }
}

impl<I: Index> IsNode<I> for Mux2Node {
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
        [self.input1, self.input2].into_iter()
    }
}
