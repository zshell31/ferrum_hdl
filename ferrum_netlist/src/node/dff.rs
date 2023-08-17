use either::Either;
use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{
    index::{Index, NodeIndex},
    net_kind::NetKind,
    output::NodeOutput,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct DFFNode {
    pub clk: NodeIndex,
    pub rst_value: NodeIndex,
    pub data: Option<NodeIndex>,
    pub out: NodeOutput,
}

impl DFFNode {
    pub fn new(
        ty: PrimTy,
        clk: NodeIndex,
        rst_value: NodeIndex,
        data: Option<NodeIndex>,
        sym: Symbol,
    ) -> Self {
        Self {
            clk,
            rst_value,
            data,
            out: NodeOutput {
                ty,
                sym,
                kind: NetKind::Reg,
            },
        }
    }
}

impl From<DFFNode> for Node {
    fn from(node: DFFNode) -> Self {
        Self::DFF(node)
    }
}

impl<I: Index> IsNode<I> for DFFNode {
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
        match self.data {
            Some(data) => Either::Left([self.clk, self.rst_value, data].into_iter()),
            None => Either::Right([self.clk, self.rst_value].into_iter()),
        }
    }
}
