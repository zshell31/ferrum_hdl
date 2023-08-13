use either::Either;
use ferrum::{
    bit::Bit,
    prim_ty::{DummyTy, IsPrimTy, PrimTy},
};

use super::{Component, IsNode, Node};
use crate::{
    index::{Index, NodeId, NodeIndex},
    net_kind::NetKind,
    output::{NodeOutput, Outputs},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct DFF<A = DummyTy> {
    pub clk: NodeId<Bit>,
    pub rst_value: NodeId<Bit>,
    pub data: Option<NodeId<A>>,
}

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

impl IsNode for DFFNode {
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

impl<I: Index, A: IsPrimTy> Component<I> for DFF<A> {
    type Node = DFFNode;
    type Outputs = (A,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        DFFNode::new(
            A::prim_ty(),
            self.clk.index(),
            self.rst_value.index(),
            self.data.map(|data| data.index()),
            sym,
        )
    }
}
