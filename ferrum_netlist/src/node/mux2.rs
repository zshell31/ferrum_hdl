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
pub struct Mux2<A = DummyTy> {
    pub sel: NodeId<Bit>,
    pub input1: NodeId<A>,
    pub input2: NodeId<A>,
}

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

impl IsNode for Mux2Node {
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

impl<I: Index, A: IsPrimTy> Component<I> for Mux2<A> {
    type Node = Mux2Node;
    type Outputs = (A,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        Mux2Node::new(
            A::prim_ty(),
            self.sel.index(),
            self.input1.index(),
            self.input2.index(),
            sym,
        )
    }
}
