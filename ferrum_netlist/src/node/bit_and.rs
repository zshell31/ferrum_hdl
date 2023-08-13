use std::ops::BitAnd;

use ferrum::prim_ty::{DummyTy, IsPrimTy, PrimTy};

use super::{Component, IsNode, Node};
use crate::{
    index::{Index, NodeId, NodeIndex},
    net_kind::NetKind,
    output::{NodeOutput, Outputs},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct BitAndComp<A = DummyTy>
where
    A: BitAnd,
{
    pub input1: NodeId<A>,
    pub input2: NodeId<A>,
}

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

impl IsNode for BitAndNode {
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

impl<I: Index, A: BitAnd + IsPrimTy> Component<I> for BitAndComp<A>
where
    <A as BitAnd>::Output: IsPrimTy,
{
    type Node = BitAndNode;
    type Outputs = (<A as BitAnd>::Output,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        BitAndNode::new(
            <A as BitAnd>::Output::prim_ty(),
            self.input1.index(),
            self.input2.index(),
            sym,
        )
    }
}
