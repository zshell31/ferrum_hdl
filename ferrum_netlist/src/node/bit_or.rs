use std::ops::BitOr;

use ferrum::prim_ty::{DummyTy, IsPrimTy, PrimTy};

use super::{Component, IsNode, Node};
use crate::{
    index::{Index, NodeId, NodeIndex},
    net_kind::NetKind,
    output::{NodeOutput, Outputs},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct BitOrComp<A = DummyTy>
where
    A: BitOr,
{
    pub input1: NodeId<A>,
    pub input2: NodeId<A>,
}

#[derive(Debug, Clone, Copy)]
pub struct BitOrNode {
    pub input1: NodeIndex,
    pub input2: NodeIndex,
    pub out: NodeOutput,
}

impl BitOrNode {
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

impl From<BitOrNode> for Node {
    fn from(node: BitOrNode) -> Self {
        Self::BitOr(node)
    }
}

impl IsNode for BitOrNode {
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

impl<I: Index, A: BitOr + IsPrimTy> Component<I> for BitOrComp<A>
where
    <A as BitOr>::Output: IsPrimTy,
{
    type Node = BitOrNode;
    type Outputs = (<A as BitOr>::Output,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        BitOrNode::new(
            <A as BitOr>::Output::prim_ty(),
            self.input1.index(),
            self.input2.index(),
            sym,
        )
    }
}
