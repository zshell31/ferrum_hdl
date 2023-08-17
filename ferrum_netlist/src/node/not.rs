use std::ops::Not;

use ferrum::prim_ty::{DummyTy, IsPrimTy, PrimTy};

use super::{Component, IsNode, Node};
use crate::{
    index::{Index, NodeId, NodeIndex},
    net_kind::NetKind,
    output::{NodeOutput, Outputs},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct NotComp<A: Not = DummyTy> {
    pub input: NodeId<A>,
}

#[derive(Debug, Clone, Copy)]
pub struct NotNode {
    pub input: NodeIndex,
    pub out: NodeOutput,
}

impl NotNode {
    pub fn new(ty: PrimTy, input: NodeIndex, sym: Symbol) -> Self {
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

impl From<NotNode> for Node {
    fn from(node: NotNode) -> Self {
        Self::Not(node)
    }
}

impl IsNode for NotNode {
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
        [self.input].into_iter()
    }
}

impl<I: Index, A: Not + IsPrimTy> Component<I> for NotComp<A>
where
    <A as Not>::Output: IsPrimTy,
{
    type Node = NotNode;
    type Outputs = (<A as Not>::Output,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        NotNode::new(A::prim_ty(), self.input.index(), sym)
    }
}
