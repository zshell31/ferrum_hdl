use ferrum::prim_ty::{DummyTy, IsPrimTy, PrimTy};

use super::{Component, IsNode, Node};
use crate::{
    index::{Index, NodeId, NodeIndex},
    net_kind::NetKind,
    output::{NodeOutput, Outputs},
    symbol::Symbol,
};

// input wire a[N:0];
// output wire b[N:0];
// assign b = a;
#[derive(Debug, Clone, Copy)]
pub struct Pass<A = DummyTy> {
    pub input: NodeId<A>,
}

#[derive(Debug, Clone, Copy)]
pub struct PassNode {
    pub input: NodeIndex,
    pub out: NodeOutput,
}

impl PassNode {
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

impl From<PassNode> for Node {
    fn from(node: PassNode) -> Self {
        Self::Pass(node)
    }
}

impl IsNode for PassNode {
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

impl<I: Index, A: IsPrimTy> Component<I> for Pass<A> {
    type Node = PassNode;
    type Outputs = (A,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        PassNode {
            input: self.input.index(),
            out: NodeOutput {
                ty: A::prim_ty(),
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}
