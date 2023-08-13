use std::{
    fmt::{self, Debug},
    marker::PhantomData,
};

use ferrum::prim_ty::{DummyTy, IsPrimTy, PrimTy};

use super::{Component, IsNode, Node};
use crate::{
    index::Index,
    net_kind::NetKind,
    output::{NodeOutput, Outputs},
    symbol::Symbol,
};

pub struct Input<A = DummyTy>(PhantomData<A>);

impl<A> Default for Input<A> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<A> Debug for Input<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Input")
    }
}

impl<A> Clone for Input<A> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<A> Copy for Input<A> {}

#[derive(Debug, Clone, Copy)]
pub struct InputNode(pub PrimTy, pub NodeOutput);

impl InputNode {
    pub fn new(ty: PrimTy, sym: Symbol) -> Self {
        Self(ty, NodeOutput {
            ty,
            sym,
            kind: NetKind::Wire,
        })
    }
}

impl From<InputNode> for Node {
    fn from(node: InputNode) -> Self {
        Self::Input(node)
    }
}

impl IsNode for InputNode {
    fn node_output(&self, out: u8) -> &NodeOutput {
        match out {
            0 => &self.1,
            _ => unreachable!(),
        }
    }

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput {
        match out {
            0 => &mut self.1,
            _ => unreachable!(),
        }
    }

    fn inputs(&self) -> impl Iterator<Item = crate::index::NodeIndex> {
        [].into_iter()
    }
}

impl<I: Index, A: IsPrimTy> Component<I> for Input<A> {
    type Node = InputNode;
    type Outputs = (A,);

    fn into_node(self, (sym,): <Self::Outputs as Outputs<I>>::Symbols) -> Self::Node {
        InputNode::new(A::prim_ty(), sym)
    }
}
