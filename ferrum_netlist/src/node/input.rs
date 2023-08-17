use std::fmt::Debug;

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{index::Index, net_kind::NetKind, output::NodeOutput, symbol::Symbol};

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

impl<I: Index> IsNode<I> for InputNode {
    type Outputs = (DummyTy,);

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
