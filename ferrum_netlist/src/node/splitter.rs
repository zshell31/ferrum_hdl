use std::fmt::Debug;

use ferrum::prim_ty::PrimTy;

use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, net_list::NodeOutId, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Splitter {
    pub input: NodeOutId,
    pub start: u128,
    pub width: u128,
    pub output: NodeOutput,
}

impl Splitter {
    pub fn new(
        ty: PrimTy,
        input: NodeOutId,
        start: u128,
        width: u128,
        sym: Symbol,
    ) -> Self {
        assert!(width > 0);
        // TODO: check that start + width < input.width()
        assert_eq!(ty.width(), width);
        Self {
            input,
            start,
            width,
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<Splitter> for Node {
    fn from(node: Splitter) -> Self {
        Self::Splitter(node)
    }
}

impl IsNode for Splitter {
    type Inputs = NodeOutId;
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
