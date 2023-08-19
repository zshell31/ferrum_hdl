use std::fmt::Debug;

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{net_kind::NetKind, net_list::NodeId, output::NodeOutput, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Splitter {
    pub input: NodeId,
    pub start: u8,
    pub width: u8,
    pub out: NodeOutput,
}

impl Splitter {
    pub fn new(ty: PrimTy, input: NodeId, start: u8, width: u8, sym: Symbol) -> Self {
        assert!(width > 0);
        // TODO: check that start + width < input.width()
        assert_eq!(ty.width(), width);
        Self {
            input,
            start,
            width,
            out: NodeOutput {
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

    fn inputs(&self) -> impl Iterator<Item = NodeId> {
        [self.input].into_iter()
    }
}
