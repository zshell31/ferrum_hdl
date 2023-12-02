use std::{fmt::Debug, ops::Index};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{arena::Vec, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Splitter {
    pub input: NodeOutId,
    pub outputs: Vec<NodeOutput>,
    pub start: Option<u128>,
    pub rev: bool,
}

impl Splitter {
    pub fn new(
        input: NodeOutId,
        outputs: impl IntoIterator<Item = (PrimTy, Symbol)>,
        start: Option<u128>,
        rev: bool,
    ) -> Self {
        Self {
            input,
            outputs: Vec::collect_from(
                outputs
                    .into_iter()
                    .map(|(ty, sym)| NodeOutput::wire(ty, sym)),
            ),
            start,
            rev,
        }
    }

    pub fn start<T: Index<NodeOutId, Output = NodeOutput>>(&self, net_list: &T) -> u128 {
        let input = net_list[self.input];
        let input_width = input.ty.width();
        self.start
            .unwrap_or(if !self.rev { 0 } else { input_width })
    }
}

impl From<Splitter> for NodeKind {
    fn from(node: Splitter) -> Self {
        Self::Splitter(node)
    }
}

impl IsNode for Splitter {
    type Inputs = NodeOutId;
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        &self.input
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs.as_slice()
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs.as_mut_slice()
    }
}
