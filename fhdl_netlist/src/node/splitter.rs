use std::{fmt::Debug, ops::Index};

use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{NetList, NodeOutId},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Splitter {
    pub input: NodeOutId,
    pub outputs: SmallVec<[NodeOutput; 8]>,
    pub start: Option<u128>,
    pub rev: bool,
}

impl Splitter {
    pub fn new(
        input: NodeOutId,
        outputs: impl IntoIterator<Item = (NodeTy, impl Into<Option<Symbol>>)>,
        start: Option<u128>,
        rev: bool,
    ) -> Self {
        Self {
            input,
            outputs: outputs
                .into_iter()
                .map(|(ty, sym)| NodeOutput::wire(ty, sym.into()))
                .collect(),
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

    pub fn pass_all_bits(&self, net_list: &NetList) -> bool {
        if self.outputs.len() != 1 {
            return false;
        }

        let in_width = net_list[self.input].width();
        let out_width = self.outputs[0].ty.width();

        in_width == out_width
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

    fn validate(&self, net_list: &NetList) {
        let input_width = net_list[self.input].width();
        let output_width = self
            .outputs
            .iter()
            .map(|output| output.width())
            .sum::<u128>();
        if output_width > input_width {
            panic!(
                "Splitter: output width {} > input width {}",
                output_width, input_width
            );
        }
    }
}
