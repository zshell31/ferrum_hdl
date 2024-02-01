use std::fmt::Debug;

use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Splitter {
    input: NodeOutIdx,
    outputs: SmallVec<[NodeOutput; 1]>,
    start: Option<u128>,
    rev: bool,
}

impl Splitter {
    pub fn new(
        input: NodeOutId,
        outputs: impl IntoIterator<Item = (NodeTy, impl Into<Option<Symbol>>)>,
        start: Option<u128>,
        rev: bool,
    ) -> Self {
        Self {
            input: input.into(),
            outputs: outputs
                .into_iter()
                .map(|(ty, sym)| NodeOutput::wire(ty, sym.into()))
                .collect(),
            start,
            rev,
        }
    }

    pub fn outputs(&self) -> &[NodeOutput] {
        &self.outputs
    }

    pub fn rev(&self) -> bool {
        self.rev
    }
}

pub type Indices<'a> = impl Iterator<Item = (&'a NodeOutput, u128)> + 'a;

impl WithId<ModuleId, &'_ Splitter> {
    pub fn input(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.input)
    }

    pub fn start(&self) -> Option<u128> {
        self.start
    }

    fn eval_start(&self, net_list: &NetList) -> (u128, bool) {
        let rev = self.rev;
        let input = net_list[self.input()];
        let width = input.ty.width();
        let start = match self.start {
            Some(start) => start,
            None => {
                if !rev {
                    0
                } else {
                    width
                }
            }
        };

        (start, rev)
    }

    pub fn eval_indices(&self, net_list: &NetList) -> Indices<'_> {
        let (mut start, rev) = self.eval_start(net_list);

        self.outputs().iter().map(move |output| {
            let width = output.width();

            if !rev {
                let res = (output, start);
                start += width;
                res
            } else {
                start -= width;
                (output, start)
            }
        })
    }

    pub fn pass_all_bits(&self, net_list: &NetList) -> bool {
        if self.outputs.len() != 1 {
            return false;
        }

        let in_width = net_list[self.input()].width();
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
    type Inputs = NodeOutIdx;
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

    fn assert(&self, module_id: ModuleId, net_list: &NetList) {
        assert!(
            !self.outputs.is_empty(),
            "Splitter should have at least one output"
        );

        let this = WithId::new(module_id, self);
        let (mut start, rev) = this.eval_start(net_list);
        let input = net_list[NodeOutId::make(module_id, self.input)].width();

        for output in self.outputs() {
            let output = output.width();

            if !rev {
                assert!(
                    start + output <= input,
                    "Invalid inputs/outputs for splitter"
                );
                start += output;
            } else {
                assert!(start >= output, "Invalid inputs/outputs for splitter");
                start -= output;
            }
        }
    }
}
