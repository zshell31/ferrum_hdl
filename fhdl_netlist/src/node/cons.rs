use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutIdx, node_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Const {
    value: u128,
    output: NodeOutput,
}

impl Const {
    pub fn new(ty: NodeTy, value: u128, sym: Option<Symbol>) -> Self {
        Self {
            value,
            output: NodeOutput::wire(ty, sym),
        }
    }

    pub fn value(&self) -> u128 {
        self.value
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl From<Const> for NodeKind {
    fn from(node: Const) -> Self {
        Self::Const(node)
    }
}

impl IsNode for Const {
    type Inputs = [NodeOutIdx];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &[]
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut []
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

#[derive(Debug, Clone)]
pub struct MultiConst {
    values: SmallVec<[u128; 1]>,
    outputs: SmallVec<[NodeOutput; 1]>,
}

impl MultiConst {
    pub fn new(
        values: impl IntoIterator<Item = u128>,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) -> Self {
        Self {
            values: values.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
        }
    }

    pub fn values(&self) -> &[u128] {
        &self.values
    }

    pub fn outputs(&self) -> &[NodeOutput] {
        &self.outputs
    }
}

impl From<MultiConst> for NodeKind {
    fn from(node: MultiConst) -> Self {
        Self::MultiConst(node)
    }
}

impl IsNode for MultiConst {
    type Inputs = [NodeOutIdx];
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        &[]
    }
    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut []
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs.as_slice()
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs.as_mut_slice()
    }
}
