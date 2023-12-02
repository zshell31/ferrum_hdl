use super::{IsNode, NodeKind, NodeOutput};
use crate::{arena::Vec, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Const {
    pub value: u128,
    pub output: NodeOutput,
}

impl Const {
    pub fn new(ty: PrimTy, value: u128, sym: Option<Symbol>) -> Self {
        Self {
            value,
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Const> for NodeKind {
    fn from(node: Const) -> Self {
        Self::Const(node)
    }
}

impl IsNode for Const {
    type Inputs = [NodeOutId];
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
    pub values: Vec<u128>,
    pub outputs: Vec<NodeOutput>,
}

impl MultiConst {
    pub fn new(
        values: impl IntoIterator<Item = u128>,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) -> Self {
        Self {
            values: Vec::collect_from(values),
            outputs: Vec::collect_from(outputs),
        }
    }
}

impl From<MultiConst> for NodeKind {
    fn from(node: MultiConst) -> Self {
        Self::MultiConst(node)
    }
}

impl IsNode for MultiConst {
    type Inputs = [NodeOutId];
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
