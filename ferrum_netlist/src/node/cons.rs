use super::{IsNode, NodeKind, NodeOutput};
use crate::{sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Const {
    pub value: u128,
    pub output: NodeOutput,
}

impl Const {
    pub fn new(ty: PrimTy, value: u128, sym: Symbol) -> Self {
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
    type Inputs = ();
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &()
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

#[derive(Debug)]
pub struct MultiConst {
    pub values: &'static [u128],
    pub outputs: &'static mut [NodeOutput],
}

impl MultiConst {
    pub fn new(values: &'static [u128], outputs: &'static mut [NodeOutput]) -> Self {
        Self { values, outputs }
    }
}

impl From<MultiConst> for NodeKind {
    fn from(node: MultiConst) -> Self {
        Self::MultiConst(node)
    }
}

impl IsNode for MultiConst {
    type Inputs = ();
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        &()
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs
    }
}
