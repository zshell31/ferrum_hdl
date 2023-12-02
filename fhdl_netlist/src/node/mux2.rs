use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{encoding::Wrap, net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct Mux2 {
    inputs: Wrap<[NodeOutId; 3]>,
    pub output: NodeOutput,
}

#[derive(Debug, Clone, Copy)]
pub struct Mux2Inputs {
    pub sel: NodeOutId,
    pub input1: NodeOutId,
    pub input2: NodeOutId,
}

impl Mux2 {
    pub fn new(
        ty: NodeTy,
        sel: NodeOutId,
        input1: NodeOutId,
        input2: NodeOutId,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        Self {
            inputs: [sel, input1, input2].into(),
            output: NodeOutput::wire(ty, sym.into()),
        }
    }

    pub fn mux2_inputs(&self) -> Mux2Inputs {
        Mux2Inputs {
            sel: self.inputs[0],
            input1: self.inputs[1],
            input2: self.inputs[2],
        }
    }
}

impl From<Mux2> for NodeKind {
    fn from(node: Mux2) -> Self {
        Self::Mux2(node)
    }
}

impl IsNode for Mux2 {
    type Inputs = [NodeOutId];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &*self.inputs
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut *self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
