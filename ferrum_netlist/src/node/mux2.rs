use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, params::Inputs, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Mux2 {
    pub inputs: Mux2Inputs,
    pub output: NodeOutput,
}

impl Mux2 {
    pub fn new(
        ty: PrimTy,
        sel: NodeOutId,
        input1: NodeOutId,
        input2: NodeOutId,
        sym: Symbol,
    ) -> Self {
        Self {
            inputs: Mux2Inputs {
                sel,
                input1,
                input2,
            },
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Mux2> for NodeKind {
    fn from(node: Mux2) -> Self {
        Self::Mux2(node)
    }
}

impl IsNode for Mux2 {
    type Inputs = Mux2Inputs;
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.inputs
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Mux2Inputs {
    pub sel: NodeOutId,
    pub input1: NodeOutId,
    pub input2: NodeOutId,
}

impl Inputs for Mux2Inputs {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [self.sel, self.input1, self.input2].into_iter()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        [&mut self.sel, &mut self.input1, &mut self.input2].into_iter()
    }

    fn len(&self) -> usize {
        3
    }
}
