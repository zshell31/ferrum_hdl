use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct Mux2 {
    inputs: [NodeOutIdx; 3],
    output: NodeOutput,
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
            inputs: [sel.into(), input1.into(), input2.into()],
            output: NodeOutput::wire(ty, sym.into()),
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ Mux2> {
    pub fn inputs(&self) -> Mux2Inputs {
        let module_id = self.id();
        Mux2Inputs {
            sel: NodeOutId::make(module_id, self.inputs[0]),
            input1: NodeOutId::make(module_id, self.inputs[1]),
            input2: NodeOutId::make(module_id, self.inputs[2]),
        }
    }
}

impl From<Mux2> for NodeKind {
    fn from(node: Mux2) -> Self {
        Self::Mux2(node)
    }
}

impl IsNode for Mux2 {
    type Inputs = [NodeOutIdx];
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

    fn assert(&self, module_id: ModuleId, net_list: &NetList) {
        let node = WithId::<ModuleId, _>::new(module_id, self);
        let inputs = node.inputs();

        let input1 = &net_list[inputs.input1];
        let input2 = &net_list[inputs.input2];

        assert_eq!(self.output.width(), input1.width());
        assert_eq!(self.output.width(), input2.width());
    }
}
