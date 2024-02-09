use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct Pass {
    input: NodeOutIdx,
    output: NodeOutput,
}

impl Pass {
    pub fn new(ty: NodeTy, input: NodeOutId, sym: Option<Symbol>) -> Self {
        Self {
            input: input.into(),
            output: NodeOutput::wire(ty, sym),
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ Pass> {
    pub fn input(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.input)
    }
}

impl From<Pass> for NodeKind {
    fn from(node: Pass) -> Self {
        Self::Pass(node)
    }
}

impl IsNode for Pass {
    type Inputs = NodeOutIdx;
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.input
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }

    fn assert(&self, module_id: ModuleId, net_list: &NetList) {
        let node = WithId::<ModuleId, _>::new(module_id, self);
        let input = &net_list[node.input()];
        assert_eq!(self.output.width(), input.width());
    }
}
