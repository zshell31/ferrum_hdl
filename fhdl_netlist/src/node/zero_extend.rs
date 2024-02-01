use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct ZeroExtend {
    input: NodeOutIdx,
    output: NodeOutput,
}

impl ZeroExtend {
    pub fn new(ty: NodeTy, input: NodeOutId, sym: impl Into<Option<Symbol>>) -> Self {
        Self {
            input: input.into(),
            output: NodeOutput::wire(ty, sym.into()),
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ ZeroExtend> {
    pub fn input(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.input)
    }
}

impl From<ZeroExtend> for NodeKind {
    fn from(node: ZeroExtend) -> Self {
        Self::ZeroExtend(node)
    }
}

impl IsNode for ZeroExtend {
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
        let input = net_list[node.input()].width();
        let output = self.output.width();

        if input > output {
            panic!(
                "ZeroExtend: output width {} < input width {}",
                output, input
            );
        }
    }
}
