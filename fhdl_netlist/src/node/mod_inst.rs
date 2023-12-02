use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct ModInst {
    pub name: Option<Symbol>,
    pub inlined: bool,
    pub module_id: ModuleId,
    pub inputs: Vec<NodeOutId>,
    pub outputs: Vec<NodeOutput>,
}

impl ModInst {
    pub fn new(
        name: Option<Symbol>,
        module_id: ModuleId,
        inlined: bool,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = (NodeTy, Option<Symbol>)>,
    ) -> Self {
        Self {
            name,
            inlined,
            module_id,
            inputs: inputs.into_iter().collect(),
            outputs: outputs
                .into_iter()
                .map(|(ty, sym)| NodeOutput::wire(ty, sym))
                .collect(),
        }
    }
}

impl From<ModInst> for NodeKind {
    fn from(node: ModInst) -> Self {
        Self::ModInst(node)
    }
}

impl IsNode for ModInst {
    type Inputs = [NodeOutId];
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        self.inputs.as_slice()
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        self.inputs.as_mut_slice()
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs.as_slice()
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs.as_mut_slice()
    }

    fn validate(&self, net_list: &NetList) {
        assert_eq!(self.inputs.len(), net_list[self.module_id].inputs_len());
        assert_eq!(self.outputs.len(), net_list[self.module_id].outputs_len());
    }
}
