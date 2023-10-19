use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    arena::Vec,
    net_list::{ModuleId, NodeOutId},
    sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
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
        outputs: impl IntoIterator<Item = (PrimTy, Option<Symbol>)>,
    ) -> Self {
        Self {
            name,
            inlined,
            module_id,
            inputs: Vec::collect_from(inputs),
            outputs: Vec::collect_from(
                outputs
                    .into_iter()
                    .map(|(ty, sym)| NodeOutput::wire(ty, sym)),
            ),
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
}
