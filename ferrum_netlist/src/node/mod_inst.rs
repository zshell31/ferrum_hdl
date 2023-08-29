use ferrum::prim_ty::PrimTy;

use super::{IsNode, Node, NodeOutput};
use crate::{
    net_kind::NetKind,
    net_list::{ModuleId, NodeOutId},
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct ModInst {
    pub name: Symbol,
    pub module_id: ModuleId,
    pub inputs: Vec<NodeOutId>,
    pub outputs: Vec<NodeOutput>,
}

impl ModInst {
    pub fn new(
        name: Symbol,
        module_id: ModuleId,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = (PrimTy, Symbol)>,
    ) -> Self {
        Self {
            name,
            module_id,
            inputs: inputs.into_iter().collect(),
            outputs: outputs
                .into_iter()
                .map(|(ty, sym)| NodeOutput {
                    ty,
                    sym,
                    kind: NetKind::Wire,
                })
                .collect(),
        }
    }
}

impl From<ModInst> for Node {
    fn from(node: ModInst) -> Self {
        Self::ModInst(node)
    }
}

impl IsNode for ModInst {
    type Inputs = Vec<NodeOutId>;
    type Outputs = Vec<NodeOutput>;

    fn inputs(&self) -> &Self::Inputs {
        &self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.outputs
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.outputs
    }
}
