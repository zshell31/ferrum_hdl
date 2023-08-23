use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{
    module::ModuleId, net_kind::NetKind, net_list::NodeId, output::NodeOutput,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct ModInst {
    pub name: Symbol,
    pub module_id: ModuleId,
    pub inputs: Vec<NodeId>,
    pub out: NodeOutput,
}

impl ModInst {
    pub fn new(
        ty: PrimTy,
        name: Symbol,
        module_id: ModuleId,
        inputs: Vec<NodeId>,
        sym: Symbol,
    ) -> Self {
        Self {
            name,
            module_id,
            inputs,
            out: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<ModInst> for Node {
    fn from(node: ModInst) -> Self {
        Self::ModInst(node)
    }
}

impl IsNode for ModInst {
    type Outputs = (DummyTy,);

    fn node_output(&self, out: u8) -> &NodeOutput {
        match out {
            0 => &self.out,
            _ => unreachable!(),
        }
    }

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput {
        match out {
            0 => &mut self.out,
            _ => unreachable!(),
        }
    }

    fn inputs(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.inputs.iter().copied()
    }
}
