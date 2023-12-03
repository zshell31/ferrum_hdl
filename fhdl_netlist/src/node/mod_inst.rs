use rustc_macros::{Decodable, Encodable};

use super::{assert_width, IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct ModInst {
    name: Option<Symbol>,
    module_id: ModuleId,
    inputs: Vec<NodeOutIdx>,
    outputs: Vec<NodeOutput>,
}

impl ModInst {
    pub fn new(
        name: Option<Symbol>,
        module_id: ModuleId,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = (NodeTy, Option<Symbol>)>,
    ) -> Self {
        Self {
            name,
            module_id,
            inputs: inputs.into_iter().map(Into::into).collect(),
            outputs: outputs
                .into_iter()
                .map(|(ty, sym)| NodeOutput::wire(ty, sym))
                .collect(),
        }
    }

    pub fn name(&self) -> Option<Symbol> {
        self.name
    }

    pub fn set_name(&mut self, name: Option<Symbol>) {
        self.name = name;
    }

    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }

    pub fn outputs(&self) -> &[NodeOutput] {
        &self.outputs
    }

    pub fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.outputs
    }

    pub fn inputs_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn inputs_is_empty(&self) -> bool {
        self.inputs.is_empty()
    }

    pub fn outputs_len(&self) -> usize {
        self.outputs.len()
    }

    pub fn outputs_is_empty(&self) -> bool {
        self.outputs.is_empty()
    }
}

impl WithId<ModuleId, &'_ ModInst> {
    pub fn inputs(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        let module_id = self.id();
        self.inputs
            .iter()
            .map(move |input| NodeOutId::make(module_id, *input))
    }
}

impl<R: Resolver> Resolve<R> for ModInst {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            name: self.name,
            module_id: resolver.resolve_module_id(self.module_id)?,
            inputs: self.inputs.clone(),
            outputs: self.outputs.resolve(resolver)?,
        })
    }
}

impl From<ModInst> for NodeKind {
    fn from(node: ModInst) -> Self {
        Self::ModInst(node)
    }
}

impl IsNode for ModInst {
    type Inputs = [NodeOutIdx];
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

    fn assert(&self, target: ModuleId, net_list: &NetList) {
        let module_id = self.module_id;
        let module = &net_list[module_id];
        let node = WithId::<ModuleId, _>::new(target, self);

        assert_eq!(self.inputs.len(), module.inputs_len());
        for (input, mod_input) in node.inputs().zip(module.inputs()) {
            assert_width!(
                net_list[input].width(),
                net_list[mod_input].only_one_out().width()
            );
        }

        assert_eq!(self.outputs.len(), module.outputs_len());
        for (output, mod_output) in self.outputs().iter().zip(module.outputs()) {
            assert_width!(output.width(), net_list[mod_output].width());
        }
    }
}
