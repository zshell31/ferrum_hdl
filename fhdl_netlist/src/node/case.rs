use std::fmt::Debug;

use rustc_macros::{Decodable, Encodable};

use super::{assert_width, IsNode, NodeKind, NodeOutput};
use crate::{
    bvm::BitVecMask,
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct Case {
    inputs: Vec<NodeOutIdx>,
    masks: Vec<BitVecMask>,
    is_default: bool,
    output: NodeOutput,
}

pub type VariantInputs<'n> = impl Iterator<Item = NodeOutId> + 'n;

pub struct CaseInputs<'n> {
    pub sel: NodeOutId,
    pub default: Option<NodeOutId>,
    pub variant_inputs: VariantInputs<'n>,
    pub masks: &'n [BitVecMask],
}

impl Case {
    pub fn new(
        ty: NodeTy,
        sel: NodeOutId,
        variants: impl IntoIterator<Item = (BitVecMask, NodeOutId)>,
        default: Option<NodeOutId>,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        let variants = variants.into_iter();
        let (size_hint, _) = variants.size_hint();
        let mut mask = Vec::with_capacity(size_hint);
        let mut inputs = Vec::with_capacity(size_hint + 2);

        inputs.push(sel.into());
        let is_default = default.is_some();
        if let Some(default) = default {
            inputs.push(default.into());
        }

        for (variant, input) in variants {
            mask.push(variant);
            inputs.push(input.into());
        }

        Self {
            inputs,
            masks: mask,
            is_default,
            output: NodeOutput::wire(ty, sym.into()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.masks.is_empty()
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ Case> {
    pub fn inputs(&self) -> CaseInputs<'_> {
        let module_id = self.id();

        let (default, offset) = if self.is_default {
            (Some(self.inputs[1]), 2)
        } else {
            (None, 1)
        };

        CaseInputs {
            sel: NodeOutId::make(module_id, self.inputs[0]),
            default: default.map(|default| NodeOutId::make(module_id, default)),
            variant_inputs: self.inputs[offset ..]
                .iter()
                .map(move |input| NodeOutId::make(module_id, *input)),
            masks: self.masks.as_slice(),
        }
    }
}

impl<R: Resolver> Resolve<R> for Case {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            inputs: self.inputs.clone(),
            masks: self.masks.resolve(resolver)?,
            is_default: self.is_default,
            output: self.output.resolve(resolver)?,
        })
    }
}

impl From<Case> for NodeKind {
    fn from(node: Case) -> Self {
        Self::Case(node)
    }
}

impl IsNode for Case {
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
        let CaseInputs { variant_inputs, .. } = node.inputs();
        for input in variant_inputs {
            assert_width!(self.output.width(), net_list[input].width());
        }
    }
}
