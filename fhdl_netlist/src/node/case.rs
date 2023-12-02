use std::{fmt::Debug, iter};

use either::Either;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    arena::Vec, bvm::BitVecMask, net_list::NodeOutId, params::Inputs, sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Case {
    pub inputs: CaseInputs,
    pub output: NodeOutput,
}

impl Case {
    pub fn new(
        ty: PrimTy,
        sel: NodeOutId,
        variants: impl IntoIterator<Item = (BitVecMask, NodeOutId)>,
        default: Option<NodeOutId>,
        sym: Option<Symbol>,
    ) -> Self {
        Self {
            inputs: CaseInputs {
                sel,
                variants: Vec::collect_from(variants),
                default,
            },
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Case> for NodeKind {
    fn from(node: Case) -> Self {
        Self::Case(node)
    }
}

impl IsNode for Case {
    type Inputs = CaseInputs;
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

#[derive(Debug, Clone)]
pub struct CaseInputs {
    pub sel: NodeOutId,
    pub variants: Vec<(BitVecMask, NodeOutId)>,
    pub default: Option<NodeOutId>,
}

impl Inputs for CaseInputs {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        let outs = iter::once(self.sel)
            .chain(self.variants.iter().map(|(_, node_out_id)| *node_out_id));

        match self.default {
            Some(node_out_id) => Either::Left(outs.chain(iter::once(node_out_id))),
            None => Either::Right(outs),
        }
    }

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        let outs = iter::once(&mut self.sel)
            .chain(self.variants.iter_mut().map(|(_, node_out_id)| node_out_id));

        match &mut self.default {
            Some(node_out_id) => Either::Left(outs.chain(iter::once(node_out_id))),
            None => Either::Right(outs),
        }
    }

    fn len(&self) -> usize {
        let len = self.variants.len() + 1;
        match self.default {
            Some(_) => len + 1,
            None => len,
        }
    }
}
