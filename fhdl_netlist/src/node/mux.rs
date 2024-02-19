use std::{fmt::Debug, iter};

use either::Either;
use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    const_val::ConstVal,
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Mux {
    cases: SmallVec<[ConstVal; 2]>,
    inputs: SmallVec<[NodeOutIdx; 3]>,
    output: NodeOutput,
    has_default: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Case {
    Val(ConstVal),
    Default,
}

pub type Variants<'n> = impl Iterator<Item = (Case, NodeOutId)> + 'n;

pub struct MuxInputs<'n> {
    pub sel: NodeOutId,
    pub variants: Variants<'n>,
}

impl Mux {
    pub fn new(
        ty: NodeTy,
        sel: NodeOutId,
        variants: impl IntoIterator<Item = (ConstVal, NodeOutId)>,
        default: Option<NodeOutId>,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        let variants = variants.into_iter();
        let (size_hint, _) = variants.size_hint();
        let mut cases = SmallVec::with_capacity(size_hint);
        let mut inputs = SmallVec::with_capacity(size_hint + 1);

        inputs.push(sel.into());

        for (case, input) in variants {
            assert!(case.width() != 0);
            cases.push(case);
            inputs.push(input.into());
        }

        let has_default = default.is_some();
        if let Some(default) = default {
            cases.push(ConstVal::default());
            inputs.push(default.into());
        }

        Self {
            cases,
            inputs,
            output: NodeOutput::reg(ty, sym.into(), None),
            has_default,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.cases.is_empty()
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ Mux> {
    pub fn inputs(&self) -> MuxInputs<'_> {
        let module_id = self.id();

        let cases = if self.has_default {
            Either::Left(self.cases.iter().map(|case| {
                if !case.is_zero_sized() {
                    Case::Val(*case)
                } else {
                    Case::Default
                }
            }))
        } else {
            let cases = self.cases.len();
            Either::Right(
                self.cases[.. cases - 1]
                    .iter()
                    .map(|case| Case::Val(*case))
                    .chain(iter::once(Case::Default)),
            )
        };

        MuxInputs {
            sel: NodeOutId::make(module_id, self.inputs[0]),
            variants: cases.zip(
                self.inputs
                    .iter()
                    .skip(1)
                    .map(move |input| NodeOutId::make(module_id, *input)),
            ),
        }
    }
}

impl From<Mux> for NodeKind {
    fn from(node: Mux) -> Self {
        Self::Mux(node)
    }
}

impl IsNode for Mux {
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
        let MuxInputs { sel, variants } = node.inputs();

        let sel_width = net_list[sel].width();

        for (case, input) in variants {
            if let Case::Val(case) = case {
                assert_eq!(case.width(), sel_width);
            }
            assert_eq!(self.output.width(), net_list[input].width());
        }
    }
}
