use fhdl_data_structures::graph::NodeId;
use smallvec::SmallVec;

use super::{IsNode, MakeNode, NodeOutput};
use crate::{const_val::ConstVal, netlist::Module, node_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Const {
    pub value: u128,
    pub output: [NodeOutput; 1],
}

pub struct ConstArgs {
    pub ty: NodeTy,
    pub value: u128,
    pub sym: Option<Symbol>,
}

impl ConstArgs {
    pub fn value(&self) -> ConstVal {
        ConstVal::new(self.value, self.ty.width())
    }
}

impl Const {
    pub fn new(args: ConstArgs) -> Self {
        let ConstArgs { ty, value, sym } = args;

        Self {
            value,
            output: [NodeOutput::wire(ty, sym)],
        }
    }

    fn from_multi_const(value: u128, output: [NodeOutput; 1]) -> Self {
        Self { value, output }
    }

    pub fn value(&self) -> ConstVal {
        ConstVal::new(self.value, self.output[0].width())
    }
}

impl MakeNode<ConstArgs> for Const {
    fn make(module: &mut Module, args: ConstArgs) -> NodeId {
        module.add_node(Self::new(args))
    }
}

impl IsNode for Const {
    #[inline]
    fn in_count(&self) -> usize {
        0
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        &self.output
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.output
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiConst {
    pub values: SmallVec<[u128; 1]>,
    pub outputs: SmallVec<[NodeOutput; 1]>,
}

impl MultiConst {
    pub fn value(&self, idx: usize) -> ConstVal {
        let value = self.values[idx];
        let output = &self.outputs[idx];

        ConstVal::new(value, output.width())
    }

    pub fn values(&self) -> impl Iterator<Item = ConstVal> + '_ {
        (0 .. self.out_count()).map(|idx| self.value(idx))
    }
}

impl MultiConst {
    fn new<O>(args: O) -> Self
    where
        O: IntoIterator<Item = ConstArgs>,
    {
        let args = args.into_iter();
        let size = args.size_hint().0;
        let mut values = SmallVec::with_capacity(size);
        let mut outputs = SmallVec::with_capacity(size);

        for arg in args {
            values.push(arg.value);
            outputs.push(NodeOutput::wire(arg.ty, arg.sym));
        }

        MultiConst { values, outputs }
    }

    pub fn val_outputs(&self) -> impl Iterator<Item = (u128, &NodeOutput)> + '_ {
        self.values.iter().copied().zip(self.outputs.iter())
    }
}

impl<O> MakeNode<O> for MultiConst
where
    O: IntoIterator<Item = ConstArgs>,
{
    fn make(module: &mut Module, args: O) -> NodeId {
        let node = MultiConst::new(args);
        if node.values.len() == 1 {
            module.add_node(Const::from_multi_const(
                node.values[0],
                node.outputs.into_inner().unwrap(),
            ))
        } else {
            module.add_node(node)
        }
    }
}

impl IsNode for MultiConst {
    #[inline]
    fn in_count(&self) -> usize {
        0
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        &self.outputs
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.outputs
    }
}
