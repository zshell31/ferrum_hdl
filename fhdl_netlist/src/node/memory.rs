use std::{num::NonZeroU128, rc::Rc};

use fhdl_data_structures::graph::NodeId;

use super::{IsNode, MakeNode, NodeOutput};
use crate::{const_val::ConstVal, netlist::Module, node_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Memory {
    pub dim: NonZeroU128,
    pub output: [NodeOutput; 1],
    pub name: Option<Symbol>,
    pub gen_i: Option<Symbol>,
    pub init: Rc<Vec<(u128, ConstVal)>>,
}

pub struct MemoryArgs<V> {
    pub ty: NodeTy,
    pub dim: NonZeroU128,
    pub init: V,
    pub name: Option<Symbol>,
    pub data_sym: Option<Symbol>,
}

impl<V> MakeNode<MemoryArgs<V>> for Memory
where
    V: IntoIterator<Item = (u128, ConstVal)>,
{
    fn make(module: &mut Module, args: MemoryArgs<V>) -> NodeId {
        let MemoryArgs {
            ty,
            dim,
            init,
            name,
            data_sym,
        } = args;
        assert!(ty.width() != 0);

        let init = init
            .into_iter()
            .take(dim.get() as usize)
            .map(|val| {
                assert_eq!(val.1.width(), ty.width());

                (val.0 % dim.get(), val.1)
            })
            .collect::<Vec<_>>();

        module.add_node(Memory {
            dim,
            output: [NodeOutput::reg(ty, data_sym); 1],
            name,
            gen_i: None,
            init: Rc::new(init),
        })
    }
}

impl IsNode for Memory {
    fn in_count(&self) -> usize {
        1
    }

    fn outputs(&self) -> &[NodeOutput] {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.output
    }
}
