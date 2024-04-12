use fhdl_data_structures::{
    cursor::Cursor,
    graph::{NodeId, Port},
};

use super::{IsNode, MakeNode, NodeOutput};
use crate::{netlist::Module, node_ty::NodeTy, symbol::Symbol, with_id::WithId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BitNot {
    pub input: Port,
    pub output: [NodeOutput; 1],
}

#[derive(Debug)]
pub struct BitNotArgs {
    pub ty: NodeTy,
    pub input: Port,
    pub sym: Option<Symbol>,
}

impl BitNotArgs {
    fn assert(&self, module: &Module) {
        let input = &module[self.input];

        assert_eq!(input.width(), self.ty.width());
    }
}

impl MakeNode<BitNotArgs> for BitNot {
    fn make(module: &mut Module, args: BitNotArgs) -> NodeId {
        args.assert(module);

        let BitNotArgs { ty, input, sym } = args;

        let node_id = module.add_node(BitNot {
            input,
            output: [NodeOutput::wire(ty, sym)],
        });

        module.add_edge(input, Port::new(node_id, 0));

        node_id
    }
}

impl IsNode for BitNot {
    #[inline]
    fn in_count(&self) -> usize {
        1
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

impl WithId<NodeId, &'_ BitNot> {
    pub fn input(&self, module: &Module) -> Port {
        let mut incoming = module.incoming(self.id);
        incoming.next_(module).unwrap()
    }
}
