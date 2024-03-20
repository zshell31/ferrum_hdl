use super::{IsNode, MakeNode, NodeOutput};
use crate::{
    netlist::{Cursor, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Extend {
    pub output: [NodeOutput; 1],
    pub is_sign: bool,
}

#[derive(Debug)]
pub struct ExtendArgs {
    pub ty: NodeTy,
    pub input: Port,
    pub sym: Option<Symbol>,
    pub is_sign: bool,
}

impl MakeNode<ExtendArgs> for Extend {
    fn make(module: &mut Module, args: ExtendArgs) -> NodeId {
        let width_in = module[args.input].width();
        let width_out = args.ty.width();
        if width_in > width_out {
            panic!("Extend: output width {width_out} < input width {width_in}",);
        }

        let node_id = module.add_node(Extend {
            output: [NodeOutput::wire(args.ty, args.sym)],
            is_sign: args.is_sign,
        });

        module.add_edge(args.input, Port::new(node_id, 0));

        node_id
    }
}

impl IsNode for Extend {
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

impl WithId<NodeId, &'_ Extend> {
    pub fn input(&self, module: &Module) -> Port {
        let mut incoming = module.incoming(self.id);
        incoming.next(module).unwrap()
    }
}
