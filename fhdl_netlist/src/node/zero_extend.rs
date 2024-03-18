use super::{IsNode, MakeNode, NodeOutput};
use crate::{
    netlist::{Cursor, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ZeroExtend {
    pub output: [NodeOutput; 1],
}

#[derive(Debug)]
pub struct ZeroExtendArgs {
    pub ty: NodeTy,
    pub input: Port,
    pub sym: Option<Symbol>,
}

impl MakeNode<ZeroExtendArgs> for ZeroExtend {
    fn make(module: &mut Module, args: ZeroExtendArgs) -> NodeId {
        let width_in = module[args.input].width();
        let width_out = args.ty.width();
        if width_in > width_out {
            panic!("ZeroExtend: output width {width_out} < input width {width_in}",);
        }

        let node_id = module.add_node(ZeroExtend {
            output: [NodeOutput::wire(args.ty, args.sym)],
        });

        module.add_edge(args.input, Port::new(node_id, 0));

        node_id
    }
}

impl IsNode for ZeroExtend {
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

impl WithId<NodeId, &'_ ZeroExtend> {
    pub fn input(&self, module: &Module) -> Port {
        let mut incoming = module.incoming(self.id);
        incoming.next(module).unwrap()
    }
}
