use super::{IsNode, MakeNode, NodeOutput};
use crate::{
    netlist::{Cursor, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct Pass {
    pub output: [NodeOutput; 1],
}

#[derive(Debug)]
pub struct PassArgs {
    pub input: Port,
    pub sym: Option<Symbol>,
    pub ty: Option<NodeTy>,
}

impl MakeNode<PassArgs> for Pass {
    fn make(module: &mut Module, args: PassArgs) -> NodeId {
        let input_ty = module[args.input].ty;
        let ty = match args.ty {
            Some(ty) => {
                assert_eq!(ty.width(), input_ty.width());
                ty
            }
            None => input_ty,
        };

        let node_id = module.add_node(Pass {
            output: [NodeOutput::wire(ty, args.sym)],
        });

        module.add_edge(args.input, Port::new(node_id, 0));

        node_id
    }
}

impl IsNode for Pass {
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

impl WithId<NodeId, &'_ Pass> {
    pub fn input(&self, module: &Module) -> Port {
        let mut incoming = module.incoming(self.id);
        incoming.next(module).unwrap()
    }
}
