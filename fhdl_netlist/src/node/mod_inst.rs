use smallvec::SmallVec;

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{
    cursor::Cursor,
    netlist::{Module, ModuleId, NodeId, Port, WithId},
    symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModInst {
    pub mod_id: ModuleId,
    pub name: Option<Symbol>,
    pub inputs: u32,
    pub outputs: SmallVec<[NodeOutput; 1]>,
    pub inline: bool,
}

pub struct ModInstArgs<'m, I, O> {
    pub module: WithId<ModuleId, &'m Module>,
    pub inputs: I,
    pub outputs: O,
}

impl<'m, I, O> MakeNode<ModInstArgs<'m, I, O>> for ModInst
where
    I: IntoIterator<Item = Port>,
    O: IntoIterator<Item = Option<Symbol>>,
{
    fn make(module: &mut Module, args: ModInstArgs<I, O>) -> NodeId {
        let mod_inputs = args.module.mod_inputs();
        let mod_outputs = args.module.mod_outputs();

        let arg_outputs = args.outputs.into_iter();
        let mut outputs = SmallVec::with_capacity(arg_outputs.size_hint().0);
        for sym in arg_outputs {
            let mod_output = args.module[mod_outputs[outputs.len()]];

            let ty = mod_output.ty;
            let sym = sym.or(mod_output.sym);
            outputs.push(NodeOutput::wire(ty, sym));
        }
        assert_eq!(outputs.len(), mod_outputs.len());

        let node_id = module.add_node(ModInst {
            mod_id: args.module.id,
            name: None,
            inputs: 0,
            outputs,
            inline: false,
        });

        let mut inputs = 0;
        for input in args.inputs {
            let ty = module[input].ty;
            let mod_in = args.module[mod_inputs[inputs as usize]];
            // TODO: how to handle signed types
            assert_eq!(ty.width(), mod_in.ty.width());

            module.add_edge(input, Port::new(node_id, inputs));
            inputs += 1;
        }
        assert_eq!(inputs as usize, mod_inputs.len());

        if let NodeKind::ModInst(mod_inst) = module[node_id].kind_mut() {
            mod_inst.inputs = inputs;
        }

        node_id
    }
}

impl IsNode for ModInst {
    #[inline]
    fn in_count(&self) -> usize {
        self.inputs as usize
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

impl ModInst {
    #[inline]
    pub fn has_ports(&self) -> bool {
        self.has_inputs() && self.has_outputs()
    }

    #[inline]
    pub fn has_inputs(&self) -> bool {
        self.in_count() != 0
    }

    #[inline]
    pub fn has_outputs(&self) -> bool {
        self.out_count() != 0
    }
}

impl WithId<NodeId, &'_ ModInst> {
    pub fn inputs<'m>(&self, module: &'m Module) -> impl Iterator<Item = Port> + 'm {
        module.incoming(self.id).into_iter_(module)
    }
}
