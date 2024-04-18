use ferrum_hdl::domain::{Polarity, SyncKind};
use fhdl_data_structures::{
    cursor::Cursor,
    graph::{NodeId, Port},
};

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{netlist::Module, node_ty::NodeTy, symbol::Symbol, with_id::WithId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DFF {
    pub rst_kind: SyncKind,
    pub rst_pol: Polarity,
    pub has_rst: bool,
    pub has_en: bool,
    pub has_data: bool,
    pub inputs: u8,
    pub output: [NodeOutput; 1],
}

#[derive(Debug, Clone, Copy)]
pub enum TyOrData {
    Ty(NodeTy),
    Data(Port),
}

#[derive(Debug)]
pub struct DFFArgs {
    pub clk: Port,
    pub rst: Option<Port>,
    pub rst_kind: SyncKind,
    pub rst_pol: Polarity,
    pub en: Option<Port>,
    pub init: Port,
    pub data: TyOrData,
    pub sym: Option<Symbol>,
}

impl DFFArgs {
    fn assert(&self, module: &Module) {
        let init = &module[self.init];
        let ty = match self.data {
            TyOrData::Ty(ty) => ty,
            TyOrData::Data(data) => module[data].ty,
        };
        assert_eq!(init.ty, ty);
    }
}

impl DFF {
    pub fn set_data(module: &mut Module, node_id: NodeId, data: Port) {
        let data_ty = module[data].ty;

        let dff = match module[node_id].dff_mut() {
            Some(dff) => dff,
            None => panic!("Expected DFF node at {node_id:?}"),
        };

        assert!(!dff.has_data);
        assert_eq!(dff.output[0].ty, data_ty);

        let port = dff.inputs;
        dff.inputs += 1;
        dff.has_data = true;

        module.add_edge(data, Port::new(node_id, port as u32));
    }
}

impl MakeNode<DFFArgs> for DFF {
    fn make(module: &mut Module, args: DFFArgs) -> NodeId {
        args.assert(module);

        let DFFArgs {
            clk,
            rst,
            rst_kind,
            rst_pol,
            en,
            init,
            data,
            sym,
        } = args;

        let (ty, has_data) = match data {
            TyOrData::Ty(ty) => (ty, false),
            TyOrData::Data(data) => (module[data].ty, true),
        };

        let node_id = module.add_node(DFF {
            rst_kind,
            rst_pol,
            has_rst: rst.is_some(),
            has_en: en.is_some(),
            has_data,
            inputs: 0,
            output: [NodeOutput::reg(ty, sym)],
        });

        let mut port = 0;

        module.add_edge(clk, Port::new(node_id, port));
        port += 1;

        if let Some(rst) = rst {
            module.add_edge(rst, Port::new(node_id, port));
            port += 1;
        }

        if let Some(en) = en {
            module.add_edge(en, Port::new(node_id, port));
            port += 1;
        }

        module.add_edge(init, Port::new(node_id, port));
        port += 1;

        if let TyOrData::Data(data) = data {
            module.add_edge(data, Port::new(node_id, port));
            port += 1;
        }

        if let NodeKind::DFF(dff) = module[node_id].kind_mut() {
            dff.inputs = port as u8;
        }

        node_id
    }
}

impl IsNode for DFF {
    #[inline]
    fn in_count(&self) -> usize {
        assert!(self.has_data);
        self.inputs as usize
    }

    #[inline]
    fn out_count(&self) -> usize {
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

#[derive(Debug)]
pub struct DFFInputs {
    pub clk: Port,
    pub rst: Option<Port>,
    pub en: Option<Port>,
    pub init: Port,
    pub data: Port,
}

impl WithId<NodeId, &'_ DFF> {
    pub fn inputs(&self, module: &Module) -> DFFInputs {
        assert!(self.has_data);
        let mut incoming = module.incoming(self.id);

        DFFInputs {
            clk: incoming.next_(module).unwrap(),
            rst: if self.has_rst {
                Some(incoming.next_(module).unwrap())
            } else {
                None
            },
            en: if self.has_en {
                Some(incoming.next_(module).unwrap())
            } else {
                None
            },
            init: incoming.next_(module).unwrap(),
            data: incoming.next_(module).unwrap(),
        }
    }
}
