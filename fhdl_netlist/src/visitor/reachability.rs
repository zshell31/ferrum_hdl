use rustc_hash::FxHashSet;

use super::Visitor;
use crate::{
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::NodeKindWithId,
};

pub struct Reachability<'n> {
    netlist: &'n mut NetList,
    node_out_ids: Vec<NodeOutId>,
    modules: FxHashSet<ModuleId>,
}

impl<'n> Reachability<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        let mut modules = FxHashSet::default();
        if let Some(top) = net_list.top_module() {
            modules.insert(top);
        }

        Self {
            netlist: net_list,
            node_out_ids: Default::default(),
            modules,
        }
    }

    pub fn run(&mut self) {
        self.visit_modules()
    }
}

impl<'n> Visitor for Reachability<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.netlist.modules() {
            if self.modules.contains(&module_id) {
                self.visit_module(module_id);
            }
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        self.node_out_ids.clear();

        self.node_out_ids
            .extend(self.netlist.mod_outputs(module_id));

        while let Some(node_out_id) = self.node_out_ids.pop() {
            let node_out = &self.netlist[node_out_id];
            if !node_out.skip || node_out.ty.width() == 0 {
                continue;
            }

            let node_id = node_out_id.node_id();
            if let NodeKindWithId::ModInst(mod_inst) = self.netlist[node_id].kind() {
                if mod_inst.empty_ports() {
                    continue;
                }
            }

            let node_out = &mut self.netlist[node_out_id];
            node_out.skip = false;

            let mod_id = node_id.module_id();

            self.netlist[mod_id].skip = false;
            let node = &mut self.netlist[node_id];
            node.skip = false;

            if let NodeKindWithId::ModInst(mod_inst) = node.kind() {
                let module_id = mod_inst.module_id();
                self.modules.insert(module_id);
            }

            self.node_out_ids
                .extend(node.inputs().map(|input| input.into_inner()));
        }
    }

    fn visit_node(&mut self, _node_id: NodeId) {
        unreachable!()
    }
}
