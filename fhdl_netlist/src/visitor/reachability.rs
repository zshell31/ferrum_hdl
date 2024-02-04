use rustc_hash::FxHashSet;

use super::Visitor;
use crate::{
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::NodeKindWithId,
};

pub struct Reachability<'n> {
    net_list: &'n mut NetList,
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
            net_list,
            node_out_ids: Vec::with_capacity(16),
            modules,
        }
    }

    pub fn run(&mut self) {
        self.visit_modules()
    }
}

impl<'n> Visitor for Reachability<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.net_list.modules() {
            if self.modules.contains(&module_id) {
                self.visit_module(module_id);
            }
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        self.node_out_ids.clear();

        self.node_out_ids
            .extend(self.net_list.mod_outputs(module_id));

        while let Some(node_out_id) = self.node_out_ids.pop() {
            if !self.net_list[node_out_id].is_skip {
                continue;
            }

            self.net_list[node_out_id].is_skip = false;
            self.net_list[node_out_id.node_id().module_id()].is_skip = false;
            let node_id = node_out_id.node_id();
            let node = &mut self.net_list[node_id];
            node.is_skip = false;

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
