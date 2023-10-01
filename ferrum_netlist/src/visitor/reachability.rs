use fnv::FnvHashSet;

use super::Visitor;
use crate::{
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{ModInst, NodeKind},
    params::Inputs,
};

pub struct Reachability<'n> {
    net_list: &'n mut NetList,
    node_out_ids: Vec<NodeOutId>,
    modules: FnvHashSet<ModuleId>,
}

impl<'n> Reachability<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        let mut modules = FnvHashSet::default();
        // the first module is top module
        if let Some(first) = net_list.modules().next() {
            modules.insert(first);
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

        self.node_out_ids.extend(self.net_list[module_id].outputs());

        while let Some(node_out_id) = self.node_out_ids.pop() {
            if !self.net_list[node_out_id].is_skip {
                continue;
            }

            self.net_list[node_out_id].is_skip = false;
            self.net_list[node_out_id.node_id().module_id()].is_skip = false;
            let node_id = node_out_id.node_id();
            let node = &mut self.net_list[node_id];
            node.is_skip = false;

            if let NodeKind::ModInst(ModInst { module_id, .. }) = node.kind {
                self.modules.insert(module_id);
            }

            self.node_out_ids.extend(node.inputs().items());
        }
    }

    fn visit_node(&mut self, _node_id: NodeId) {
        unreachable!()
    }
}
