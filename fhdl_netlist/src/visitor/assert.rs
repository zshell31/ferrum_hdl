use crate::{
    net_list::{ModuleId, NetList, NodeId},
    visitor::Visitor,
};

pub struct Assert<'n> {
    netlist: &'n NetList,
}

impl<'n> Assert<'n> {
    pub fn new(net_list: &'n NetList) -> Self {
        Self { netlist: net_list }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }
}

impl<'n> Visitor for Assert<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.netlist.modules() {
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let mut cursor = self.netlist.mod_cursor(module_id);
        while let Some(node_id) = self.netlist.next(&mut cursor) {
            self.visit_node(node_id);
        }
    }

    fn visit_node(&mut self, node_id: NodeId) {
        let node = &self.netlist[node_id];
        node.assert(self.netlist);
    }
}
