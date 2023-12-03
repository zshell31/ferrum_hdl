use crate::{
    net_list::{ModuleId, NetList, NodeId},
    visitor::Visitor,
};

pub struct Assert<'n> {
    net_list: &'n NetList,
}

impl<'n> Assert<'n> {
    pub fn new(net_list: &'n NetList) -> Self {
        Self { net_list }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }
}

impl<'n> Visitor for Assert<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.net_list.modules() {
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let mut cursor = self.net_list.mod_cursor(module_id);
        while let Some(node_id) = self.net_list.next(&mut cursor) {
            self.visit_node(node_id);
        }
    }

    fn visit_node(&mut self, node_id: NodeId) {
        let node = &self.net_list[node_id];
        node.assert(self.net_list);
    }
}
