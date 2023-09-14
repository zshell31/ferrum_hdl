use crate::{
    net_list::{ModuleId, NetList, NodeId},
    node::{Node, PassNode},
    symbol::Symbol,
    visitor::Visitor,
};

pub struct InjectPass<'n> {
    net_list: &'n mut NetList,
}

impl<'n> InjectPass<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        Self { net_list }
    }

    pub fn inject(&mut self) {
        self.visit_modules();
    }

    pub fn input_sym(&self, node_id: NodeId) -> Option<Symbol> {
        let node = &self.net_list[node_id];
        match node {
            Node::Pass(PassNode { inject, input, .. }) if inject.is_none() => {
                match self.input_sym(input.node_id()) {
                    Some(sym) => Some(sym),
                    None => Some(self.net_list[*input].sym),
                }
            }
            _ => None,
        }
    }
}

impl<'n> Visitor for InjectPass<'n> {
    fn visit_modules(&mut self) {
        for module in self.net_list.modules() {
            self.visit_module(module);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let module = &mut self.net_list[module_id];

        for node in module.nodes() {
            self.visit_node(node);
        }
    }

    fn visit_node(&mut self, node_id: NodeId) {
        if let Some(sym) = self.input_sym(node_id) {
            let node = &mut self.net_list[node_id];
            if let Node::Pass(PassNode { inject, output, .. }) = node {
                *inject = Some(true);
                output.sym = sym;
            }
        }
    }
}
