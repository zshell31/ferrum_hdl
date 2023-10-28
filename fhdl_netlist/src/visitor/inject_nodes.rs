use smallvec::SmallVec;

use crate::{
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{Node, NodeKind},
    visitor::Visitor,
};

pub struct InjectNodes<'n> {
    net_list: &'n mut NetList,
}

impl<'n> InjectNodes<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        Self { net_list }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }

    fn linked_by_dff(&self, link: &Node, link_out_id: NodeOutId) -> bool {
        match &*link.kind {
            NodeKind::DFF(node) => {
                let inputs = node.dff_inputs();
                inputs.rst_val == link_out_id
                    || inputs.data == link_out_id
                    || (inputs.en.is_some() && inputs.en.unwrap() == link_out_id)
            }
            _ => false,
        }
    }

    fn is_out_node(&self, node_id: NodeId) -> bool {
        self.net_list.is_out_node(node_id)
    }

    pub fn maybe_to_inject(node: &Node) -> bool {
        node.is_const()
            || node.is_pass()
            || node.is_expr()
            || node.is_splitter()
            || node.is_merger()
    }

    fn try_inject(
        &mut self,
        node_id: NodeId,
        check: impl Fn(&Self, NodeId, &Node, NodeOutId) -> bool,
    ) {
        let node = &self.net_list[node_id];

        let mut inject_outs: SmallVec<[NodeOutId; 8]> = SmallVec::new();

        for node_out_id in node.node_out_ids() {
            for (link_id, link) in self.net_list.links(node_out_id) {
                if !link.is_skip && check(self, link_id, link, node_out_id) {
                    inject_outs.push(node_out_id);
                }
            }
        }

        let node = &mut self.net_list[node_id];

        for node_out_id in inject_outs {
            node.output_by_ind_mut(node_out_id.out_id()).inject = true;
        }

        if node.outputs().all(|output| output.inject) {
            node.inject = true;
        }
    }
}

impl<'n> Visitor for InjectNodes<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.net_list.modules() {
            let module = &self.net_list[module_id];
            if module.is_skip {
                continue;
            }
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let mut cursor = self.net_list.mod_cursor(module_id);
        while let Some(node_id) = self.net_list.next(&mut cursor) {
            let node = &self.net_list[node_id];
            if node.is_skip || !Self::maybe_to_inject(node) {
                continue;
            }

            self.visit_node(node_id);
        }
    }

    fn visit_node(&mut self, node_id: NodeId) {
        let node = &self.net_list[node_id];

        if node.is_const() || node.is_expr() || node.is_splitter() {
            self.try_inject(node_id, |this, link_id, link, link_out_id| {
                (link.is_pass() && this.is_out_node(link_id))
                    || link.is_expr()
                    || link.is_mux()
                    || link.is_merger()
                    || link.is_mod_inst()
                    || this.linked_by_dff(link, link_out_id)
            });
            return;
        }

        if node.is_merger() || node.is_zero_extend() {
            self.try_inject(node_id, |this, link_id, link, link_out_id| {
                (link.is_pass() && this.is_out_node(link_id))
                    || link.is_merger()
                    || link.is_mux()
                    || link.is_mod_inst()
                    || this.linked_by_dff(link, link_out_id)
            });
        }
    }
}
