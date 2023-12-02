use smallvec::SmallVec;

use crate::{
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{IsNode, Node, NodeKind, Pass, DFF},
    params::Outputs,
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
        match link.kind {
            NodeKind::DFF(DFF { ref inputs, .. }) => {
                inputs.rst_val == link_out_id || inputs.data == link_out_id
            }
            _ => false,
        }
    }

    fn is_out_node(&self, node_id: NodeId) -> bool {
        let module = &self.net_list[node_id.module_id()];
        module.is_node_output(node_id)
    }

    pub fn maybe_to_inject(node: &Node) -> bool {
        node.kind.is_const()
            || node.kind.is_pass()
            || node.kind.is_expr()
            || node.kind.is_splitter()
            || node.kind.is_merger()
    }

    fn inject_input_for_pass(&mut self, node_id: NodeId) {
        let mut next = &self.net_list[node_id];
        let mut input_id = None;
        while let NodeKind::Pass(Pass { ref input, .. }) = next.kind {
            input_id = Some(*input);
            next = &self.net_list[input.node_id()];
        }

        if let Some(input_id) = input_id {
            if Self::maybe_to_inject(&self.net_list[input_id.node_id()]) {
                let out = &mut self.net_list[input_id];
                out.inject = true;

                let node = &mut self.net_list[input_id.node_id()];
                if node.kind.outputs().items().all(|output| output.out.inject) {
                    node.inject = true;
                }
            }
        }
    }

    fn try_inject_if_not_linked_by_pass(
        &mut self,
        node_id: NodeId,
        check: impl Fn(&Self, &Node, NodeOutId) -> bool,
    ) {
        let node = &self.net_list[node_id];

        let mut inject_outs: SmallVec<[NodeOutId; 8]> = SmallVec::new();

        for node_out_id in node.kind.node_out_ids(node_id) {
            for (link, link_out_id) in self.net_list.not_pass_links(node_out_id) {
                if check(self, link, link_out_id) {
                    inject_outs.push(node_out_id);
                }
            }
        }

        let node = &mut self.net_list[node_id];

        for node_out_id in inject_outs {
            node.kind
                .outputs_mut()
                .by_ind_mut(node_out_id.out_id())
                .out
                .inject = true;
        }

        if node.kind.outputs().items().all(|output| output.out.inject) {
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
        let module = &mut self.net_list[module_id];

        for node_id in module.nodes() {
            let node = &self.net_list[node_id];
            if node.is_skip || !Self::maybe_to_inject(node) {
                continue;
            }

            self.visit_node(node_id);
        }
    }

    fn visit_node(&mut self, node_id: NodeId) {
        if self.net_list[node_id].kind.is_pass() {
            if !self.is_out_node(node_id) {
                let node = &mut self.net_list[node_id];
                node.kind.outputs_mut().only_one_mut().out.inject = true;
                node.inject = true;
            } else {
                self.inject_input_for_pass(node_id);
            }
            return;
        }

        let node = &self.net_list[node_id];
        if node.kind.is_const() || node.kind.is_expr() || node.kind.is_splitter() {
            self.try_inject_if_not_linked_by_pass(node_id, |this, link, link_out_id| {
                link.kind.is_expr()
                    || link.kind.is_mux()
                    || link.kind.is_merger()
                    || this.linked_by_dff(link, link_out_id)
            });
            return;
        }

        if node.kind.is_merger() {
            self.try_inject_if_not_linked_by_pass(node_id, |this, link, link_out_id| {
                link.kind.is_merger()
                    || link.kind.is_mux()
                    || this.linked_by_dff(link, link_out_id)
            });
        }
    }
}
