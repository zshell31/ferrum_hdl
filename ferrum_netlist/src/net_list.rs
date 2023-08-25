use std::collections::BTreeSet;

use crate::{
    node::{InputNode, IsNode, Node, PassNode},
    output::{IsOneOutput, NodeOutput, Outputs},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(usize, u8);

#[derive(Debug, Default)]
pub struct NetList {
    pub inputs: BTreeSet<NodeId>,
    pub outputs: BTreeSet<NodeId>,
    nodes: Vec<Node>,
}

impl NetList {
    pub fn new() -> Self {
        Self {
            inputs: BTreeSet::new(),
            outputs: BTreeSet::new(),
            nodes: Vec::with_capacity(16),
        }
    }

    pub fn index(&mut self, out: u8) -> NodeId {
        NodeId(self.nodes.len(), out)
    }

    pub fn add_node<N: IsNode>(&mut self, node: N) -> NodeId
    where
        N::Outputs: IsOneOutput,
    {
        let outputs = <N::Outputs as Outputs>::make_node_ids(self);
        self.add_node_inner(node);

        outputs[0]
    }

    pub fn add_dummy_node(&mut self, node: InputNode) -> NodeId
    where
        <InputNode as IsNode>::Outputs: IsOneOutput,
    {
        let outputs = <<InputNode as IsNode>::Outputs as Outputs>::make_node_ids(self);
        self.add_node_inner(Node::DummyInput(node));

        outputs[0]
    }

    pub fn replace<N: IsNode>(&mut self, node_index: NodeId, node: N) {
        if self.node(node_index).is_input() {
            self.inputs.remove(&node_index);
        }
        let node: Node = node.into();
        if node.is_input() {
            self.inputs.insert(node_index);
        }

        self.nodes[node_index.0] = node;
    }

    fn add_node_inner(&mut self, node: impl Into<Node>) {
        let node: Node = node.into();
        if node.is_input() {
            let node_idx = self.index(0);
            self.inputs.insert(node_idx);
        }

        self.nodes.push(node);
    }

    pub fn add_output_node(&mut self, node_index: NodeId) {
        self.outputs.insert(node_index);
    }

    pub fn node(&self, node_index: NodeId) -> &Node {
        &self.nodes[node_index.0]
    }

    pub fn node_mut(&mut self, node_index: NodeId) -> &mut Node {
        &mut self.nodes[node_index.0]
    }

    pub fn node_output(&self, node_index: NodeId) -> &NodeOutput {
        let node = self.node(node_index);
        node.node_output(node_index.1)
    }

    pub fn node_output_mut(&mut self, node_index: NodeId) -> &mut NodeOutput {
        let node = self.node_mut(node_index);
        node.node_output_mut(node_index.1)
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }

    pub fn inputs(&self) -> impl Iterator<Item = &Node> {
        self.inputs.iter().map(|node_id| self.node(*node_id))
    }

    pub fn outputs(&self) -> impl Iterator<Item = &Node> {
        self.outputs.iter().map(|node_id| self.node(*node_id))
    }

    pub fn find_node(
        &self,
        node_index: NodeId,
        f: impl Fn(&Node) -> bool + Copy,
    ) -> Vec<NodeId> {
        let mut res = vec![];
        self.find_node_inner(node_index, f, &mut res);
        res
    }

    fn find_node_inner(
        &self,
        node_index: NodeId,
        f: impl Fn(&Node) -> bool + Copy,
        res: &mut Vec<NodeId>,
    ) {
        let node = self.node(node_index);
        if f(node) {
            res.push(node_index)
        }

        for input in node.inputs() {
            self.find_node_inner(input, f, res);
        }
    }

    pub fn find_dummy_inputs(&self, node_index: NodeId) -> Vec<NodeId> {
        self.find_node(node_index, |node| node.is_dummy_input())
    }

    pub fn link_dff(&mut self, dff: NodeId, comb: NodeId) {
        if let Node::DFF(dff) = self.node_mut(dff) {
            dff.data = Some(comb);
        }
    }

    pub fn link_dummy_input(&mut self, with_dummy: NodeId, to_link: &[NodeId]) {
        let dummy = self.find_dummy_inputs(with_dummy);
        assert_eq!(dummy.len(), to_link.len());
        for (dummy, to_link) in dummy.into_iter().zip(to_link.iter().copied()) {
            let dummy_out = self.node_output(dummy);

            self.replace(dummy, PassNode::new(dummy_out.ty, to_link, dummy_out.sym));
        }
    }
}
