#![allow(clippy::ptr_arg)]
use std::{fmt::Debug, hash::BuildHasherDefault};

use indexmap::IndexSet;
use rustc_hash::FxHasher;

type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;

use super::{
    ident::{NodeId, NodeOutId},
    list::{List, ListStorage},
    ModuleId, NodeIdx, NodeOutIdx, Nodes,
};
use crate::{net_list::Idx, node::Node, symbol::Symbol};

struct NodeStorage<'n> {
    module_id: ModuleId,
    nodes: &'n mut Nodes,
}

impl<'n> ListStorage<NodeIdx> for NodeStorage<'n> {
    type Item = Node;

    fn item(&self, idx: NodeIdx) -> &Self::Item {
        let node_id = NodeId::make(self.module_id, idx);
        &self.nodes[node_id]
    }

    fn item_mut(&mut self, idx: NodeIdx) -> &mut Self::Item {
        let node_id = NodeId::make(self.module_id, idx);
        &mut self.nodes[node_id]
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: Symbol,
    pub is_skip: bool,
    pub is_inlined: bool,
    pub only_inputs: bool,
    module_id: ModuleId,
    list: List<NodeIdx>,
    inputs: FxIndexSet<NodeIdx>,
    outputs: Vec<NodeOutIdx>,
}

impl Module {
    pub(super) fn new(module_id: ModuleId, name: Symbol) -> Self {
        Self {
            module_id,
            name,
            is_skip: true,
            only_inputs: true,
            list: Default::default(),
            inputs: Default::default(),
            outputs: Default::default(),
            is_inlined: false,
        }
    }

    pub(super) fn next_node_id(&mut self) -> NodeId {
        NodeId::make(self.module_id, self.list.next_idx())
    }

    pub(super) fn shift_last_idx(&mut self, idx: usize) {
        self.list.shift_last_idx(idx);
    }

    pub(super) fn last_idx(&self) -> usize {
        self.list.last_idx()
    }

    #[inline]
    fn node_storage<'n>(&self, nodes: &'n mut Nodes) -> NodeStorage<'n> {
        NodeStorage {
            module_id: self.module_id,
            nodes,
        }
    }

    pub(super) fn add(&mut self, nodes: &mut Nodes, node_idx: NodeIdx) {
        self.list.add(&mut self.node_storage(nodes), node_idx);
        self.add_input(nodes, node_idx);
    }

    pub(super) fn remove(&mut self, nodes: &mut Nodes, node_idx: NodeIdx) {
        self.remove_input(node_idx);
        self.list.remove(&mut self.node_storage(nodes), node_idx);
    }

    pub(super) fn insert(
        &mut self,
        nodes: &mut Nodes,
        prev_node_idx: Option<NodeIdx>,
        node_idx: NodeIdx,
    ) {
        self.list
            .insert(&mut self.node_storage(nodes), prev_node_idx, node_idx);
        self.add_input(nodes, node_idx);
    }

    pub(super) fn replace(&mut self, nodes: &mut Nodes, node: &mut Node) {
        let node_id = node.node_id();
        assert_eq!(node_id.module_id(), self.module_id);

        let old_node = &nodes[node_id];

        if !(old_node.is_input() && node.is_input()) {
            self.remove_input(node_id.into());
            self.add_input(nodes, node_id.into());
        }

        self.list
            .replace(&mut self.node_storage(nodes), node_id.into(), node);
    }

    fn add_input(&mut self, nodes: &Nodes, node_idx: NodeIdx) {
        let node_id = NodeId::make(self.module_id, node_idx);
        if nodes[node_id].is_input() {
            self.inputs.insert(node_idx);
        } else {
            self.only_inputs = false;
        }
    }

    fn remove_input(&mut self, node_idx: NodeIdx) {
        self.inputs.shift_remove(&node_idx);
    }

    pub(super) fn is_input(&self, node_idx: NodeIdx) -> bool {
        self.inputs.contains(&node_idx)
    }

    pub(super) fn add_output(&mut self, node_out_idx: NodeOutIdx) {
        self.outputs.push(node_out_idx);
    }

    pub(super) fn is_output(&self, node_out_idx: NodeOutIdx) -> bool {
        self.outputs.contains(&node_out_idx)
    }

    pub fn inputs(&self) -> impl Iterator<Item = NodeId> + '_ {
        let module_id = self.module_id;
        self.inputs
            .iter()
            .map(move |input| NodeId::make(module_id, *input))
    }

    pub fn outputs(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        let module_id = self.module_id;
        self.outputs
            .iter()
            .map(move |output| NodeOutId::make(module_id, *output))
    }

    pub(super) fn output_by_ind(&self, ind: usize) -> NodeOutId {
        NodeOutId::make(self.module_id, self.outputs[ind])
    }

    pub(super) fn replace_output(
        &mut self,
        old_id: NodeOutId,
        new_id: NodeOutId,
    ) -> bool {
        let mut found = false;
        let old_idx = old_id.into();
        for output in &mut self.outputs {
            if *output == old_idx {
                *output = new_id.into();
                found = true;
            }
        }

        found
    }

    pub(super) fn head(&self) -> Option<NodeId> {
        self.list
            .head()
            .map(|head| NodeId::make(self.module_id, head))
    }

    pub(super) fn tail(&self) -> Option<NodeId> {
        self.list
            .tail()
            .map(|tail| NodeId::make(self.module_id, tail))
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn inputs_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn outputs_len(&self) -> usize {
        self.outputs.len()
    }

    pub(crate) fn dump(&self, module_id: ModuleId) {
        println!(
            "{} {} (is_skip {}, is_inlined {}, head {:?}, tail {:?}, last_idx: {})",
            module_id.idx(),
            self.name,
            self.is_skip,
            self.is_inlined,
            self.head(),
            self.tail(),
            self.last_idx()
        );
        println!(
            "inputs: {}",
            self.inputs
                .iter()
                .map(|input| format!("{}", input.idx()))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
        println!(
            "outputs: {}",
            self.outputs
                .iter()
                .map(|input| format!("{} ({})", input.node_idx().idx(), input.idx()))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
    }
}
