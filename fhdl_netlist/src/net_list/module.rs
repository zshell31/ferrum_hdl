#![allow(clippy::ptr_arg)]
use fnv::FnvBuildHasher;
use indexmap::IndexSet;

use super::{
    ident::{NodeId, NodeOutId},
    ModuleId,
};
use crate::{node::Node, symbol::Symbol};

type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;

#[derive(Debug)]
pub struct Module {
    pub name: Symbol,
    pub is_skip: bool,
    pub inject: bool,
    head: Option<NodeId>,
    tail: Option<NodeId>,
    len: usize,
    inputs: FnvIndexSet<NodeId>,
    outputs: FnvIndexSet<NodeOutId>,
}

impl Module {
    pub(super) fn new(name: Symbol) -> Self {
        Self {
            name,
            is_skip: true,
            inject: false,
            head: None,
            tail: None,
            len: 0,
            inputs: Default::default(),
            outputs: Default::default(),
        }
    }

    pub(super) fn add(&mut self, nodes: &mut Vec<Node>, node_id: NodeId) {
        let tail = self.tail;

        if self.head.is_none() {
            self.head = Some(node_id);
        }
        self.tail = Some(node_id);
        self.len += 1;

        if let Some(tail) = tail {
            Self::link(nodes, tail, Some(node_id));
        }

        if nodes[node_id].is_input() {
            self.add_input(node_id);
        }
    }

    pub(super) fn remove(&mut self, nodes: &mut Vec<Node>, node_id: NodeId) {
        let prev = nodes[node_id].prev();
        let next = nodes[node_id].next();
        if let Some(prev) = prev {
            Self::link(nodes, prev, next);
        }

        nodes[node_id].set_prev(None);
        nodes[node_id].set_next(None);

        if self.head == Some(node_id) {
            self.head = next;
        }

        if self.tail == Some(node_id) {
            self.tail = prev;
        }

        if nodes[node_id].is_input() {
            self.remove_input(node_id);
        }
    }

    pub(super) fn insert(
        &mut self,
        nodes: &mut Vec<Node>,
        prev_node_id: Option<NodeId>,
        node_id: NodeId,
    ) {
        match prev_node_id {
            Some(prev_node_id) => {
                let next = nodes[prev_node_id].next();

                Self::link(nodes, prev_node_id, Some(node_id));
                Self::link(nodes, node_id, next);

                if self.tail == Some(prev_node_id) {
                    self.tail = Some(node_id);
                }
            }
            None => {
                if self.head.is_some() {
                    Self::link(nodes, node_id, self.head);
                    self.head = Some(node_id);
                } else {
                    self.add(nodes, node_id);
                }
            }
        }

        if nodes[node_id].is_input() {
            self.add_input(node_id);
        }
    }

    pub(super) fn replace(&mut self, nodes: &mut Vec<Node>, node: &mut Node) {
        let node_id = node.node_id();

        let old_node = &nodes[node_id];
        node.set_prev(old_node.prev());
        node.set_next(old_node.next());

        if !(old_node.is_input() && node.is_input()) {
            if old_node.is_input() {
                self.remove_input(node_id);
            }

            if node.is_input() {
                self.add_input(node_id);
            }
        }
    }

    fn link(nodes: &mut Vec<Node>, node_id: NodeId, next: Option<NodeId>) {
        nodes[node_id].set_next(next);
        if let Some(next) = next {
            nodes[next].set_prev(Some(node_id));
        }
    }

    pub(super) fn add_input(&mut self, node_id: NodeId) {
        self.inputs.insert(node_id);
    }

    pub(super) fn remove_input(&mut self, node_id: NodeId) {
        self.inputs.shift_remove(&node_id);
    }

    pub(super) fn is_input(&self, node_id: NodeId) -> bool {
        self.inputs.contains(&node_id)
    }

    pub(super) fn inputs(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.inputs.iter().copied()
    }

    pub(super) fn input_idx(&self, node_id: NodeId) -> usize {
        self.inputs.get_index_of(&node_id).unwrap()
    }

    pub(super) fn add_output(&mut self, node_out_id: NodeOutId) {
        self.outputs.insert(node_out_id);
    }

    pub(super) fn is_output(&self, node_out_id: NodeOutId) -> bool {
        self.outputs.contains(&node_out_id)
    }

    pub(super) fn outputs(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        self.outputs.iter().copied()
    }

    pub(super) fn output_by_ind(&self, ind: usize) -> NodeOutId {
        self.outputs[ind]
    }

    pub(super) fn replace_output(&mut self, old_id: NodeOutId, new_id: NodeOutId) {
        if let Some(old_idx) = self.outputs.get_index_of(&old_id) {
            let (new_idx, _) = self.outputs.replace_full(new_id);
            self.outputs.swap_indices(old_idx, new_idx);
            self.outputs.shift_remove(&old_id);
        }
    }

    pub(super) fn head(&self) -> Option<NodeId> {
        self.head
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn inputs_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn outputs_len(&self) -> usize {
        self.outputs.len()
    }

    pub(crate) fn dump(&self, module_id: ModuleId) {
        println!(
            "{} {} (is_skip {}, inject {}, head {:?}, tail {:?})",
            module_id.idx(),
            self.name,
            self.is_skip,
            self.inject,
            self.head.map(|head| head.idx()),
            self.tail.map(|tail| tail.idx())
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
                .map(|input| format!("{} ({})", input.node_id().idx(), input.out_id()))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
    }
}
