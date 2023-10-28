use fnv::FnvBuildHasher;
use indexmap::IndexSet;

use super::ident::{NodeId, NodeOutId};
use crate::symbol::Symbol;

type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;

#[derive(Debug)]
pub struct Module {
    pub name: Symbol,
    pub is_skip: bool,
    pub inject: bool,
    head: NodeId,
    tail: NodeId,
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
            head: NodeId::none(),
            tail: NodeId::none(),
            len: 0,
            inputs: Default::default(),
            outputs: Default::default(),
        }
    }

    pub(super) fn add(&mut self, node_id: NodeId) -> Option<NodeId> {
        let old_node_id = self.tail;
        if self.head.is_none() {
            self.head = node_id;
        }
        self.tail = node_id;
        self.len += 1;
        old_node_id.into_opt()
    }

    pub(super) fn add_input(&mut self, node_id: NodeId) {
        self.inputs.insert(node_id);
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

    pub(super) fn replace_output(&mut self, old_id: NodeOutId, new_id: NodeOutId) {
        if let Some(old_idx) = self.outputs.get_index_of(&old_id) {
            let (new_idx, _) = self.outputs.replace_full(new_id);
            self.outputs.swap_indices(old_idx, new_idx);
            self.outputs.shift_remove(&old_id);
        }
    }

    pub(super) fn is_output(&self, node_out_id: NodeOutId) -> bool {
        self.outputs.contains(&node_out_id)
    }

    pub(super) fn outputs(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        self.outputs.iter().copied()
    }

    pub(super) fn head(&self) -> Option<NodeId> {
        self.head.into_opt()
    }

    pub(super) fn truncate(&mut self, tail: Option<NodeId>) -> Option<NodeId> {
        let old_tail = self.tail.into_opt();
        self.tail = NodeId::from_opt(tail);
        old_tail
    }

    pub(super) fn set_tail(&mut self, tail: Option<NodeId>) {
        self.tail = NodeId::from_opt(tail);
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

    pub(crate) fn dump(&self) {
        println!(
            "{} (is_skip {}, inject {}, head {:?}, tail {:?})",
            self.name,
            self.is_skip,
            self.inject,
            self.head.idx(),
            self.tail.idx()
        );
        println!(
            "inputs: {}",
            self.inputs
                .iter()
                .map(|input| format!("{}", input.idx().unwrap()))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
        println!(
            "outputs: {}",
            self.outputs
                .iter()
                .map(|input| format!(
                    "{} ({})",
                    input.node_id().idx().unwrap(),
                    input.out_id()
                ))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
    }
}
