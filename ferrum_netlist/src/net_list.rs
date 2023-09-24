use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use fnv::FnvBuildHasher;
use indexmap::IndexSet;

use crate::{
    group::ItemId,
    node::{InputNode, IsNode, Node, NodeOutput},
    params::{Inputs, Outputs},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(ModuleId, usize);

impl NodeId {
    pub fn module_id(&self) -> ModuleId {
        self.0
    }
}

pub type OutId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeOutId(NodeId, OutId);

impl NodeOutId {
    pub fn new(node_id: NodeId, out: OutId) -> Self {
        Self(node_id, out)
    }

    pub fn node_id(&self) -> NodeId {
        self.0
    }
}

type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;
type FnvHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;

#[derive(Debug)]
pub struct Module {
    pub name: Symbol,
    module_id: ModuleId,
    nodes: Vec<Node>,
    inputs: FnvIndexSet<NodeId>,
    outputs: FnvIndexSet<NodeOutId>,
}

impl Index<NodeId> for Module {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        assert_eq!(self.module_id, index.module_id());
        &self.nodes[index.1]
    }
}

impl IndexMut<NodeId> for Module {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        assert_eq!(self.module_id, index.module_id());
        &mut self.nodes[index.1]
    }
}

impl Index<NodeOutId> for Module {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        let node = &self[index.node_id()];
        node.outputs().by_ind(index.1).out
    }
}

impl IndexMut<NodeOutId> for Module {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        let node = &mut self[index.node_id()];
        node.outputs_mut().by_ind_mut(index.1).out
    }
}

impl Module {
    fn new(name: Symbol, module_id: ModuleId) -> Self {
        Self {
            name,
            module_id,
            nodes: Vec::with_capacity(16),
            inputs: FnvIndexSet::default(),
            outputs: FnvIndexSet::default(),
        }
    }

    fn add_node<N: IsNode>(&mut self, node: N) -> NodeId {
        let node_id = NodeId(self.module_id, self.nodes.len());

        let node: Node = node.into();
        if node.is_input() {
            self.inputs.insert(node_id);
        }
        self.nodes.push(node);

        node_id
    }

    fn replace<N: IsNode>(&mut self, node_id: NodeId, node: N) {
        let old_node = &self[node_id];
        assert_eq!(old_node.outputs().len(), node.outputs().len());

        if old_node.is_input() {
            self.inputs.remove(&node_id);
        }
        let node: Node = node.into();
        if node.is_input() {
            self.inputs.insert(node_id);
        }

        self[node_id] = node;
    }

    fn add_output(&mut self, node_id: NodeId, out: OutId) {
        assert_eq!(self.module_id, node_id.module_id());
        let node_out_id = NodeOutId(node_id, out);
        self.outputs.insert(node_out_id);
    }

    fn add_all_outputs(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id.1];
        for out in node.outputs().items() {
            self.outputs.insert(out.node_out_id(node_id));
        }
    }

    pub fn is_output(&self, node_out_id: NodeOutId) -> bool {
        self.outputs.contains(&node_out_id)
    }

    pub fn nodes(&self) -> impl Iterator<Item = NodeId> {
        let module_id = self.module_id;
        (0 .. self.nodes.len()).map(move |ind| NodeId(module_id, ind))
    }

    pub fn inputs(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.inputs.iter().copied()
    }

    pub fn inputs_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn outputs(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        self.outputs.iter().copied()
    }

    pub fn outputs_len(&self) -> usize {
        self.outputs.len()
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn truncate(&mut self, new_len: usize) {
        self.nodes.truncate(new_len)
    }
}

#[derive(Debug, Default)]
pub struct NetList {
    modules: Vec<Module>,
    dummy_inputs: FnvHashMap<ItemId, Vec<NodeId>>,
}

impl !Sync for NetList {}
impl !Send for NetList {}

impl Index<ModuleId> for NetList {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self.modules[index.0]
    }
}

impl IndexMut<ModuleId> for NetList {
    fn index_mut(&mut self, index: ModuleId) -> &mut Self::Output {
        &mut self.modules[index.0]
    }
}

impl Index<NodeId> for NetList {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        let module = &self[index.0];
        &module[index]
    }
}

impl IndexMut<NodeId> for NetList {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        let module = &mut self[index.0];
        &mut module[index]
    }
}

impl Index<NodeOutId> for NetList {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        let node = &self[index.0];
        node.outputs().by_ind(index.1).out
    }
}

impl IndexMut<NodeOutId> for NetList {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        let node = &mut self[index.0];
        node.outputs_mut().by_ind_mut(index.1).out
    }
}

impl NetList {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module(&mut self, name: Symbol) -> ModuleId {
        let module_id = ModuleId(self.modules.len());
        let module = Module::new(name, module_id);
        self.modules.push(module);
        module_id
    }

    pub fn add_node<N: IsNode>(&mut self, module_id: ModuleId, node: N) -> NodeId {
        let module = &mut self[module_id];
        module.add_node(node)
    }

    pub fn add_dummy_node(&mut self, module_id: ModuleId, node: InputNode) -> NodeId {
        let node = Node::DummyInput(node);
        self.add_node(module_id, node)
    }

    pub fn replace<N: IsNode>(&mut self, node_id: NodeId, node: N) {
        let module = &mut self[node_id.module_id()];
        module.replace(node_id, node);
    }

    pub fn add_output(&mut self, node_id: NodeId, out: OutId) {
        let module = &mut self[node_id.module_id()];
        module.add_output(node_id, out);
    }

    pub fn add_all_outputs(&mut self, node_id: NodeId) {
        let module = &mut self[node_id.module_id()];
        module.add_all_outputs(node_id);
    }

    pub fn is_output(&mut self, node_out_id: NodeOutId) -> bool {
        let module = &self[node_out_id.node_id().module_id()];
        module.is_output(node_out_id)
    }

    pub fn only_one_node_out_id(&self, node_id: NodeId) -> NodeOutId {
        self[node_id].outputs().only_one().node_out_id(node_id)
    }

    pub fn modules(&self) -> impl Iterator<Item = ModuleId> {
        (0 .. self.modules.len()).map(ModuleId)
    }

    pub fn find_node(
        &self,
        node_id: NodeId,
        f: impl Fn(&Node) -> bool + Copy,
    ) -> Vec<NodeId> {
        let mut res = vec![];
        self.find_node_inner(node_id, f, &mut res);
        res
    }

    fn find_node_inner(
        &self,
        node_id: NodeId,
        f: impl Fn(&Node) -> bool + Copy,
        res: &mut Vec<NodeId>,
    ) {
        let node = &self[node_id];
        if f(node) {
            res.push(node_id);
        }

        for input in node.inputs().items() {
            let node_id = input.0;
            self.find_node_inner(node_id, f, res);
        }
    }

    pub fn add_dummy_inputs(
        &mut self,
        item_id: ItemId,
        dummy_inputs: impl IntoIterator<Item = NodeId>,
    ) {
        self.dummy_inputs
            .insert(item_id, dummy_inputs.into_iter().collect());
    }

    pub fn dummy_inputs_len(&self, item_id: ItemId) -> Option<usize> {
        self.dummy_inputs
            .get(&item_id)
            .map(|dummy_inputs| dummy_inputs.len())
    }

    pub fn dummy_input(&mut self, item_id: ItemId, ind: usize) -> Option<NodeId> {
        self.dummy_inputs
            .get(&item_id)
            .and_then(|dummy_inputs| dummy_inputs.get(ind))
            .copied()
    }

    pub fn find_dummy_inputs(&self, node_id: NodeId) -> Vec<NodeId> {
        self.find_node(node_id, |node| node.is_dummy_input())
    }

    pub fn module_len(&self, module_id: ModuleId) -> usize {
        self[module_id].len()
    }

    pub fn module_truncate(&mut self, module_id: ModuleId, new_len: usize) {
        self[module_id].truncate(new_len)
    }

    // pub fn link(
    //     &mut self,
    //     from: NodeOutId,
    //     to: NodeId
    // ) {

    // }

    // pub fn link_dummy_input(
    //     &mut self,
    //     with_dummy: NodeId,
    //     to_link: &[NodeOutId],
    //     ignore: bool,
    // ) {
    //     let dummy = self.find_dummy_inputs(with_dummy);
    //     if !ignore {
    //         assert_eq!(dummy.len(), to_link.len());
    //     }

    //     if dummy.len() == to_link.len() {
    //         for (dummy, to_link) in dummy.into_iter().zip(to_link.iter().copied()) {
    //             let to_link_node = &self[to_link.node_id()];
    //             let to_link_out = to_link_node.outputs().only_one().out;
    //             let dummy_out = &self[dummy].outputs().only_one().out;

    //             let pass = PassNode::new(to_link_out.ty, to_link, dummy_out.sym);
    //             self.replace(dummy, pass);
    //         }
    //     }
    // }
}
