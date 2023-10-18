use std::ops::{Index, IndexMut};

use fnv::{FnvBuildHasher, FnvHashMap, FnvHashSet};
use indexmap::IndexSet;
use smallvec::SmallVec;

use crate::{
    const_val::ConstVal,
    group::ItemId,
    node::{Const, Input, IsNode, MultiConst, Node, NodeKind, NodeOutput},
    params::{Inputs, Outputs},
    symbol::Symbol,
};

pub type OutId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModNodeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModNodeOutId(ModNodeId, OutId);

impl ModNodeOutId {
    pub fn new(node_id: ModNodeId, out: OutId) -> Self {
        Self(node_id, out)
    }

    pub fn node_id(&self) -> ModNodeId {
        self.0
    }

    pub fn out_id(&self) -> OutId {
        self.1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(ModuleId, usize);

impl NodeId {
    pub fn module_id(&self) -> ModuleId {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeOutId(NodeId, OutId);

impl NodeOutId {
    pub fn new(node_id: NodeId, out: OutId) -> Self {
        Self(node_id, out)
    }

    pub fn node_id(&self) -> NodeId {
        self.0
    }

    pub fn out_id(&self) -> OutId {
        self.1
    }
}

type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;

#[derive(Debug)]
pub struct Module {
    pub name: Symbol,
    pub is_skip: bool,
    pub inject: bool,
    module_id: ModuleId,
    nodes: FnvHashMap<usize, Node>,
    inputs: FnvIndexSet<NodeId>,
    outputs: FnvIndexSet<NodeOutId>,
}

impl Module {
    fn new(name: Symbol, module_id: ModuleId) -> Self {
        Self {
            name,
            is_skip: true,
            inject: false,
            module_id,
            nodes: FnvHashMap::default(),
            inputs: FnvIndexSet::default(),
            outputs: FnvIndexSet::default(),
        }
    }

    // Private because nodes can refer to nodes from other modules (it's necessary for injecting
    // const values, for example)
    fn node(&self, node_id: NodeId) -> &Node {
        self.nodes.get(&node_id.1).unwrap()
    }

    // Private because nodes can refer to nodes from other modules (it's necessary for injecting
    // const values, for example)
    fn node_mut(&mut self, node_id: NodeId) -> &mut Node {
        self.nodes.get_mut(&node_id.1).unwrap()
    }

    fn add_node(&mut self, node: Node) -> NodeId {
        let node_id = NodeId(self.module_id, self.nodes.len());

        if node.kind.is_input() {
            self.inputs.insert(node_id);
        }

        self.nodes.insert(node_id.1, node);

        node_id
    }

    fn replace(&mut self, node_id: NodeId, node: Node) {
        let old_node = &self.node(node_id).kind;
        assert_eq!(old_node.outputs().len(), node.kind.outputs().len());

        if old_node.is_input() {
            self.inputs.remove(&node_id);
        }

        if node.kind.is_input() {
            self.inputs.insert(node_id);
        }

        let old_node = self.node_mut(node_id);
        *old_node = node;
    }

    fn add_output(&mut self, node_id: NodeId, out: OutId) {
        assert_eq!(self.module_id, node_id.module_id());
        let node_out_id = NodeOutId(node_id, out);
        self.outputs.insert(node_out_id);
    }

    fn add_all_outputs(&mut self, node_id: NodeId) {
        let node = self.nodes.get(&node_id.1).unwrap();
        for out in node.kind.outputs().items() {
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

    pub fn inputs(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        self.inputs
            .iter()
            .filter(|input| self.node(**input).kind.is_input())
            .filter_map(|&input| {
                let node = self.node(input);
                if node.kind.is_input() {
                    Some(node.kind.outputs().only_one().node_out_id(input))
                } else {
                    None
                }
            })
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

    pub fn is_node_output(&self, node_id: NodeId) -> bool {
        self.outputs
            .iter()
            .any(|output| output.node_id() == node_id)
    }

    pub fn get_out_id(&self, node_out_id: NodeOutId) -> Option<NodeOutId> {
        let out = node_out_id.out_id();
        self.outputs.get_index(out).copied()
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}

pub type Links = FnvHashSet<NodeId>;

#[derive(Debug, Default)]
pub struct NetList {
    modules: Vec<Module>,
    dummy_inputs: FnvHashMap<ItemId, SmallVec<[NodeId; 8]>>,
    rev_links: FnvHashMap<NodeOutId, Links>,
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
        module.node(index)
    }
}

impl IndexMut<NodeId> for NetList {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        let module = &mut self[index.0];
        module.node_mut(index)
    }
}

impl Index<NodeOutId> for NetList {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        let node = &self[index.0];
        node.kind.outputs().by_ind(index.1).out
    }
}

impl IndexMut<NodeOutId> for NetList {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        let node = &mut self[index.0];
        node.kind.outputs_mut().by_ind_mut(index.1).out
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

    pub fn add<N: IsNode>(&mut self, module_id: ModuleId, node: N) -> NodeId {
        let node: NodeKind = node.into();
        let node: Node = node.into();
        self.add_node(module_id, node)
    }

    pub(crate) fn add_node(&mut self, module_id: ModuleId, node: Node) -> NodeId {
        // let node = self.transform(node);
        let module = &mut self[module_id];
        let node_id = module.add_node(node);
        self.add_links(node_id);

        node_id
    }

    fn add_links(&mut self, node_id: NodeId) {
        let node = self.modules[node_id.module_id().0].node(node_id);
        for input in node.kind.inputs().items() {
            let links = self.rev_links.entry(input).or_default();
            links.insert(node_id);
        }
    }

    fn remove_links(&mut self, node_id: NodeId) {
        let node = self.modules[node_id.module_id().0].node(node_id);
        for input in node.kind.inputs().items() {
            let links = self.rev_links.entry(input).or_default();
            links.remove(&node_id);
        }
    }

    pub(crate) fn replace_inputs(
        &mut self,
        node_id: NodeId,
        new_inputs: impl IntoIterator<Item = Option<NodeOutId>>,
    ) {
        let node = &mut self.modules[node_id.module_id().0].node_mut(node_id);
        for (input, new_input) in node.kind.inputs_mut().items_mut().zip(new_inputs) {
            if let Some(new_input) = new_input {
                self.rev_links.remove(input);
                *input = new_input;
            }
        }
    }

    pub fn links(
        &self,
        node_out_id: NodeOutId,
    ) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        self.rev_links
            .get(&node_out_id)
            .into_iter()
            .flat_map(|links| links.iter())
            .map(move |link| (*link, &self[*link]))
    }

    pub fn add_dummy_node(&mut self, module_id: ModuleId, node: Input) -> NodeId {
        let node = NodeKind::DummyInput(node);
        self.add(module_id, node)
    }

    pub fn replace<N: IsNode>(&mut self, node_id: NodeId, node: N) {
        let node: NodeKind = node.into();
        let node: Node = node.into();
        self.replace_node(node_id, node);
    }

    pub(crate) fn replace_node(&mut self, node_id: NodeId, node: Node) {
        assert_eq!(
            self[node_id].kind.outputs().len(),
            node.kind.outputs().len()
        );
        // let node = self.transform(node);
        self.remove_links(node_id);

        let module = &mut self[node_id.module_id()];
        module.replace(node_id, node);
        self.add_links(node_id);

        // let all_links = self[node_id]
        //     .kind
        //     .node_out_ids(node_id)
        //     .flat_map(|output| self.rev_links.get(&output))
        //     .flat_map(|links| links.iter())
        //     .copied()
        //     .collect::<SmallVec<[_; 8]>>();

        // for link in all_links {
        //     self.add_links(link);
        // }
    }

    pub fn add_output(&mut self, node_id: NodeId, out: OutId) {
        let module = &mut self[node_id.module_id()];
        module.add_output(node_id, out);
    }

    pub fn add_all_outputs(&mut self, node_id: NodeId) {
        let module = &mut self[node_id.module_id()];
        module.add_all_outputs(node_id);
    }

    pub fn is_output(&self, node_out_id: NodeOutId) -> bool {
        let module = &self[node_out_id.node_id().module_id()];
        module.is_output(node_out_id)
    }

    pub fn only_one_node_out_id(&self, node_id: NodeId) -> NodeOutId {
        self[node_id].kind.outputs().only_one().node_out_id(node_id)
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

        for input in node.kind.inputs().items() {
            let node_id = input.0;
            self.find_node_inner(node_id, f, res);
        }
    }

    pub fn add_dummy_inputs(
        &mut self,
        item_id: ItemId,
        dummy_inputs: impl IntoIterator<Item = NodeId>,
    ) {
        self.dummy_inputs.insert(
            item_id,
            dummy_inputs
                .into_iter()
                .filter(|node_id| self[*node_id].kind.is_dummy_input())
                .collect(),
        );
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
        self.find_node(node_id, |node| node.kind.is_dummy_input())
    }

    pub fn module_len(&self, module_id: ModuleId) -> usize {
        self[module_id].len()
    }

    pub(crate) fn to_const(&self, node_out_id: NodeOutId) -> Option<ConstVal> {
        match &self[node_out_id.node_id()].kind {
            NodeKind::Const(Const { value, output }) => {
                Some(ConstVal::new(*value, output.width()))
            }
            NodeKind::MultiConst(MultiConst { values, outputs }) => {
                let out_id = node_out_id.out_id();
                Some(ConstVal::new(values[out_id], outputs[out_id].width()))
            }
            _ => None,
        }
    }
}
