mod ident;
mod in_out;
mod module;
mod with_id;

use std::ops::{Deref, Index, IndexMut};

use fnv::{FnvHashMap, FnvHashSet};
pub use ident::{ModuleId, NodeId, NodeInId, NodeOutId};
pub(crate) use in_out::InOut;
pub use with_id::WithId;

pub use self::module::Module;
use crate::{
    const_val::ConstVal,
    node::{Const, IsNode, ModInst, MultiConst, Node, NodeKind, NodeOutput},
    sig_ty::NodeTy,
    symbol::Symbol,
};

pub type Links = FnvHashSet<NodeId>;

#[derive(Debug, Default)]
pub struct NetList {
    modules: Vec<Module>,
    top_module: Option<ModuleId>,
    nodes: Vec<Node>,
    links: FnvHashMap<NodeOutId, FnvHashSet<NodeInId>>,
}

impl !Sync for NetList {}
impl !Send for NetList {}

impl Index<ModuleId> for Vec<Module> {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self[index.idx()]
    }
}

impl IndexMut<ModuleId> for Vec<Module> {
    fn index_mut(&mut self, index: ModuleId) -> &mut Self::Output {
        &mut self[index.idx()]
    }
}

impl Index<ModuleId> for NetList {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self.modules[index]
    }
}

impl IndexMut<ModuleId> for NetList {
    fn index_mut(&mut self, index: ModuleId) -> &mut Self::Output {
        &mut self.modules[index]
    }
}

impl Index<NodeId> for Vec<Node> {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self[index.idx()]
    }
}

impl IndexMut<NodeId> for Vec<Node> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self[index.idx()]
    }
}

impl Index<NodeId> for NetList {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index]
    }
}

impl IndexMut<NodeId> for NetList {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index]
    }
}

impl Index<NodeOutId> for Vec<Node> {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        let node = &self[index.node_id()];
        node.output_by_ind(index.out_id()).into_inner()
    }
}

impl IndexMut<NodeOutId> for Vec<Node> {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        let node = &mut self[index.node_id()];
        node.output_by_ind_mut(index.out_id()).into_inner()
    }
}

impl Index<NodeOutId> for NetList {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        &self.nodes[index]
    }
}

impl IndexMut<NodeOutId> for NetList {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        &mut self.nodes[index]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NodeCursor {
    mod_id: ModuleId,
    node_id: Option<NodeId>,
}

impl NodeCursor {
    pub(crate) fn set_node_id(&mut self, node_id: Option<NodeId>) {
        self.node_id = node_id;
    }
}

impl NetList {
    pub fn new() -> Self {
        Self::default()
    }

    fn next_mod_id(&self) -> ModuleId {
        ModuleId::new(self.modules.len())
    }

    fn next_node_id(&self, mod_id: ModuleId) -> NodeId {
        NodeId::new(mod_id, self.nodes.len())
    }

    pub fn add_module(&mut self, name: Symbol, is_top: bool) -> ModuleId {
        let module_id = self.next_mod_id();
        let module = Module::new(name);
        self.modules.push(module);
        if is_top {
            self.top_module = Some(module_id);
        }
        module_id
    }

    pub fn mod_cursor(&self, mod_id: ModuleId) -> NodeCursor {
        NodeCursor {
            mod_id,
            node_id: None,
        }
    }

    pub fn cursor(&self, mod_id: ModuleId, node_id: Option<NodeId>) -> NodeCursor {
        NodeCursor { mod_id, node_id }
    }

    pub fn next(&self, cursor: &mut NodeCursor) -> Option<NodeId> {
        match cursor.node_id {
            None => {
                cursor.node_id = self.modules[cursor.mod_id].head();
            }
            Some(node) => {
                cursor.node_id = self[node].next();
            }
        }

        cursor.node_id
    }

    pub fn mod_inputs(&self, mod_id: ModuleId) -> impl Iterator<Item = NodeOutId> + '_ {
        self.modules[mod_id]
            .inputs()
            .filter_map(|node_id| {
                let node = &self.nodes[node_id];
                if node.is_input() {
                    Some(node.node_out_ids())
                } else {
                    None
                }
            })
            .flatten()
    }

    pub fn mod_outputs(&self, mod_id: ModuleId) -> impl Iterator<Item = NodeOutId> + '_ {
        self.modules[mod_id].outputs()
    }

    pub fn add<N: IsNode>(&mut self, mod_id: ModuleId, node: N) -> NodeId {
        node.validate(self);
        let node_id = self.next_node_id(mod_id);
        let node: Node = Node::new(node_id, node.into());

        self.nodes.push(node);

        let module = &mut self.modules[mod_id];
        module.add(&mut self.nodes, node_id);

        self.add_links(node_id);

        node_id
    }

    fn remove(&mut self, node_id: NodeId) {
        self.remove_links(node_id);
        for node_out_id in self.nodes[node_id].node_out_ids() {
            self.links.remove(&node_out_id);
        }

        let module = &mut self.modules[node_id.module_id()];
        module.remove(&mut self.nodes, node_id);
    }

    pub fn add_and_get_out<N: IsNode>(&mut self, mod_id: ModuleId, node: N) -> NodeOutId {
        let node_id = self.add(mod_id, node);
        self.nodes[node_id].only_one_out().node_out_id()
    }

    pub fn const_val(&mut self, mod_id: ModuleId, ty: NodeTy, val: u128) -> NodeOutId {
        self.add_and_get_out(mod_id, Const::new(ty, val, None))
    }

    pub fn const_zero(&mut self, mod_id: ModuleId, ty: NodeTy) -> NodeOutId {
        self.const_val(mod_id, ty, 0)
    }

    pub fn const_one(&mut self, mod_id: ModuleId, ty: NodeTy) -> NodeOutId {
        self.const_val(mod_id, ty, 1)
    }

    fn insert<N: IsNode>(
        &mut self,
        mod_id: ModuleId,
        prev_node_id: Option<NodeId>,
        node: N,
    ) -> NodeId {
        node.validate(self);
        let node_id = self.next_node_id(mod_id);
        let node = Node::new(node_id, node.into());

        self.nodes.push(node);

        let module = &mut self.modules[mod_id];
        module.insert(&mut self.nodes, prev_node_id, node_id);

        self.add_links(node_id);

        node_id
    }

    pub fn replace<N: IsNode>(&mut self, node_id: NodeId, node: N) {
        node.validate(self);
        let mod_id = node_id.module_id();
        let mut node: Node = Node::new(node_id, node.into());
        assert_eq!(self[node_id].outputs_len(), node.outputs_len());

        self.remove_links(node_id);

        let module = &mut self.modules[mod_id];
        module.replace(&mut self.nodes, &mut node);

        self.nodes[node_id] = node;

        self.add_links(node_id);
    }

    fn add_links(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        for input in node.inputs() {
            if node_id.module_id() == input.node_id().module_id() {
                let links = self.links.entry(*input).or_default();
                links.insert(input.node_in_id());
            }
        }
    }

    fn remove_links(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        for input in node.inputs() {
            if let Some(links) = self.links.get_mut(&input) {
                links.remove(&input.node_in_id());
            }
        }
    }

    pub(crate) fn reconnect(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        assert_eq!(node.inputs_len(), node.outputs_len());
        let len = node.inputs_len();

        self.remove_links(node_id);
        for ind in 0 .. len {
            let input = self.nodes[node_id].input_by_ind(ind);
            self.reconnect_input_by_ind(node_id, ind, *input);
        }
    }

    pub(crate) fn reconnect_input_by_ind(
        &mut self,
        node_id: NodeId,
        ind: usize,
        new_input: NodeOutId,
    ) {
        let output = self.nodes[node_id].output_by_ind(ind).node_out_id();
        self.reconnect_input(output, new_input);
    }

    fn reconnect_input(&mut self, input: NodeOutId, new_input: NodeOutId) {
        self.links.entry(new_input).or_default();
        for link in self
            .links
            .remove(&input)
            .iter()
            .flat_map(|links| links.iter())
        {
            if link.node_id().module_id() == new_input.node_id().module_id() {
                **self.nodes[link.node_id()].input_by_ind_mut(link.in_id()) = new_input;
                self.links.get_mut(&new_input).unwrap().insert(*link);
            }
        }

        self.modules[input.node_id().module_id()].replace_output(input, new_input);
    }

    pub fn links(&self, node_out_id: NodeOutId) -> impl Iterator<Item = &Node> + '_ {
        self.links
            .get(&node_out_id)
            .into_iter()
            .flat_map(|links| links.iter())
            .map(move |link| &self[link.node_id()])
    }

    pub fn add_output(&mut self, node_out_id: NodeOutId) {
        let mod_id = node_out_id.node_id().module_id();
        let module = &mut self.modules[mod_id];
        module.add_output(node_out_id);
    }

    pub fn is_input(&self, node_id: NodeId) -> bool {
        let mod_id = node_id.module_id();
        let module = &self.modules[mod_id];
        module.is_input(node_id) && self.nodes[node_id].is_input()
    }

    pub fn is_output(&self, node_out_id: NodeOutId) -> bool {
        let mod_id = node_out_id.node_id().module_id();
        let module = &self.modules[mod_id];
        module.is_output(node_out_id)
    }

    pub fn is_out_node(&self, node_id: NodeId) -> bool {
        self.mod_outputs(node_id.module_id())
            .any(|node_out_id| node_out_id.node_id() == node_id)
    }

    pub fn modules(&self) -> impl Iterator<Item = ModuleId> {
        (0 .. self.modules.len()).map(ModuleId::new)
    }

    pub fn top_module(&self) -> Option<ModuleId> {
        self.top_module
    }

    pub fn module_len(&self, mod_id: ModuleId) -> usize {
        self.modules[mod_id].len()
    }

    pub(crate) fn to_const(&self, node_out_id: NodeOutId) -> Option<ConstVal> {
        match &*self[node_out_id.node_id()].kind {
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

    pub(crate) fn inline_mod(&mut self, mod_inst_node_id: NodeId) -> Option<NodeId> {
        let target = mod_inst_node_id.module_id();
        let source = match &*self.nodes[mod_inst_node_id].kind {
            NodeKind::ModInst(ModInst { module_id, .. }) => *module_id,
            _ => {
                return None;
            }
        };

        if source == target || self.modules[source].is_empty() {
            return None;
        }

        let mut prev_id = self.nodes[mod_inst_node_id].prev();
        let res = prev_id;

        #[derive(Debug, Clone, Copy)]
        enum Item {
            NodeId(NodeId),
            NodeOutId(NodeOutId),
        }

        let mut map = FnvHashMap::default();
        fn get_node_out_id(
            map: &FnvHashMap<NodeId, Item>,
            input: NodeOutId,
        ) -> NodeOutId {
            match map.get(&input.node_id()).unwrap() {
                Item::NodeId(node_id) => NodeOutId::new(*node_id, input.out_id()),
                Item::NodeOutId(node_out_id) => *node_out_id,
            }
        }

        let mut cursor = self.mod_cursor(source);
        while let Some(node_id) = self.next(&mut cursor) {
            if self.nodes[node_id].is_input() {
                let idx = self.modules[source].input_idx(node_id);
                let node_out_id = *self.nodes[mod_inst_node_id].input_by_ind(idx);
                map.insert(node_id, Item::NodeOutId(node_out_id));
                continue;
            }

            let new_node = self.nodes[node_id].kind.deref().clone();
            let new_node_id = self.insert(target, prev_id, new_node);
            prev_id = Some(new_node_id);
            map.insert(node_id, Item::NodeId(new_node_id));
        }

        let mut cursor = self.cursor(target, res);
        while let Some(node_id) = self.next(&mut cursor) {
            if node_id == mod_inst_node_id {
                break;
            }
            let node = &mut self.nodes[node_id];
            for mut input in node.inputs_mut() {
                **input = get_node_out_id(&map, **input);
            }

            self.add_links(node_id);
        }

        let len = self.nodes[mod_inst_node_id].outputs_len();
        for ind in 0 .. len {
            let input = self.modules[source].output_by_ind(ind);
            let new_input = get_node_out_id(&map, input);

            self.reconnect_input_by_ind(mod_inst_node_id, ind, new_input);

            let sym = self.nodes[mod_inst_node_id].output_by_ind(ind).sym;
            self.nodes[new_input].sym = sym;
        }

        self.remove(mod_inst_node_id);

        res
    }

    pub fn set_dff_data(&mut self, node_id: NodeId, data: NodeOutId) {
        let node = &mut self.nodes[node_id];
        if let NodeKind::DFF(dff) = node.kind.as_mut() {
            if node_id.module_id() == data.node_id().module_id() {
                let idx = dff.set_data(data);
                self.links
                    .entry(data)
                    .or_default()
                    .insert(NodeInId::new(node_id, idx));
            }
        }
    }
}
