mod ident;
mod in_out;
pub(crate) mod list;
mod module;
mod with_id;

use std::{
    hash::BuildHasherDefault,
    ops::{Index, IndexMut},
};

pub use ident::*;
pub(crate) use in_out::InOut;
use indexmap::IndexSet;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
pub use with_id::WithId;

pub use self::module::Module;
use crate::{
    const_val::ConstVal,
    node::{Const, IsNode, Node, NodeKindWithId, NodeKindWithIdMut, NodeOutput, Pass},
    node_ty::NodeTy,
    symbol::Symbol,
};

pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;

pub type Nodes = FxHashMap<NodeId, Node>;
pub type Links = FxHashSet<NodeId>;

#[derive(Debug)]
pub struct NetListCfg {
    pub inline_all: bool,
}

#[derive(Debug)]
pub struct NetList {
    modules: Vec<Module>,
    top_module: Option<ModuleId>,
    nodes: Nodes,
    links: FxHashMap<NodeOutId, FxHashSet<NodeInIdx>>,
    cfg: NetListCfg,
    pub(crate) nodes_injected: bool,
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

impl Index<NodeId> for Nodes {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        self.get(&index).unwrap()
    }
}

impl IndexMut<NodeId> for Nodes {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.get_mut(&index).unwrap()
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

impl Index<NodeOutId> for Nodes {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        let node = &self[index.node_id()];
        node.output_by_ind(index.idx()).into_inner()
    }
}

impl IndexMut<NodeOutId> for Nodes {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        let node = &mut self[index.node_id()];
        node.output_by_ind_mut(index.idx()).into_inner()
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
    pub fn new(cfg: NetListCfg) -> Self {
        Self {
            modules: Default::default(),
            top_module: None,
            nodes: Default::default(),
            links: Default::default(),
            cfg,
            nodes_injected: false,
        }
    }

    pub fn cfg(&self) -> &NetListCfg {
        &self.cfg
    }

    fn next_mod_id(&self) -> ModuleId {
        ModuleId::new(self.modules.len())
    }

    pub fn add_module(&mut self, name: Symbol, is_top: bool) -> ModuleId {
        let module_id = self.next_mod_id();
        let module = Module::new(module_id, is_top, name);
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

    pub fn mod_inputs(
        &self,
        mod_id: ModuleId,
    ) -> impl DoubleEndedIterator<Item = NodeOutId> + '_ {
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

    pub fn mod_outputs(
        &self,
        mod_id: ModuleId,
    ) -> impl DoubleEndedIterator<Item = NodeOutId> + '_ {
        self.modules[mod_id].outputs()
    }

    pub fn add<N: IsNode>(&mut self, mod_id: ModuleId, node: N) -> NodeId {
        let module = &mut self.modules[mod_id];
        let node_id = module.next_node_id();
        let node: Node = Node::new(node_id, node.into());

        self.add_node(mod_id, node);

        node_id
    }

    pub fn add_node(&mut self, mod_id: ModuleId, node: Node) {
        assert!(!self.nodes.contains_key(&node.node_id()));
        assert_eq!(node.node_id().module_id(), mod_id);

        let node_id = node.node_id();
        self.nodes.insert(node_id, node);

        let module = &mut self.modules[mod_id];
        module.add(&mut self.nodes, node_id.into());

        self.add_links(node_id);
    }

    fn remove(&mut self, node_id: NodeId) {
        self.remove_links(node_id);
        for node_out_id in self.nodes[node_id].node_out_ids() {
            self.links.remove(&node_out_id);
        }

        let module = &mut self.modules[node_id.module_id()];
        module.remove(&mut self.nodes, node_id.into());
    }

    pub fn add_and_get_out<N: IsNode>(&mut self, mod_id: ModuleId, node: N) -> NodeOutId {
        let node_id = self.add(mod_id, node);
        self.nodes[node_id].only_one_out().node_out_id()
    }

    pub fn add_mod_span(&mut self, mod_id: ModuleId, span: Option<String>) {
        self.modules[mod_id].set_span(span);
    }

    pub fn add_span(&mut self, node_id: impl Into<NodeId>, span: Option<String>) {
        let node_id = node_id.into();
        let node = &mut self.nodes[node_id];
        if !node.is_input() {
            self.nodes[node_id].set_span(span);
        }
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

    fn insert_node(
        &mut self,
        mod_id: ModuleId,
        prev_node_id: Option<NodeId>,
        node: Node,
    ) {
        assert!(!self.nodes.contains_key(&node.node_id()));
        assert_eq!(node.node_id().module_id(), mod_id);

        let node_id = node.node_id();
        self.nodes.insert(node_id, node);

        let module = &mut self.modules[mod_id];
        module.insert(
            &mut self.nodes,
            prev_node_id.map(Into::into),
            node_id.into(),
        );

        self.add_links(node_id);
    }

    pub fn replace<N: IsNode>(&mut self, node_id: NodeId, node: N) {
        node.assert(node_id.module_id(), self);

        let mod_id = node_id.module_id();
        let mut node: Node = Node::new(node_id, node.into());
        assert_eq!(self[node_id].outputs_len(), node.outputs_len());

        self.remove_links(node_id);

        let module = &mut self.modules[mod_id];
        module.replace(&mut self.nodes, &mut node);

        self.nodes[node_id] = node;

        self.add_links(node_id);
    }

    fn add_link(&mut self, node_out_id: NodeOutId, node_in_id: NodeInId) {
        assert_eq!(
            node_out_id.node_id().module_id(),
            node_in_id.node_id().module_id()
        );
        self.links
            .entry(node_out_id)
            .or_default()
            .insert(node_in_id.into());
    }

    fn add_links(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        for input in node.inputs() {
            assert_eq!(node_id.module_id(), input.node_id().module_id());

            self.links
                .entry(*input)
                .or_default()
                .insert(input.node_in_id().into());
        }
    }

    fn remove_links(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        for input in node.inputs() {
            if let Some(links) = self.links.get_mut(&input) {
                links.remove(&(input.node_in_id().into()));
            }
        }
    }

    pub(crate) fn reconnect(&mut self, node_id: NodeId) {
        self.reconnect_from_to(node_id, node_id);
    }

    pub(crate) fn reconnect_from_to(&mut self, from_id: NodeId, to_id: NodeId) {
        let from = &self.nodes[from_id];
        let to = &self.nodes[to_id];
        assert_eq!(from.inputs_len(), to.outputs_len());

        let len = from.inputs_len();
        self.remove_links(from_id);

        for ind in 0 .. len {
            let input = self.nodes[from_id].input_by_ind(ind);
            self.reconnect_input_by_ind(to_id, ind, *input);
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
        let module_id = input.node_id().module_id();
        assert_eq!(module_id, new_input.node_id().module_id());

        self.links.entry(new_input).or_default();
        for link in self
            .links
            .remove(&input)
            .iter()
            .flat_map(|links| links.iter())
        {
            let link_node_id = NodeId::make(module_id, link.node_idx());

            **self.nodes[link_node_id].input_by_ind_mut(link.idx()) = new_input.into();
            self.links.get_mut(&new_input).unwrap().insert(*link);
        }

        if self.modules[module_id].replace_output(input, new_input) {
            let node_id = new_input.node_id();
            let node = &self.nodes[node_id];

            // add pass node if node is module input and replace output with it
            let new_input = if node.is_input() {
                let module_id = node_id.module_id();

                let pass = self.add_and_get_out(
                    module_id,
                    Pass::new(node.only_one_out().ty, new_input, None),
                );
                self.modules[module_id].replace_output(new_input, pass);

                pass
            } else {
                new_input
            };

            // use output name for new_input
            let sym = self.nodes[input].sym;
            if sym.is_some() {
                self.nodes[new_input].sym = sym;
            }
        }
    }

    pub fn links(&self, node_out_id: NodeOutId) -> impl Iterator<Item = &Node> + '_ {
        let module_id = node_out_id.node_id().module_id();

        self.links
            .get(&node_out_id)
            .into_iter()
            .flat_map(|links| links.iter())
            .map(move |link| NodeId::make(module_id, link.node_idx()))
            .map(move |node_id| &self[node_id])
    }

    pub fn add_output(&mut self, node_out_id: NodeOutId) {
        // Check that node_out_id exists
        let _ = self.nodes[node_out_id];

        let mod_id = node_out_id.node_id().module_id();
        let module = &mut self.modules[mod_id];
        module.add_output(node_out_id.into());
    }

    pub fn is_input(&self, node_id: NodeId) -> bool {
        let mod_id = node_id.module_id();
        let module = &self.modules[mod_id];
        module.is_input(node_id.into()) && self.nodes[node_id].is_input()
    }

    pub fn is_output(&self, node_out_id: NodeOutId) -> bool {
        let mod_id = node_out_id.node_id().module_id();
        let module = &self.modules[mod_id];
        module.is_output(node_out_id.into())
    }

    pub fn is_out_node(&self, node_id: NodeId) -> bool {
        self.mod_outputs(node_id.module_id())
            .any(|node_out_id| node_out_id.node_id() == node_id)
    }

    pub fn modules(&self) -> impl Iterator<Item = ModuleId> {
        (0 .. self.modules.len()).map(ModuleId::new)
    }

    pub fn modules_len(&self, skip: bool) -> usize {
        if !skip {
            self.modules.len()
        } else {
            self.modules()
                .filter(|module| self.modules[*module].skip)
                .count()
        }
    }

    pub fn nodes_len(&self, module_id: ModuleId, skip: bool) -> usize {
        let mut cursor = self.mod_cursor(module_id);
        let mut count = 0;
        while let Some(node_id) = self.next(&mut cursor) {
            if skip {
                if !self.nodes[node_id].skip {
                    count += 1;
                }
            } else {
                count += 1;
            }
        }

        count
    }

    pub fn top_module(&self) -> Option<ModuleId> {
        self.top_module
    }

    pub fn to_const(&self, node_out_id: NodeOutId) -> Option<ConstVal> {
        use NodeKindWithId as NodeKind;

        match self[node_out_id.node_id()].kind() {
            NodeKind::Const(cons) => {
                Some(ConstVal::new(cons.value(), cons.output().width()))
            }
            NodeKind::MultiConst(multi_cons) => {
                let out_id = node_out_id.idx();
                Some(ConstVal::new(
                    multi_cons.values()[out_id],
                    multi_cons.outputs()[out_id].width(),
                ))
            }
            _ => None,
        }
    }

    pub fn shift_last_idx(&mut self, module_id: ModuleId, idx: usize) {
        self.modules[module_id].shift_last_idx(idx)
    }

    pub fn last_idx(&self, module_id: ModuleId) -> usize {
        self.modules[module_id].last_idx()
    }

    pub(crate) fn inline_mod(&mut self, mod_inst_node_id: NodeId) -> Option<NodeId> {
        use NodeKindWithId as NodeKind;

        let target = mod_inst_node_id.module_id();
        let source = match self.nodes[mod_inst_node_id].kind() {
            NodeKind::ModInst(mod_inst) => mod_inst.module_id(),
            _ => {
                return None;
            }
        };

        if source == target || self.modules[source].is_empty() {
            return None;
        }

        let mut prev_id = self.nodes[mod_inst_node_id].prev();
        let res = prev_id;

        let offset = self.last_idx(target);
        let last_idx = self.last_idx(source) + offset;
        self.shift_last_idx(target, last_idx);

        let inputs: FxHashMap<NodeOutId, NodeOutId> = self.modules[source]
            .inputs()
            .zip(self.nodes[mod_inst_node_id].inputs())
            .map(|(module_input, input)| {
                let module_input = self.nodes[module_input].only_one_out().node_out_id();
                (module_input, *input)
            })
            .collect();

        let get_node_out_id =
            |inputs: &FxHashMap<_, _>, node_out_id: NodeOutId| -> NodeOutId {
                match inputs.get(&node_out_id) {
                    Some(node_out_id) => *node_out_id,
                    None => node_out_id.with_module_id(target) + offset,
                }
            };

        let mut cursor = self.mod_cursor(source);
        while let Some(node_id) = self.next(&mut cursor) {
            if self.nodes[node_id].is_input() {
                continue;
            }

            let node = &self.nodes[node_id];
            let node_id = node_id.with_module_id(target) + offset;
            let mut node = Node::clone_from(node_id, node);

            for mut input in node.inputs_mut() {
                let node_out_id = NodeOutId::make(source, **input);
                **input = get_node_out_id(&inputs, node_out_id).into();
            }

            self.insert_node(target, prev_id, node);
            prev_id = Some(node_id);
        }

        // Reconnect nodes referring to ModInst to nodes that are outputs for the source module.
        let len = self.nodes[mod_inst_node_id].outputs_len();
        for ind in 0 .. len {
            let input = self.modules[source].output_by_ind(ind);
            let new_input = get_node_out_id(&inputs, input);

            self.reconnect_input_by_ind(mod_inst_node_id, ind, new_input);

            let sym = self.nodes[mod_inst_node_id].output_by_ind(ind).sym;
            self.nodes[new_input].sym = sym;
        }

        self.remove(mod_inst_node_id);

        res
    }

    pub fn set_dff_data(&mut self, node_id: NodeId, data: NodeOutId) {
        let node = &mut self.nodes[node_id];
        if let NodeKindWithIdMut::DFF(mut dff) = node.kind_mut() {
            let idx = dff.set_data(data);

            self.add_link(data, NodeInId::new(node_id, idx));
        }
    }

    pub fn is_reversible(&self, from_id: NodeId, to_id: NodeId) -> bool {
        let from = &self.nodes[from_id];
        let to = &self.nodes[to_id];

        let compare = |from: NodeOutId, to: NodeOutId| {
            let from = &self.nodes[from];
            let to = &self.nodes[to];

            from.ty == to.ty
        };

        from.inputs_len() == to.outputs_len()
            && from.outputs_len() == to.inputs_len()
            && from
                .inputs()
                .zip(to.outputs())
                .all(|(from, to)| compare(*from, to.node_out_id()))
            && from
                .outputs()
                .zip(to.inputs())
                .all(|(from, to)| from.node_out_id() == *to)
    }
}
