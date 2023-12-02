mod ident;
mod module;

use std::ops::{Index, IndexMut};

use fnv::{FnvHashMap, FnvHashSet};
pub use ident::{ModuleId, NodeId, NodeOutId};
use smallvec::SmallVec;

pub use self::module::Module;
use crate::{
    const_val::ConstVal,
    group::ItemId,
    node::{
        Const, Input, IsNode, ModInst, MultiConst, MultiPass, Node, NodeKind, NodeOutput,
        Pass, Splitter, ZeroExtend,
    },
    params::{Inputs, Outputs},
    symbol::Symbol,
};

pub type Links = FnvHashSet<NodeId>;

#[derive(Debug, Default)]
pub struct NetList {
    modules: Vec<Module>,
    top_module: Option<ModuleId>,
    nodes: Vec<Node>,
    dummy_inputs: FnvHashMap<ItemId, SmallVec<[NodeId; 8]>>,
    rev_links: FnvHashMap<NodeOutId, Links>,
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
        &self[index.idx().unwrap()]
    }
}

impl IndexMut<NodeId> for Vec<Node> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self[index.idx().unwrap()]
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

impl Index<NodeOutId> for NetList {
    type Output = NodeOutput;

    fn index(&self, index: NodeOutId) -> &Self::Output {
        let node = &self.nodes[index.node_id()];
        node.kind.outputs().by_ind(index.out_id()).out
    }
}

impl IndexMut<NodeOutId> for NetList {
    fn index_mut(&mut self, index: NodeOutId) -> &mut Self::Output {
        let node = &mut self.nodes[index.node_id()];
        node.kind.outputs_mut().by_ind_mut(index.out_id()).out
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
        // let head = self.modules[mod_id].head();
        NodeCursor {
            mod_id,
            node_id: None,
        }
    }

    pub fn next(&self, cursor: &mut NodeCursor) -> Option<NodeId> {
        match cursor.node_id {
            None => {
                cursor.node_id = self.modules[cursor.mod_id].head();
            }
            Some(node) => {
                let next = self[node].next();
                cursor.node_id = next;
            }
        }

        cursor.node_id
    }

    pub fn mod_inputs(&self, mod_id: ModuleId) -> impl Iterator<Item = NodeOutId> + '_ {
        self.modules[mod_id]
            .inputs()
            .filter_map(|node_id| {
                let node = &self.nodes[node_id];
                if node.kind.is_input() {
                    Some(node.kind.node_out_ids(node_id))
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
        let node: NodeKind = node.into();
        let node: Node = node.into();
        self.add_node(mod_id, node)
    }

    pub(crate) fn add_node(&mut self, mod_id: ModuleId, node: Node) -> NodeId {
        self.validate_node(&node);
        let is_input = node.kind.is_input();
        let node_id = self.next_node_id(mod_id);
        self.nodes.push(node);

        let module = &mut self.modules[mod_id];
        let old_node_id = module.add(node_id);
        if is_input {
            module.add_input(node_id);
        }

        if let Some(old_node_id) = old_node_id {
            self.nodes[old_node_id].set_next(node_id);
            self.nodes[node_id].set_prev(old_node_id);
        }

        self.add_links(node_id);

        node_id
    }

    pub fn replace<N: IsNode>(&mut self, node_id: NodeId, node: N) {
        let node: NodeKind = node.into();
        let node: Node = node.into();
        self.replace_node(node_id, node);
    }

    pub(crate) fn replace_node(&mut self, node_id: NodeId, node: Node) {
        self.validate_node(&node);
        let mod_id = node_id.module_id();
        let is_new_input = node.kind.is_input();
        assert_eq!(
            self[node_id].kind.outputs().len(),
            node.kind.outputs().len()
        );

        self.remove_links(node_id);

        let old_node = &self.nodes[node_id];
        let next_id = old_node.next();
        let prev_id = old_node.prev();
        let is_old_input = old_node.kind.is_input();
        self.nodes[node_id] = node;

        let module = &mut self.modules[mod_id];
        if !(is_new_input & is_old_input) {
            if is_old_input {
                module.remove_input(node_id);
            }
            if is_new_input {
                module.add_input(node_id);
            }
        }

        if let Some(prev_id) = prev_id {
            self.nodes[node_id].set_prev(prev_id);
        }
        if let Some(next_id) = next_id {
            self.nodes[node_id].set_next(next_id);
        }

        self.add_links(node_id);
    }

    pub(crate) fn validate_node(&self, node: &Node) {
        match &node.kind {
            NodeKind::Splitter(Splitter { input, outputs, .. }) => {
                let input_width = self[*input].width();
                let output_width =
                    outputs.iter().map(|output| output.width()).sum::<u128>();
                if output_width > input_width {
                    panic!(
                        "Splitter: output width {} > input width {}",
                        output_width, input_width
                    );
                }
            }
            NodeKind::ZeroExtend(ZeroExtend { input, output }) => {
                let input_width = self[*input].width();
                if input_width > output.width() {
                    panic!(
                        "ZeroExtend: output width {} < input width {}",
                        output.width(),
                        input_width
                    );
                }
            }
            _ => {}
        }
    }

    fn add_links(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        for input in node.kind.inputs().items() {
            let links = self.rev_links.entry(input).or_default();
            links.insert(node_id);
        }
    }

    fn remove_links(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        for input in node.kind.inputs().items() {
            let links = self.rev_links.entry(input).or_default();
            links.remove(&node_id);
        }
    }

    pub(crate) fn exclude_pass_nodes(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];
        let mut prev_inputs = SmallVec::<[Option<NodeOutId>; 8]>::new();
        let mut syms = SmallVec::<[_; 8]>::new();

        for input in node.kind.inputs().items() {
            let input_node = &self.nodes[input.node_id()];

            let prev_input = match &input_node.kind {
                NodeKind::Pass(Pass { input, .. }) => Some(*input),
                NodeKind::MultiPass(MultiPass { inputs, .. }) => {
                    Some(inputs[input.out_id()])
                }
                _ => None,
            };

            prev_inputs.push(prev_input);

            if let Some(prev_input) = prev_input {
                let sym = self[input].sym;
                if self[prev_input].sym.is_none() && sym.is_some() {
                    syms.push((prev_input, sym));
                }
            }
        }

        let node = &mut self.nodes[node_id];
        for (input, prev_input) in node.kind.inputs_mut().items_mut().zip(prev_inputs) {
            if let Some(prev_input) = prev_input {
                if let Some(links) = self.rev_links.get_mut(input) {
                    links.remove(&node_id);
                }
                if let Some(links) = self.rev_links.get_mut(&prev_input) {
                    links.remove(&input.node_id());
                    links.insert(node_id);
                }
                *input = prev_input;
            }
        }

        for (prev_input, sym) in syms {
            self[prev_input].sym = sym;
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

    pub fn add_output(&mut self, node_id: NodeId, out: usize) {
        let mod_id = node_id.module_id();
        let module = &mut self.modules[mod_id];
        module.add_output(NodeOutId::new(node_id, out));
    }

    pub fn replace_output(&mut self, old_id: NodeOutId, new_id: NodeOutId) {
        let mod_id = old_id.node_id().module_id();
        let module = &mut self.modules[mod_id];
        module.replace_output(old_id, new_id);
    }

    pub fn add_all_outputs(&mut self, node_id: NodeId) {
        let mod_id = node_id.module_id();
        let node = &self.nodes[node_id];
        let module = &mut self.modules[mod_id];

        for out in node.kind.node_out_ids(node_id) {
            module.add_output(out);
        }
    }

    pub fn is_input(&self, node_id: NodeId) -> bool {
        let mod_id = node_id.module_id();
        let module = &self.modules[mod_id];
        module.is_input(node_id) && self.nodes[node_id].kind.is_input()
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

    pub fn module_len(&self, mod_id: ModuleId) -> usize {
        self.modules[mod_id].len()
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

    pub(crate) fn inline_mod(&mut self, mod_inst_node_id: NodeId) -> bool {
        let target = mod_inst_node_id.module_id();
        let (source, inputs) = match &self.nodes[mod_inst_node_id].kind {
            NodeKind::ModInst(ModInst {
                module_id, inputs, ..
            }) => (
                *module_id,
                inputs.iter().copied().collect::<SmallVec<[_; 8]>>(),
            ),
            _ => {
                return false;
            }
        };

        if source == target || self.modules[source].is_empty() {
            return false;
        }

        let mut last_node = None;
        let prev_id = self.nodes[mod_inst_node_id].prev();
        let old_tail_id = self.modules[target].truncate(prev_id);

        // Add new nodes:
        // Pass
        // nodes of modules except of inputs
        // MultiPass
        let mut node_id_map = FnvHashMap::default();
        let mut cursor = self.mod_cursor(source);
        while let Some(node_id) = self.next(&mut cursor) {
            let new_node = self.nodes[node_id].clone();
            let new_node = match new_node.kind {
                NodeKind::Input(input) => {
                    let input_idx = self.modules[source].input_idx(node_id);
                    let pass_input = inputs[input_idx];
                    let pass_output = input.output;
                    Pass::new(pass_output.ty, pass_input, pass_output.sym).into()
                }
                _ => new_node,
            };

            for input in new_node.kind.inputs_mut().items_mut() {
                if let Some(new_node_id) = node_id_map.get(&input.node_id()) {
                    let out_id = input.out_id();
                    *input = NodeOutId::new(*new_node_id, out_id);
                }
            }

            let new_node_id = self.add_node(target, new_node);
            node_id_map.insert(node_id, new_node_id);
            last_node = Some(new_node_id);
        }

        let pass = MultiPass::new(
            self.modules[source].outputs().filter_map(|output| {
                let new_node_id = node_id_map.get(&output.node_id())?;
                Some(NodeOutId::new(*new_node_id, output.out_id()))
            }),
            self.nodes[mod_inst_node_id]
                .kind
                .outputs()
                .items()
                .map(|output| (output.out.ty, output.out.sym)),
        );

        self.replace_node(mod_inst_node_id, pass.into());
        if let Some(last_node) = last_node {
            self.nodes[mod_inst_node_id].set_prev(last_node);
            self.nodes[last_node].set_next(mod_inst_node_id);
        }

        // Restore tail of module
        self.modules[target].set_tail(old_tail_id);

        true
    }
}
