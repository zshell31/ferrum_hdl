use std::{
    ops::{Index, IndexMut},
    rc::Rc,
};

use indexmap::set::Slice;
use rustc_hash::FxHashMap;

use super::{
    graph::{IncomingEdges, OutgoingEdges},
    list::{List, ListCursor, ListStorage},
    EdgeId, Graph, IndexType, ListItem, ModuleId, NodeId, Port, PortPos, WithId,
};
use crate::{
    const_val::ConstVal,
    cursor::Cursor,
    node::{
        Const, ConstArgs, Input, InputArgs, IsNode, MakeNode, ModInst, Node, NodeKind,
        NodeOutput, Pass, PassArgs,
    },
    node_ty::NodeTy,
    symbol::Symbol,
    FxIndexSet,
};

#[derive(Debug)]
pub struct Module {
    pub name: Symbol,
    pub is_top: bool,
    pub skip: bool,
    pub inline: bool,
    span: Option<Rc<String>>,
    graph: Graph,
    list: List<Graph>,
    inputs: FxIndexSet<Port>,
    outputs: FxIndexSet<Port>,
}

impl ListStorage for Graph {
    type Idx = NodeId;
    type Item = Node;
}

impl Index<NodeId> for Module {
    type Output = Node;

    #[inline]
    fn index(&self, node_id: NodeId) -> &Self::Output {
        &self.graph[node_id]
    }
}

impl IndexMut<NodeId> for Module {
    #[inline]
    fn index_mut(&mut self, node_id: NodeId) -> &mut Self::Output {
        &mut self.graph[node_id]
    }
}

impl Index<Port> for Module {
    type Output = NodeOutput;

    fn index(&self, port: Port) -> &Self::Output {
        let node = &self[port.node];
        &node.outputs()[port.port as usize]
    }
}

impl IndexMut<Port> for Module {
    fn index_mut(&mut self, port: Port) -> &mut Self::Output {
        let node = &mut self[port.node];
        &mut node.outputs_mut()[port.port as usize]
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Incoming(IncomingEdges);

impl Cursor for Incoming {
    type Item = Port;
    type Storage = Module;

    #[inline]
    fn next_(&mut self, module: &Module) -> Option<Self::Item> {
        let edge_id = self.0.next_(&module.graph)?;
        Some(module.graph[edge_id].port_out)
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Outgoing(OutgoingEdges);

impl Cursor for Outgoing {
    type Item = NodeId;
    type Storage = Module;

    #[inline]
    fn next_(&mut self, module: &Module) -> Option<Self::Item> {
        let edge_id = self.0.next_(&module.graph)?;
        Some(module.graph[edge_id].port_in.node)
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct NodeCursor(ListCursor<Graph>);

impl NodeCursor {
    #[inline]
    pub(crate) fn set_next(&mut self, next: NodeId) {
        self.0.set_next(next)
    }
}

impl Cursor for NodeCursor {
    type Item = NodeId;
    type Storage = Module;

    #[inline]
    fn next_(&mut self, module: &Module) -> Option<Self::Item> {
        self.0.next_(&module.graph)
    }
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeWithInputs {
    pub kind: NodeKind,
    pub inputs: Vec<Port>,
}

#[cfg(test)]
impl NodeWithInputs {
    pub fn new(
        kind: impl Into<NodeKind>,
        inputs: impl IntoIterator<Item = Port>,
    ) -> Self {
        Self {
            kind: kind.into(),
            inputs: inputs.into_iter().collect(),
        }
    }

    pub fn from_node(node_id: NodeId, module: &Module) -> Self {
        let node = &module[node_id];
        let kind = node.kind().clone();

        NodeWithInputs::new(kind, module.incoming(node_id).into_iter_(module))
    }
}

impl Module {
    pub fn new(name: impl AsRef<str>, is_top: bool) -> Self {
        Self {
            name: Symbol::intern(name),
            is_top,
            skip: true,
            inline: false,
            span: None,
            graph: Default::default(),
            list: Default::default(),
            inputs: Default::default(),
            outputs: Default::default(),
        }
    }

    #[cfg(test)]
    pub fn test() -> Self {
        Self::new("test", false)
    }

    pub fn span(&self) -> Option<&str> {
        self.span.as_ref().map(|s| s.as_str())
    }

    pub fn set_span(&mut self, span: Option<String>) {
        self.span = span.map(Rc::new);
    }

    pub fn add<Args, N: MakeNode<Args>>(&mut self, args: Args) -> NodeId {
        let node_id = N::make(self, args);

        self.add_mod_input(Port::new(node_id, 0));
        self.list.add(&mut self.graph, node_id);

        node_id
    }

    pub fn add_and_get_port<Args, N: MakeNode<Args>>(&mut self, args: Args) -> Port {
        let node_id = self.add::<Args, N>(args);
        let node = &self.graph[node_id];
        assert_eq!(node.out_count(), 1);

        Port {
            node: node_id,
            port: 0,
        }
    }

    pub fn insert<Args, N: MakeNode<Args>>(
        &mut self,
        prev_node_id: NodeId,
        args: Args,
    ) -> NodeId {
        let node_id = N::make(self, args);

        self.add_mod_input(Port::new(node_id, 0));
        self.list.insert(&mut self.graph, prev_node_id, node_id);

        node_id
    }

    pub fn insert_and_get_port<Args, N: MakeNode<Args>>(
        &mut self,
        prev_node_id: NodeId,
        args: Args,
    ) -> Port {
        let node_id = self.insert::<Args, N>(prev_node_id, args);
        let node = &self.graph[node_id];
        assert_eq!(node.out_count(), 1);

        Port {
            node: node_id,
            port: 0,
        }
    }

    pub fn add_input(&mut self, ty: NodeTy, sym: Option<impl AsRef<str>>) -> Port {
        self.add_and_get_port::<_, Input>(InputArgs {
            ty,
            sym: sym.map(Symbol::intern),
        })
    }

    #[inline]
    pub fn const_val(&mut self, ty: NodeTy, val: u128) -> Port {
        self.add_and_get_port::<_, Const>(ConstArgs {
            ty,
            value: val,
            sym: None,
        })
    }

    #[inline]
    pub fn const_zero(&mut self, ty: NodeTy) -> Port {
        self.const_val(ty, 0)
    }

    pub fn add_span(&mut self, node_id: NodeId, span: Option<String>) {
        let node = &mut self.graph[node_id];
        node.set_span(span);
    }

    fn clone_span(&mut self, from: NodeId, to: NodeId) {
        let span = self[from].span_rc();
        self[to].set_span_rc(span);
    }

    pub(crate) fn replace<Args, N: MakeNode<Args>>(
        &mut self,
        node_id: NodeId,
        args: Args,
    ) {
        let new_node_id = N::make(self, args);
        self.clone_span(node_id, new_node_id);

        let old_node = self.node(node_id);
        let new_node = self.node(new_node_id);

        assert!(!old_node.is_input());
        assert!(!new_node.is_input());
        assert_eq!(old_node.out_count(), new_node.out_count());

        for (old_port, new_port) in old_node.out_ports().zip(new_node.out_ports()) {
            self.reconnect_all_outgoing_(old_port, new_port);
        }

        self.list.insert(&mut self.graph, node_id, new_node_id);
        self.remove(node_id);
    }

    pub(crate) fn remove(&mut self, node_id: NodeId) {
        self.list.remove(&mut self.graph, node_id);
        self.graph.remove_node(node_id);
    }

    pub(crate) fn add_node(&mut self, node: impl Into<NodeKind>) -> NodeId {
        self.graph.add_node(Node::new(node.into()))
    }

    #[inline]
    pub(crate) fn add_edge(&mut self, port_out: Port, port_in: Port) -> EdgeId {
        self.graph.add_edge(port_out, port_in)
    }

    #[inline]
    pub fn incoming(&self, node_id: NodeId) -> Incoming {
        Incoming(self.graph.incoming(node_id))
    }

    #[inline]
    pub fn incoming_iter(&self, node_id: NodeId) -> impl Iterator<Item = Port> + '_ {
        self.incoming(node_id).into_iter_(self)
    }

    #[inline]
    pub fn outgoing(&self, port: Port) -> Outgoing {
        Outgoing(self.graph.outgoing(port))
    }

    pub fn mod_inst_inputs<'m>(
        &'m self,
        mod_inst: WithId<NodeId, &ModInst>,
        orig_mod: WithId<ModuleId, &'m Module>,
    ) -> impl Iterator<Item = (WithId<Port, &'m NodeOutput>, WithId<Port, &'m NodeOutput>)> + 'm
    {
        assert_eq!(mod_inst.mod_id, orig_mod.id);

        self.incoming(mod_inst.id)
            .into_iter_(self)
            .map(|port| WithId::new(port, &self[port]))
            .zip(
                orig_mod
                    .mod_inputs()
                    .iter()
                    .copied()
                    .map(move |port| WithId::new(port, &orig_mod[port])),
            )
    }

    pub fn mod_inst_outputs<'m>(
        &self,
        mod_inst: WithId<NodeId, &'m ModInst>,
        orig_mod: WithId<ModuleId, &'m Module>,
    ) -> impl Iterator<Item = (WithId<Port, &'m NodeOutput>, WithId<Port, &'m NodeOutput>)> + 'm
    {
        assert_eq!(mod_inst.mod_id, orig_mod.id);

        mod_inst.outputs().zip(
            orig_mod
                .mod_outputs()
                .iter()
                .copied()
                .map(move |port| WithId::new(port, &orig_mod[port])),
        )
    }

    fn add_mod_input(&mut self, port: Port) {
        if self.graph[port.node].is_input() {
            self.inputs.insert(port);
        }
    }

    #[inline]
    pub fn add_mod_output(&mut self, port: Port) {
        assert!(
            !self.outputs.contains(&port),
            "{port} already added as output"
        );
        self.outputs.insert(port);
    }

    pub fn add_mod_outputs(&mut self, node_id: NodeId) {
        let node = self.node(node_id);
        self.outputs.extend(node.out_ports());
    }

    fn replace_mod_output(&mut self, old_port: Port, new_port: Port) -> bool {
        if self.outputs.contains(&old_port) {
            self.outputs.insert(new_port);
            self.outputs.swap_remove(&old_port)
        } else {
            false
        }
    }

    #[inline]
    pub fn is_mod_input(&self, port: Port) -> bool {
        self.inputs.contains(&port)
    }

    #[inline]
    pub fn mod_inputs(&self) -> &Slice<Port> {
        self.inputs.as_slice()
    }

    #[inline]
    pub fn mod_in_count(&self) -> usize {
        self.inputs.len()
    }

    #[inline]
    pub fn is_mod_output(&self, port: Port) -> bool {
        self.outputs.contains(&port)
    }

    #[inline]
    pub fn mod_outputs(&self) -> &Slice<Port> {
        self.outputs.as_slice()
    }

    #[cfg(test)]
    pub fn mod_outputs_vec(&self, skip: bool) -> Vec<NodeWithInputs> {
        self.mod_outputs()
            .iter()
            .filter(|port| !(skip && self[port.node].skip))
            .map(|port| {
                let node_id = port.node;
                NodeWithInputs::from_node(node_id, self)
            })
            .collect()
    }

    #[inline]
    pub fn mod_out_count(&self) -> usize {
        self.outputs.len()
    }

    pub(crate) fn has_const_outputs(&self) -> bool {
        self.outputs
            .iter()
            .all(|port| self.graph[port.node].is_const())
    }

    pub fn port_pos(&self, port: Port) -> Option<PortPos> {
        self.inputs
            .get_index_of(&port)
            .map(PortPos::Input)
            .or_else(|| self.outputs.get_index_of(&port).map(PortPos::Output))
    }

    pub fn get_port_by_pos(&self, port_pos: PortPos) -> Port {
        match port_pos {
            PortPos::Input(idx) => self.inputs[idx],
            PortPos::Output(idx) => self.outputs[idx],
        }
    }

    #[inline]
    pub fn nodes(&self) -> NodeCursor {
        NodeCursor(self.list.cursor())
    }

    #[cfg(test)]
    pub fn nodes_vec(&self, skip: bool) -> Vec<NodeWithInputs> {
        self.nodes()
            .into_iter_(self)
            .filter(|node_id| !(skip && self[*node_id].skip))
            .map(|node_id| NodeWithInputs::from_node(node_id, self))
            .collect()
    }

    #[inline]
    pub fn node_count(&self) -> usize {
        self.graph.node_count()
    }

    #[inline]
    pub fn node(&self, node_id: NodeId) -> WithId<NodeId, &Node> {
        let inner = &self.graph[node_id];
        WithId { id: node_id, inner }
    }

    #[inline]
    pub fn node_mut(&mut self, node_id: NodeId) -> WithId<NodeId, &mut Node> {
        let inner = &mut self.graph[node_id];
        WithId { id: node_id, inner }
    }

    #[inline]
    pub fn is_const(&self, port: Port) -> bool {
        self.graph[port.node].is_const()
    }

    pub fn to_const(&self, port: Port) -> Option<ConstVal> {
        match self.graph[port.node].kind() {
            NodeKind::Const(cons) => Some(cons.value()),
            NodeKind::MultiConst(multi_cons) => {
                Some(multi_cons.value(port.port as usize))
            }
            _ => None,
        }
    }

    pub fn node_has_const_inputs(&self, node_id: NodeId) -> bool {
        self.incoming(node_id)
            .into_iter_(self)
            .all(|port| self.graph[port.node].is_const())
    }

    pub fn node_out_ports(&self, node_id: NodeId) -> impl Iterator<Item = Port> {
        let node = self.node(node_id);
        node.out_ports()
    }

    pub fn reconnect_all_outgoing(
        &mut self,
        node_id: NodeId,
        new_ports: impl IntoIterator<Item = Port>,
    ) {
        for (idx, new_port) in new_ports.into_iter().enumerate() {
            let old_port = Port::new(node_id, idx as u32);
            self.reconnect_all_outgoing_(old_port, new_port);
        }
    }

    fn reconnect_all_outgoing_(&mut self, old_port: Port, new_port: Port) {
        self.graph.reconnect_all_outgoing(old_port, new_port);

        if self.replace_mod_output(old_port, new_port) {
            let node = &self.graph[new_port.node];

            let new_port = if node.is_input() {
                let pass = self.add_and_get_port::<_, Pass>(PassArgs {
                    input: new_port,
                    sym: None,
                    ty: None,
                });

                self.replace_mod_output(new_port, pass);
                pass
            } else {
                new_port
            };

            // use output name for new_input
            let sym = self[old_port].sym;
            if sym.is_some() {
                self[new_port].sym = sym;
            }
        }
    }

    pub fn reconnect_from_inputs_to_outputs(&mut self, from_id: NodeId, to_id: NodeId) {
        let from = &self.graph[from_id];
        let to = &self.graph[to_id];
        assert_eq!(from.in_count(), to.out_count());

        let mut incoming = self.graph.incoming(from_id);
        while let Some(edge_id) = incoming.next_(&self.graph) {
            let edge = &self.graph[edge_id];
            let new_port = edge.port_out;
            let old_port = Port {
                node: to_id,
                port: edge.port_in.port,
            };

            self.reconnect_all_outgoing_(old_port, new_port);
        }
    }

    #[inline]
    pub fn reconnect(&mut self, node_id: NodeId) {
        self.reconnect_from_inputs_to_outputs(node_id, node_id);
    }

    pub fn is_reversible(&self, from_id: NodeId, to_id: NodeId) -> bool {
        let from = &self.graph[from_id];
        let to = &self.graph[to_id];

        from.in_count() == to.out_count()
            && from.out_count() == to.in_count()
            && self
                .incoming(from_id)
                .into_iter_(self)
                .zip(to.outputs())
                .all(|(from, to)| {
                    let from = &self[from];
                    from.ty == to.ty
                })
            && self
                .graph
                .incoming(to_id)
                .into_iter_(&self.graph)
                .all(|edge_id| {
                    let edge = &self.graph[edge_id];
                    let port_out = edge.port_out;
                    let port_in = edge.port_in;

                    edge.port_out.node == from_id && port_out.port == port_in.port
                })
    }

    pub(super) fn inline_mod(
        &mut self,
        mod_inst_id: NodeId,
        source_mod: WithId<ModuleId, &Module>,
    ) -> Option<NodeId> {
        let last_node_id = self.graph.last_node_id();

        let mod_inst = self.graph[mod_inst_id].mod_inst().unwrap();
        let mod_inst = WithId {
            id: mod_inst_id,
            inner: mod_inst,
        };
        let mod_inst_inputs = self
            .mod_inst_inputs(mod_inst, source_mod)
            .map(|(mod_inst, source_mod)| (source_mod.id, mod_inst.id))
            .collect::<FxHashMap<_, _>>();

        let calc_node_id = |id: NodeId| {
            if !id.is_empty() && !source_mod[id].is_input() {
                NodeId::new(id.as_u32() + last_node_id.as_u32())
            } else {
                NodeId::EMPTY
            }
        };

        let calc_port = |port: Port| match mod_inst_inputs.get(&port) {
            Some(port) => *port,
            None => Port::new(calc_node_id(port.node), port.port),
        };

        self.graph.reserve_nodes(source_mod.graph.node_count());
        self.graph.reserve_edges(source_mod.graph.edge_count());

        for (node_id, node) in source_mod.graph.raw_nodes() {
            if node.is_input() {
                continue;
            }

            let node_id = calc_node_id(*node_id);
            let node = node.new_from(calc_node_id);

            self.graph.insert_node(node_id, node);
        }

        for (node_id, _) in source_mod.graph.raw_nodes() {
            let node_id = *node_id;
            for edge_id in source_mod
                .graph
                .incoming(node_id)
                .into_iter_(&source_mod.graph)
            {
                let edge = &source_mod.graph[edge_id];
                let port_out = calc_port(edge.port_out);
                let port_in = calc_port(edge.port_in);

                self.graph.add_edge(port_out, port_in);
            }
        }

        for (idx, output) in source_mod.mod_outputs().iter().enumerate() {
            let old_port = Port::new(mod_inst_id, idx as u32);
            let new_port = output.with(calc_node_id(output.node));

            self.reconnect_all_outgoing_(old_port, new_port);

            let sym = self[old_port].sym;
            if sym.is_some() {
                self[new_port].sym = sym;
            }
        }

        let mut start = source_mod.list.head;
        while !start.is_empty() && source_mod.graph[start].is_input() {
            start = source_mod.graph[start].next();
        }
        start = calc_node_id(start);
        let end = calc_node_id(source_mod.list.tail);
        self.list
            .insert_list(&mut self.graph, mod_inst_id, start, end);

        self.remove(mod_inst_id);

        start.into_opt()
    }
}

impl WithId<ModuleId, &'_ Module> {
    pub(crate) fn dump(self) {
        println!(
            "{} {} (is_skip {}, head {}, tail {})",
            self.id, self.name, self.skip, self.list.head, self.list.tail,
        );
        println!(
            "inputs: {}",
            self.inputs
                .iter()
                .map(|input| input.node.as_u32().to_string())
                .intersperse(", ".to_string())
                .collect::<String>()
        );
        println!(
            "outputs: {}",
            self.outputs
                .iter()
                .map(|input| input.to_string())
                .intersperse(", ".to_string())
                .collect::<String>()
        );
    }
}
