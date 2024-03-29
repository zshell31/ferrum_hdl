use std::{
    fmt::Write,
    ops::{Index, IndexMut},
};

use tracing::debug;

use super::{
    index_storage::IndexStorage, list::ListCursor, EdgeId, IndexType, ListItem,
    ListStorage, NodeId, Port,
};
use crate::{cursor::Cursor, node::Node};

pub trait Direction {
    const IDX: usize;
}

pub struct IncomingDir;

impl Direction for IncomingDir {
    const IDX: usize = 0;
}

pub struct OutgoingDir;

impl Direction for OutgoingDir {
    const IDX: usize = 1;
}

#[derive(Debug, Clone, Copy)]
pub struct Edge {
    pub port_out: Port,
    pub port_in: Port,
    next: [EdgeId; 2],
    prev: [EdgeId; 2],
}

impl<D: Direction> ListItem<EdgeId, D> for Edge {
    #[inline]
    fn next(&self) -> EdgeId {
        self.next[D::IDX]
    }

    #[inline]
    fn set_next(&mut self, next: EdgeId) {
        self.next[D::IDX] = next;
    }

    #[inline]
    fn prev(&self) -> EdgeId {
        self.prev[D::IDX]
    }

    #[inline]
    fn set_prev(&mut self, prev: EdgeId) {
        self.prev[D::IDX] = prev;
    }
}

pub type Nodes = IndexStorage<NodeId, Node>;
pub type Edges = IndexStorage<EdgeId, Edge>;

impl<D: Direction> ListStorage<D> for Edges {
    type Idx = EdgeId;
    type Item = Edge;
}

#[derive(Debug, Default)]
pub struct Graph {
    nodes: Nodes,
    edges: Edges,
}

impl Index<NodeId> for Graph {
    type Output = Node;

    #[inline]
    fn index(&self, node_id: NodeId) -> &Self::Output {
        &self.nodes[node_id]
    }
}

impl IndexMut<NodeId> for Graph {
    #[inline]
    fn index_mut(&mut self, node_id: NodeId) -> &mut Self::Output {
        &mut self.nodes[node_id]
    }
}

impl Index<EdgeId> for Graph {
    type Output = Edge;

    #[inline]
    fn index(&self, edge_id: EdgeId) -> &Self::Output {
        &self.edges[edge_id]
    }
}

impl IndexMut<EdgeId> for Graph {
    #[inline]
    fn index_mut(&mut self, edge_id: EdgeId) -> &mut Self::Output {
        &mut self.edges[edge_id]
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct IncomingEdges(ListCursor<Edges, IncomingDir>);

impl Cursor for IncomingEdges {
    type Item = EdgeId;
    type Storage = Graph;

    #[inline]
    fn next_(&mut self, graph: &Graph) -> Option<Self::Item> {
        self.0.next_(&graph.edges)
    }
}

#[derive(Clone, Copy)]
pub struct OutgoingEdges {
    edges: ListCursor<Edges, OutgoingDir>,
    out: u32,
}

impl Cursor for OutgoingEdges {
    type Item = EdgeId;
    type Storage = Graph;

    #[inline]
    fn next_(&mut self, graph: &Graph) -> Option<Self::Item> {
        loop {
            let edge_id = self.edges.next_(&graph.edges)?;
            let edge = &graph.edges[edge_id];

            if edge.port_out.port == self.out {
                return Some(edge_id);
            }
        }
    }
}

impl Graph {
    #[inline]
    pub fn add_node(&mut self, node: Node) -> NodeId {
        self.nodes.push(node)
    }

    #[inline]
    pub(crate) fn insert_node(&mut self, node_id: NodeId, node: Node) {
        self.nodes.insert(node_id, node);
    }

    #[inline]
    pub fn remove_node(&mut self, node_id: NodeId) {
        self.remove_all_edges(node_id);
        self.nodes.swap_remove(&node_id);
    }

    pub fn add_edge(&mut self, port_out: Port, port_in: Port) -> EdgeId {
        let edge_id = self.push_edge(port_out, port_in);
        self.nodes[port_out.node]
            .outgoing
            .add(&mut self.edges, edge_id);
        self.nodes[port_in.node]
            .incoming
            .add(&mut self.edges, edge_id);

        edge_id
    }

    pub fn remove_edge(&mut self, edge_id: EdgeId) {
        let edge = &self.edges[edge_id];
        let port_out = edge.port_out.node;
        let port_in = edge.port_in.node;

        self.nodes[port_out]
            .outgoing
            .remove(&mut self.edges, edge_id);
        self.nodes[port_in]
            .incoming
            .remove(&mut self.edges, edge_id);

        self.edges.swap_remove(&edge_id);
    }

    #[inline]
    fn push_edge(&mut self, port_out: Port, port_in: Port) -> EdgeId {
        self.edges.push(Edge {
            port_out,
            port_in,
            next: [EdgeId::EMPTY; 2],
            prev: [EdgeId::EMPTY; 2],
        })
    }

    pub fn incoming(&self, node_id: NodeId) -> IncomingEdges {
        let node = &self.nodes[node_id];
        IncomingEdges(node.incoming.cursor())
    }

    pub fn outgoing(&self, port: Port) -> OutgoingEdges {
        let node = &self.nodes[port.node];
        OutgoingEdges {
            edges: node.outgoing.cursor(),
            out: port.port,
        }
    }

    fn remove_inc_edges(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];

        let mut incoming = node.incoming.cursor();
        while let Some(edge_id) = incoming.next_(&self.edges) {
            self.remove_edge(edge_id);
        }
    }

    fn remove_out_edges(&mut self, node_id: NodeId) {
        let node = &self.nodes[node_id];

        let mut outgoing = node.outgoing.cursor();
        while let Some(edge_id) = outgoing.next_(&self.edges) {
            self.remove_edge(edge_id);
        }
    }

    fn remove_all_edges(&mut self, node_id: NodeId) {
        self.remove_inc_edges(node_id);
        self.remove_out_edges(node_id);
    }

    pub fn reconnect_all_outgoing(&mut self, old_port: Port, new_port: Port) {
        let mut outgoing = self.outgoing(old_port);

        while let Some(old_edge_id) = outgoing.next_(self) {
            let old_edge = &self.edges[old_edge_id];
            let port_out = old_edge.port_out;
            let port_in = old_edge.port_in;

            let new_edge_id = self.push_edge(new_port, port_in);

            // Remove from old_edge.port_out.outgoing
            self.nodes[port_out.node]
                .outgoing
                .remove(&mut self.edges, old_edge_id);

            // Add to new_edge.port_out.outgoing
            self.nodes[new_port.node]
                .outgoing
                .add(&mut self.edges, new_edge_id);

            // Replace in old_edge.port_in.incoming
            self.nodes[port_in.node].incoming.replace(
                &mut self.edges,
                old_edge_id,
                new_edge_id,
            );

            self.edges.swap_remove(&old_edge_id);
        }
    }

    #[inline]
    pub fn raw_nodes(&self) -> &Nodes {
        &self.nodes
    }

    #[inline]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    #[inline]
    pub fn last_node_id(&self) -> NodeId {
        self.nodes.last_idx()
    }

    #[inline]
    pub fn reserve_nodes(&mut self, additional: usize) {
        self.nodes.reserve(additional)
    }

    #[inline]
    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }

    #[inline]
    pub fn reserve_edges(&mut self, additional: usize) {
        self.edges.reserve(additional)
    }

    #[allow(dead_code)]
    pub(super) fn dump_edges(&self) {
        let mut buf = String::new();

        for (node_id, node) in &self.nodes {
            writeln!(
                &mut buf,
                "node {}: {} {}",
                node_id,
                node.incoming.dump_to_str(&self.edges),
                node.outgoing.dump_to_str(&self.edges)
            )
            .unwrap();
        }

        for (edge_id, edge) in &self.edges {
            writeln!(
                &mut buf,
                "edge {}: {} -> {}",
                edge_id, edge.port_out, edge.port_in
            )
            .unwrap();
        }

        writeln!(&mut buf).unwrap();

        debug!("\n{}", buf);
    }
}
