use std::collections::{BTreeSet, HashMap};

use crate::{
    buffer::Buffer,
    index::{Index, NodeIndex},
    net_kind::NetKind,
    node::{
        BitAndNode, BitNotNode, BitOrNode, ConstNode, DFFNode, InputNode, IsNode,
        Mux2Node, Node, NotNode, PassNode,
    },
    output::{IsOneOutput, NodeOutput, Outputs},
    symbol::Symbol,
};

#[derive(Debug)]
pub struct NetList {
    pub name: Symbol,
    pub inputs: BTreeSet<NodeIndex>,
    pub outputs: BTreeSet<NodeIndex>,
    nodes: Vec<Node>,
}

impl NetList {
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            inputs: BTreeSet::new(),
            outputs: BTreeSet::new(),
            nodes: Vec::with_capacity(16),
        }
    }

    pub fn add_node<N: IsNode<Self>>(&mut self, node: N) -> NodeIndex
    where
        N::Outputs: IsOneOutput,
    {
        let outputs = <N::Outputs as Outputs<Self>>::make_node_idx(self);
        self.add_node_inner(node);

        outputs[0]
    }

    pub fn add_dummy_node(&mut self, node: InputNode) -> NodeIndex
    where
        <InputNode as IsNode<Self>>::Outputs: IsOneOutput,
    {
        let outputs =
            <<InputNode as IsNode<Self>>::Outputs as Outputs<Self>>::make_node_idx(self);
        self.add_node_inner(Node::DummyInput(node));

        outputs[0]
    }

    pub fn replace<N: IsNode<Self>>(&mut self, node_index: NodeIndex, node: N) {
        if self.node(node_index).is_input() {
            self.inputs.remove(&node_index);
        }
        let node: Node = node.into();
        if node.is_input() {
            self.inputs.insert(node_index);
        }

        self.nodes[node_index.0] = node;
    }

    fn add_node_inner(&mut self, node: impl Into<Node>) {
        let node: Node = node.into();
        if node.is_input() {
            let node_idx = self.index(0);
            self.inputs.insert(node_idx);
        }

        self.nodes.push(node);
    }

    pub fn add_output_node(&mut self, node_index: NodeIndex) {
        self.outputs.insert(node_index);
    }

    pub fn node(&self, node_index: NodeIndex) -> &Node {
        &self.nodes[node_index.0]
    }

    pub fn node_mut(&mut self, node_index: NodeIndex) -> &mut Node {
        &mut self.nodes[node_index.0]
    }

    pub fn node_output(&self, node_index: NodeIndex) -> &NodeOutput {
        let node = self.node(node_index);
        node.node_output::<Self>(node_index.1)
    }

    pub fn node_output_mut(&mut self, node_index: NodeIndex) -> &mut NodeOutput {
        let node = self.node_mut(node_index);
        node.node_output_mut::<Self>(node_index.1)
    }

    pub fn find_node(
        &self,
        node_index: NodeIndex,
        f: impl Fn(&Node) -> bool + Copy,
    ) -> Vec<NodeIndex> {
        let mut res = vec![];
        self.find_node_inner(node_index, f, &mut res);
        res
    }

    fn find_node_inner(
        &self,
        node_index: NodeIndex,
        f: impl Fn(&Node) -> bool + Copy,
        res: &mut Vec<NodeIndex>,
    ) {
        let node = self.node(node_index);
        if f(node) {
            res.push(node_index)
        }

        for input in node.inputs::<Self>() {
            self.find_node_inner(input, f, res);
        }
    }

    pub fn find_dummy_inputs(&self, node_index: NodeIndex) -> Vec<NodeIndex> {
        self.find_node(node_index, |node| node.is_dummy_input())
    }
}

impl Index for NetList {
    fn index(&mut self, out: u8) -> NodeIndex {
        NodeIndex(self.nodes.len(), out)
    }
}

impl NetList {
    pub fn verilog(&self) -> String {
        let mut generator = Generator::new(self);

        let mut declarations = Buffer::new();

        declarations.write_fmt(format_args!("module {}\n(\n", self.name));

        declarations.push_ident();
        if !self.inputs.is_empty() {
            declarations.write_ident();
            declarations.write_str("// Inputs\n");

            generator.synthesize_inputs(
                &mut declarations,
                self.inputs.iter().map(|input| *self.node_output(*input)),
                !self.outputs.is_empty(),
            );
        }

        if !self.outputs.is_empty() {
            declarations.write_ident();
            declarations.write_str("// Outputs\n");

            generator.synthesize_outputs(
                &mut declarations,
                self.outputs.iter().map(|output| *self.node_output(*output)),
            );
        }
        declarations.pop_ident();
        declarations.write_str(");\n");

        declarations.push_ident();

        let mut expressions = Buffer::new_with_ident(declarations.ident);
        for node in &self.nodes {
            generator.synthesize(node, &mut expressions);
        }

        generator.synthesize_locals(&mut declarations);

        declarations.write_eol();
        declarations.extend(expressions);

        declarations.pop_ident();

        declarations.write_str("\nendmodule\n");

        declarations.buffer
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    Input,
    Output,
}

pub struct Generator<'a> {
    net_list: &'a NetList,
    locals: HashMap<Symbol, (NodeOutput, Option<u128>)>,
}

impl<'a> Generator<'a> {
    pub fn new(net_list: &'a NetList) -> Self {
        Self {
            net_list,
            locals: HashMap::new(),
        }
    }

    fn add_local(&mut self, out: NodeOutput, init: Option<u128>) {
        // TODO: check if local is already defined with different net kind
        self.locals.entry(out.sym).or_insert((out, init));
    }

    pub fn synthesize(&mut self, node: &Node, buffer: &mut Buffer) {
        match node {
            Node::DummyInput(_) | Node::Input(_) => {}
            Node::Pass(PassNode { input, out }) => {
                self.add_local(*out, None);
                let input = self.net_list.node_output(*input).sym;
                let output = out.sym;

                buffer.write_template(format_args!("assign {output} = {input};"));
            }
            Node::Const(ConstNode { value, out }) => {
                self.add_local(*out, None);
                let output = out.sym;

                buffer.write_template(format_args!("assign {output} = {value};"));
            }
            Node::BitNot(BitNotNode { input, out }) => {
                self.add_local(*out, None);
                let input = self.net_list.node_output(*input).sym;
                let output = out.sym;

                buffer.write_template(format_args!("assign {output} = ~{input};",));
            }
            Node::Not(NotNode { input, out }) => {
                self.add_local(*out, None);
                let input = self.net_list.node_output(*input).sym;
                let output = out.sym;

                buffer.write_template(format_args!("assign {output} = !{input};",));
            }
            Node::BitAnd(BitAndNode {
                input1,
                input2,
                out,
            }) => {
                self.add_local(*out, None);
                let input1 = self.net_list.node_output(*input1).sym;
                let input2 = self.net_list.node_output(*input2).sym;
                let output = out.sym;

                buffer.write_template(format_args!(
                    "assign {output} = {input1} & {input2};",
                ));
            }
            Node::BitOr(BitOrNode {
                input1,
                input2,
                out,
            }) => {
                self.add_local(*out, None);
                let input1 = self.net_list.node_output(*input1).sym;
                let input2 = self.net_list.node_output(*input2).sym;
                let output = out.sym;

                buffer.write_template(format_args!(
                    "assign {output} = {input1} | {input2};",
                ));
            }
            Node::Mux2(Mux2Node {
                sel,
                input1,
                input2,
                out,
            }) => {
                self.add_local(*out, None);

                let sel = self.net_list.node_output(*sel).sym;
                let input1 = self.net_list.node_output(*input1).sym;
                let input2 = self.net_list.node_output(*input2).sym;
                let output = out.sym;

                buffer.write_template(format_args!(
                    "
always @ ({sel} or {input1} or {input2}) begin
    case ({sel})
        1'h0: {output} = {input1};
        1'h1: {output} = {input2};
        default:
            {output} = 'h0;
    endcase
end
"
                ));
            }
            Node::DFF(DFFNode {
                clk,
                rst_value,
                data,
                out,
            }) => {
                let init = if let Node::Const(ConstNode { value, .. }) =
                    self.net_list.node(*rst_value)
                {
                    Some(*value)
                } else {
                    None
                };
                self.add_local(*out, init);

                let clk = self.net_list.node_output(*clk).sym;
                let data = data.expect("data is not initialized in dff");
                let data = self.net_list.node_output(data).sym;
                let output = out.sym;

                buffer.write_template(format_args!(
                    "
always @ (posedge {clk}) begin
    {output} <= {data};
end
"
                ));
            }
        }
    }

    fn synthesize_decl<E>(
        &self,
        buffer: &mut Buffer,
        params: impl IntoIterator<Item = (NodeOutput, E)>,
        mut before: impl FnMut(&mut Buffer, &(NodeOutput, E)),
        mut after: impl FnMut(&mut Buffer, &(NodeOutput, E)),
    ) {
        for item in params {
            before(buffer, &item);

            buffer.write_str(match item.0.kind {
                NetKind::Wire => "wire ",
                NetKind::Reg => "reg ",
            });

            if item.0.ty.width() > 1 {
                buffer.write_fmt(format_args!("[{}:0] ", item.0.ty.width() - 1));
            }

            buffer.write_fmt(format_args!("{}", item.0.sym));

            after(buffer, &item);
        }
    }

    pub fn synthesize_locals(&self, buffer: &mut Buffer) {
        self.synthesize_decl(
            buffer,
            self.locals.values().copied(),
            |buffer, _| {
                buffer.write_ident();
            },
            |buffer, (_, init)| {
                if let Some(init) = init {
                    buffer.write_fmt(format_args!(" = {}", init));
                }
                buffer.write_char(';');
                buffer.write_eol();
            },
        )
    }

    fn synthesize_params(
        &self,
        buffer: &mut Buffer,
        kind: ParamKind,
        params: impl IntoIterator<Item = NodeOutput>,
    ) {
        let mut first_param = true;
        self.synthesize_decl::<()>(
            buffer,
            params.into_iter().map(|param| (param, ())),
            |buffer, _| {
                if !first_param {
                    buffer.write_str(",\n");
                } else {
                    first_param = false;
                }

                buffer.write_ident();
                buffer.write_str(match kind {
                    ParamKind::Input => "input ",
                    ParamKind::Output => "output ",
                });
            },
            |_, _| {},
        )
    }

    pub fn synthesize_inputs(
        &self,
        buffer: &mut Buffer,
        inputs: impl IntoIterator<Item = NodeOutput>,
        has_outputs: bool,
    ) {
        self.synthesize_params(buffer, ParamKind::Input, inputs);
        if has_outputs {
            buffer.write_char(',')
        }
        buffer.write_eol();
    }

    pub fn synthesize_outputs(
        &self,
        buffer: &mut Buffer,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) {
        self.synthesize_params(buffer, ParamKind::Output, outputs);
        buffer.write_eol();
    }
}
