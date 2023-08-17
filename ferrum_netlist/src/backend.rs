use std::collections::BTreeSet;

use crate::{
    buffer::Buffer,
    module::{Module, ModuleList},
    net_kind::NetKind,
    net_list::NetList,
    node::{
        BitAndNode, BitNotNode, BitOrNode, ConstNode, DFFNode, Mux2Node, Node, NotNode,
        PassNode,
    },
    output::NodeOutput,
    symbol::Symbol,
};

pub trait Backend {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    Input,
    Output,
}

pub trait Visitor {
    fn visit_modules(&mut self, modules: &ModuleList) {
        for module in modules.iter() {
            self.visit_module(modules, module);
        }
    }

    fn visit_module(&mut self, modules: &ModuleList, module: &Module);

    fn visit_param(&mut self, param: &Node, kind: ParamKind);

    fn visit_node(&mut self, modules: &ModuleList, net_list: &NetList, node: &Node);
}

#[derive(Default)]
pub struct Verilog {
    buffer: Buffer,
    locals: BTreeSet<Symbol>,
}

impl Verilog {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn generate(mut self, modules: &ModuleList) -> String {
        self.visit_modules(modules);

        self.buffer.buffer
    }

    fn write_net_kind(&mut self, net_kind: NetKind) {
        match net_kind {
            NetKind::Wire => self.buffer.write_str("wire"),
            NetKind::Reg => self.buffer.write_str("reg"),
        };
    }

    fn write_local(&mut self, out: NodeOutput, init: Option<u128>) {
        if !self.locals.contains(&out.sym) {
            self.buffer.write_tab();
            self.write_net_kind(out.kind);
            self.buffer.write_fmt(format_args!(" {}", out.sym));

            if let Some(init) = init {
                self.buffer.write_fmt(format_args!(" = {}", init));
            }

            self.buffer.write_str(";\n");

            self.locals.insert(out.sym);
        }
    }
}

impl Backend for Verilog {}

impl Visitor for Verilog {
    fn visit_module(&mut self, modules: &ModuleList, module: &Module) {
        self.locals = BTreeSet::new();

        self.buffer
            .write_fmt(format_args!("module {}\n(\n", module.name));
        self.buffer.push_tab();

        let mut inputs = module.net_list.inputs().peekable();
        if inputs.peek().is_some() {
            self.buffer.write_tab();
            self.buffer.write_str("// Inputs\n");

            for input in inputs {
                self.buffer.write_tab();
                self.visit_param(input, ParamKind::Input);
                self.buffer.write_str(",\n");
            }
        }

        let mut outputs = module.net_list.outputs().peekable();
        if outputs.peek().is_some() {
            self.buffer.write_tab();
            self.buffer.write_str("// Outputs\n");

            for output in outputs {
                self.buffer.write_tab();
                self.visit_param(output, ParamKind::Output);
                self.buffer.write_str(",\n");
            }
        }

        self.buffer.pop(2);
        self.buffer.pop_tab();
        self.buffer.write_str("\n);\n");

        self.buffer.push_tab();

        for node in module.net_list.nodes() {
            self.visit_node(modules, &module.net_list, node);
        }

        self.buffer.pop_tab();

        self.buffer.write_str("\nendmodule\n");
    }

    fn visit_param(&mut self, param: &Node, kind: ParamKind) {
        let out = param.node_output(0);

        self.buffer.write_str(match kind {
            ParamKind::Input => "input ",
            ParamKind::Output => "output ",
        });
        self.write_net_kind(out.kind);
        self.buffer.write_fmt(format_args!(" {}", out.sym));
    }

    fn visit_node(&mut self, _: &ModuleList, net_list: &NetList, node: &Node) {
        match node {
            Node::DummyInput(_) | Node::Input(_) => {}
            Node::Pass(PassNode { input, out }) => {
                self.write_local(*out, None);
                let input = net_list.node_output(*input).sym;
                let output = out.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = {input};"));
            }
            Node::Const(ConstNode { value, out }) => {
                self.write_local(*out, None);
                let output = out.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = {value};"));
            }
            Node::BitNot(BitNotNode { input, out }) => {
                self.write_local(*out, None);
                let input = net_list.node_output(*input).sym;
                let output = out.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = ~{input};",));
            }
            Node::Not(NotNode { input, out }) => {
                self.write_local(*out, None);
                let input = net_list.node_output(*input).sym;
                let output = out.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = !{input};",));
            }
            Node::BitAnd(BitAndNode {
                input1,
                input2,
                out,
            }) => {
                self.write_local(*out, None);
                let input1 = net_list.node_output(*input1).sym;
                let input2 = net_list.node_output(*input2).sym;
                let output = out.sym;

                self.buffer.write_template(format_args!(
                    "assign {output} = {input1} & {input2};",
                ));
            }
            Node::BitOr(BitOrNode {
                input1,
                input2,
                out,
            }) => {
                self.write_local(*out, None);
                let input1 = net_list.node_output(*input1).sym;
                let input2 = net_list.node_output(*input2).sym;
                let output = out.sym;

                self.buffer.write_template(format_args!(
                    "assign {output} = {input1} | {input2};",
                ));
            }
            Node::Mux2(Mux2Node {
                sel,
                input1,
                input2,
                out,
            }) => {
                self.write_local(*out, None);

                let sel = net_list.node_output(*sel).sym;
                let input1 = net_list.node_output(*input1).sym;
                let input2 = net_list.node_output(*input2).sym;
                let output = out.sym;

                self.buffer.write_template(format_args!(
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
                    net_list.node(*rst_value)
                {
                    Some(*value)
                } else {
                    None
                };
                self.write_local(*out, init);

                let clk = net_list.node_output(*clk).sym;
                let data = data.expect("data is not initialized in dff");
                let data = net_list.node_output(data).sym;
                let output = out.sym;

                self.buffer.write_template(format_args!(
                    "
always @ (posedge {clk}) begin
    {output} <= {data};
end
"
                ));
            }
        }
    }
}
