use std::collections::BTreeSet;

use crate::{
    buffer::Buffer,
    net_kind::NetKind,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{
        BinOpNode, BitNotNode, ConstNode, DFFNode, IsNode, ModInst, Mux2Node, Node,
        NodeOutput, NotNode, PassNode, Splitter,
    },
    params::Outputs,
    symbol::Symbol,
    visitor::{ParamKind, Visitor},
};

pub trait Backend {}

pub struct Verilog<'n> {
    buffer: Buffer,
    locals: BTreeSet<Symbol>,
    net_list: &'n NetList,
}

impl<'n> Verilog<'n> {
    pub fn new(net_list: &'n NetList) -> Self {
        Self {
            buffer: Buffer::new(),
            locals: BTreeSet::new(),
            net_list,
        }
    }

    pub fn generate(mut self) -> String {
        self.visit_modules();

        self.buffer.buffer
    }

    fn write_out(&mut self, out: &NodeOutput) {
        match out.kind {
            NetKind::Wire => self.buffer.write_str("wire"),
            NetKind::Reg => self.buffer.write_str("reg"),
        };

        if out.ty.width() > 1 {
            self.buffer
                .write_fmt(format_args!(" [{}:0]", out.ty.width() - 1));
        }
    }

    fn write_value(&mut self, out: &NodeOutput, value: u128) {
        let width = out.ty.width();

        self.buffer.write_fmt(format_args!("{}'d{}", width, value));
    }

    fn write_local(&mut self, out: &NodeOutput, init: Option<u128>) {
        if !self.locals.contains(&out.sym) {
            // TODO: don't write if node is output
            self.buffer.write_tab();
            self.write_out(out);
            self.buffer.write_fmt(format_args!(" {}", out.sym));
            self.buffer.write_str(";\n");

            if let Some(init) = init {
                self.buffer.write_tab();
                self.buffer.write_str("initial begin\n");
                self.buffer.push_tab();

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!("{} = ", out.sym));
                self.write_value(out, init);
                self.buffer.write_str(";\n");

                self.buffer.pop_tab();
                self.buffer.write_tab();
                self.buffer.write_str("end\n");
            }

            self.locals.insert(out.sym);
        }
    }

    fn inject_const(&mut self, node_out_id: NodeOutId) {
        let node = &self.net_list[node_out_id.node_id()];

        if let Node::Const(ConstNode {
            value,
            inject: true,
            output,
        }) = node
        {
            self.write_value(output, *value);
        } else {
            let sym = node.outputs().only_one().out.sym;
            self.buffer.write_fmt(format_args!("{}", sym));
        }
    }
}

impl<'n> Backend for Verilog<'n> {}

impl<'n> Visitor for Verilog<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.net_list.modules() {
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        self.locals = BTreeSet::new();

        let module = &self.net_list[module_id];

        self.buffer
            .write_fmt(format_args!("module {}\n(\n", module.name));
        self.buffer.push_tab();

        let mut inputs = module.inputs().peekable();
        if inputs.peek().is_some() {
            self.buffer.write_tab();
            self.buffer.write_str("// Inputs\n");

            for input in inputs {
                let node = &module[input];
                for node_out_id in node.outputs().items() {
                    self.buffer.write_tab();
                    self.visit_param(node_out_id.node_out_id(input), ParamKind::Input);
                    self.buffer.write_str(",\n");
                }
            }
        }

        let mut outputs = module.outputs().peekable();
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
        self.buffer.write_eol();

        for node in module.nodes() {
            self.visit_node(node);
        }

        self.buffer.pop_tab();

        self.buffer.write_str("endmodule\n\n");
    }

    fn visit_param(&mut self, param: NodeOutId, kind: ParamKind) {
        let out = &self.net_list[param];

        self.buffer.write_str(match kind {
            ParamKind::Input => "input ",
            ParamKind::Output => "output ",
        });
        self.write_out(out);
        self.buffer.write_fmt(format_args!(" {}", out.sym));
    }

    fn visit_node(&mut self, node_id: NodeId) {
        let node = &self.net_list[node_id];

        match node {
            Node::DummyInput(_) | Node::Input(_) => {}
            Node::ModInst(ModInst {
                name,
                module_id,
                inputs,
                outputs,
            }) => {
                let module = &self.net_list[*module_id];
                assert_eq!(inputs.len(), module.inputs_len());
                assert_eq!(outputs.len(), module.outputs_len());

                for output in outputs.iter() {
                    self.write_local(output, None);
                }

                self.buffer.write_tab();

                self.buffer
                    .write_fmt(format_args!("{} {} (\n", module.name, name));

                self.buffer.push_tab();
                if !inputs.is_empty() {
                    self.buffer.write_tab();
                    self.buffer.write_str("// Inputs\n");
                }
                for (input, mod_input) in inputs.iter().zip(module.inputs()) {
                    let input_sym = self.net_list[*input].sym;
                    let mod_input_sym = module[mod_input].outputs().only_one().out.sym;

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!(".{mod_input_sym}({input_sym}),\n"));
                }
                self.buffer.write_tab();
                self.buffer.write_str("// Outputs\n");

                for (output, mod_output) in outputs.iter().zip(module.outputs()) {
                    let output_sym = output.sym;
                    let mod_output_sym = module[mod_output].sym;

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!(".{mod_output_sym}({output_sym}),\n"));
                }
                self.buffer.pop(2);
                self.buffer.write_eol();

                self.buffer.pop_tab();
                self.buffer.write_tab();
                self.buffer.write_str(");\n\n");
            }
            Node::Pass(PassNode {
                inject,
                input,
                output,
            }) => {
                if !inject.unwrap_or_default() {
                    self.write_local(output, None);
                    let input = self.net_list[*input].sym;
                    let output = output.sym;

                    self.buffer
                        .write_template(format_args!("assign {output} = {input};"));
                }
            }
            Node::Const(ConstNode {
                value,
                inject,
                output,
            }) => {
                if !inject {
                    self.write_local(output, None);
                    let output = output.sym;

                    self.buffer
                        .write_template(format_args!("assign {output} = {value};"));
                }
            }
            Node::Splitter(Splitter {
                input,
                start,
                width,
                output,
            }) => {
                self.write_local(output, None);
                let input = self.net_list[*input].sym;
                let output = output.sym;

                self.buffer.write_tab();
                self.buffer
                    .write_fmt(format_args!("assign {output} = {input}"));

                let width = *width;
                let start = *start;
                if width == 1 {
                    self.buffer.write_fmt(format_args!("[{}]", start));
                } else {
                    self.buffer.write_fmt(format_args!(
                        "[{}:{}]",
                        start + width - 1,
                        start
                    ));
                }
                self.buffer.write_str(";\n\n");
            }
            Node::BitNot(BitNotNode { input, output }) => {
                self.write_local(output, None);
                let input = self.net_list[*input].sym;
                let output = output.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = ~{input};",));
            }
            Node::Not(NotNode { input, output }) => {
                self.write_local(output, None);
                let input = self.net_list[*input].sym;
                let output = output.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = !{input};",));
            }
            Node::BinOp(BinOpNode {
                bin_op,
                inputs: (input1, input2),
                output,
            }) => {
                self.write_local(output, None);
                let output = output.sym;

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!("assign {output} = "));

                self.inject_const(*input1);

                self.buffer.write_fmt(format_args!(" {bin_op} "));

                self.inject_const(*input2);

                self.buffer.write_str(";\n\n");
            }
            Node::Mux2(Mux2Node {
                sel,
                inputs: (input1, input2),
                output,
            }) => {
                self.write_local(output, None);

                let sel = self.net_list[*sel].sym;
                let input1 = self.net_list[*input1].sym;
                let input2 = self.net_list[*input2].sym;
                let output = output.sym;

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
                inputs: (clk, rst_val, data),
                output,
            }) => {
                let init = if let Node::Const(ConstNode {
                    value,
                    inject: true,
                    ..
                }) = &self.net_list[rst_val.node_id()]
                {
                    Some(*value)
                } else {
                    None
                };
                self.write_local(output, init);

                let clk = self.net_list[*clk].sym;
                let data = self.net_list[*data].sym;
                let output = output.sym;

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
