use std::collections::BTreeSet;

use crate::{
    buffer::Buffer,
    net_kind::NetKind,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{
        BinOpNode, BitNotNode, BitVecTrans, ConstNode, DFFNode, IsNode, Merger, ModInst,
        Mux2Node, Node, NodeOutput, NotNode, PassNode, Splitter,
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

    fn write_value(&mut self, out: &NodeOutput, value: u128) {
        let width = out.ty.width();

        self.buffer.write_fmt(format_args!("{}'d{}", width, value));
    }

    fn write_local(&mut self, out: &NodeOutput, init: Option<u128>) {
        if !self.locals.contains(&out.sym) {
            // TODO: don't write if node is output
            self.buffer.write_tab();
            write_out(&mut self.buffer, out);
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

fn write_param(
    net_list: &NetList,
    buffer: &mut Buffer,
    param: NodeOutId,
    kind: ParamKind,
) {
    let out = &net_list[param];

    buffer.write_str(match kind {
        ParamKind::Input => "input ",
        ParamKind::Output => "output ",
    });
    write_out(buffer, out);
    buffer.write_fmt(format_args!(" {}", out.sym));
}

fn write_out(buffer: &mut Buffer, out: &NodeOutput) {
    match out.kind {
        NetKind::Wire => buffer.write_str("wire"),
        NetKind::Reg => buffer.write_str("reg"),
    };

    if out.ty.width() > 1 {
        buffer.write_fmt(format_args!(" [{}:0]", out.ty.width() - 1));
    }
}

const SEP: &str = ",\n";

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

        let mut inputs = module.inputs().peekable();
        let mut has_inputs = false;
        self.buffer.push_tab();
        if inputs.peek().is_some() {
            has_inputs = true;
            self.buffer.write_tab();
            self.buffer.write_str("// Inputs\n");

            let net_list = &self.net_list;
            self.buffer.intersperse(SEP, inputs, |buffer, input| {
                let node = &module[input];
                buffer.intersperse(SEP, node.outputs().items(), |buffer, node_out_id| {
                    buffer.write_tab();
                    write_param(
                        net_list,
                        buffer,
                        node_out_id.node_out_id(input),
                        ParamKind::Input,
                    );
                });
            });
        }
        self.buffer.pop_tab();

        let mut outputs = module.outputs().peekable();
        self.buffer.push_tab();
        if outputs.peek().is_some() {
            if has_inputs {
                self.buffer.write_str(SEP);
            }
            self.buffer.write_tab();
            self.buffer.write_str("// Outputs\n");

            let net_list = &self.net_list;
            self.buffer.intersperse(SEP, outputs, |buffer, output| {
                buffer.write_tab();
                write_param(net_list, buffer, output, ParamKind::Output);
            });
        }
        self.buffer.pop_tab();

        self.buffer.write_str("\n);\n");
        self.buffer.write_eol();

        self.buffer.push_tab();
        for node in module.nodes() {
            self.visit_node(node);
        }
        self.buffer.pop_tab();

        self.buffer.write_str("endmodule\n\n");
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

                self.buffer.intersperse(
                    SEP,
                    outputs.iter().zip(module.outputs()),
                    |buffer, (output, mod_output)| {
                        let output_sym = output.sym;
                        let mod_output_sym = module[mod_output].sym;

                        buffer.write_tab();
                        buffer.write_fmt(format_args!(".{mod_output_sym}({output_sym})"));
                    },
                );

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
                outputs,
                start,
            }) => {
                let input = self.net_list[*input].sym;
                let mut start = start.unwrap_or_default();
                for output in outputs.iter() {
                    self.write_local(output, None);

                    let width = output.ty.width();
                    let output = output.sym;

                    self.buffer.write_tab();
                    if width == 1 {
                        self.buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start}];\n\n"
                        ));
                    } else {
                        self.buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start} +: {width}];\n\n"
                        ));
                    }
                    start += width;
                }
            }
            Node::Merger(Merger { inputs, output }) => {
                self.write_local(output, None);
                let output = output.sym;

                self.buffer.write_tab();
                self.buffer
                    .write_fmt(format_args!("assign {output} = {{\n"));

                let net_list = &self.net_list;
                self.buffer.push_tab();
                self.buffer
                    .intersperse(SEP, inputs.iter().rev(), |buffer, input| {
                        let input = net_list[*input].sym;
                        buffer.write_tab();
                        buffer.write_fmt(format_args!("{}", input));
                    });
                self.buffer.pop_tab();

                self.buffer.write_eol();
                self.buffer.write_tab();
                self.buffer.write_str("};\n\n");
            }
            Node::BitVecTrans(BitVecTrans {
                input,
                output,
                trans,
            }) => {
                self.write_local(output, None);
                let input = self.net_list[*input].sym;
                let output = output.sym;

                trans(&mut self.buffer, input, output);
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
