use std::{cmp, collections::BTreeSet};

use either::Either;

use crate::{
    buffer::Buffer,
    net_kind::NetKind,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{
        BinOpNode, BitNotNode, BitVecMask, BitVecTrans, Case, ConstNode, DFFNode, Expr,
        IsNode, LoopStart, Merger, ModInst, Mux2Node, Node, NodeOutput, NotNode,
        PassNode, Splitter,
    },
    params::Outputs,
    symbol::Symbol,
    visitor::{ParamKind, Visitor},
};

pub trait Backend {}

pub struct Verilog<'n> {
    pub buffer: Buffer,
    pub locals: BTreeSet<Symbol>,
    pub net_list: &'n NetList,
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
            skip: true,
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
                println!("{}", module.name);
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
            Node::LoopStart(LoopStart {
                genvar,
                count,
                output,
            }) => {
                if let Some(output) = output {
                    self.write_local(output, None);
                }

                self.buffer.write_template(format_args!(
                    r#"
genvar {genvar};
generate
for ({genvar} = 0; {genvar} < {count}; {genvar} = {genvar} + 1) begin
"#
                ));
                self.buffer.push_tab();
            }
            Node::LoopEnd(_) => {
                self.buffer.pop_tab();
                self.buffer.write_template(format_args!(
                    r#"
end
endgenerate
                "#,
                ));
            }
            Node::Expr(Expr {
                input,
                output,
                skip_output_def,
                expr,
            }) => {
                if !skip_output_def {
                    self.write_local(output, None);
                }
                let input = self.net_list[*input].sym;
                let output = output.sym;

                expr(&mut self.buffer, input, output);
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
                output,
                skip,
            }) => {
                if !skip {
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
                rev,
            }) => {
                let input = self.net_list[*input];
                let input_width = input.ty.width();
                let mut start =
                    start.unwrap_or_else(|| if !rev { 0 } else { input_width });
                let input = input.sym;

                for output in outputs.iter() {
                    self.write_local(output, None);

                    let width = output.ty.width();
                    let output = output.sym;

                    self.buffer.write_tab();
                    if width == 1 {
                        let start = if !rev { start } else { start - 1 };

                        self.buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start}];\n\n"
                        ));
                    } else {
                        #[allow(clippy::collapsible_else_if)]
                        let (start, width) = if !rev {
                            if start <= input_width - width {
                                (start, width)
                            } else {
                                (start, input_width - start)
                            }
                        } else {
                            if start >= width {
                                (start - width, width)
                            } else {
                                (0, start)
                            }
                        };
                        if width == 0 {
                            continue;
                        }

                        self.buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start} +: {width}];\n\n"
                        ));
                    }
                    if !rev {
                        start = cmp::min(start + width, input_width);
                    } else {
                        start = start.saturating_sub(width);
                    }
                }
            }
            Node::Merger(Merger {
                inputs,
                output,
                rev,
            }) => {
                self.write_local(output, None);
                let output = output.sym;

                self.buffer.write_tab();
                self.buffer
                    .write_fmt(format_args!("assign {output} = {{\n"));

                let net_list = &self.net_list;
                let inputs = if !rev {
                    Either::Left(inputs.iter())
                } else {
                    Either::Right(inputs.iter().rev())
                };

                self.buffer.push_tab();
                self.buffer.intersperse(SEP, inputs, |buffer, input| {
                    let input = net_list[*input].sym;
                    buffer.write_tab();
                    buffer.write_fmt(format_args!("{}", input));
                });
                self.buffer.pop_tab();

                self.buffer.write_eol();
                self.buffer.write_tab();
                self.buffer.write_str("};\n\n");
            }
            Node::Case(Case {
                inputs: (sel, inputs, default),
                output,
            }) => {
                if !inputs.is_empty() {
                    self.write_local(output, None);
                    let output = output.sym;

                    let sel_sym = self.net_list[*sel].sym;
                    let sel_width = self.net_list[*sel].ty.width();

                    let has_mask = inputs.iter().any(|(mask, _)| mask.mask != 0);
                    let small_mask = inputs
                        .iter()
                        .map(|(mask, _)| mask.mask)
                        .min()
                        .unwrap_or_default();
                    let small_mask_ones = small_mask.trailing_ones();

                    self.buffer.write_tab();
                    self.buffer.write_fmt(format_args!("always @(*) begin\n"));

                    self.buffer.push_tab();

                    self.buffer.write_tab();

                    if !has_mask {
                        self.buffer.write_fmt(format_args!("case ({sel_sym})\n"));
                    } else if small_mask != 0 && small_mask_ones > 0 {
                        let end = sel_width - 1;
                        let start = small_mask_ones;
                        self.buffer.write_fmt(format_args!(
                            "casez ({sel_sym}[{end}:{start}])\n"
                        ));
                    } else {
                        self.buffer.write_fmt(format_args!("casez ({sel_sym})\n"));
                    }

                    self.buffer.push_tab();

                    let mut write_case = |mut mask: BitVecMask, input: NodeOutId| {
                        let input = self.net_list[input].sym;

                        self.buffer.write_tab();
                        if small_mask != 0 && small_mask_ones > 0 {
                            let ones = small_mask_ones as u128;
                            let width = sel_width - ones;
                            mask.shiftr(ones);
                            let mask = mask.to_bitstr(width, '?');
                            self.buffer.write_fmt(format_args!(
                                "{width}'b{mask} : {output} = {input};\n"
                            ));
                        } else {
                            let mask = mask.to_bitstr(sel_width, '?');

                            self.buffer.write_fmt(format_args!(
                                "{sel_width}'b{mask} : {output} = {input};\n",
                            ));
                        }
                    };

                    for i in 0 .. (inputs.len() - 1) {
                        let (mask, input) = inputs[i];
                        write_case(mask, input);
                    }

                    match default {
                        Some(default) => {
                            let (mask, input) = inputs.last().unwrap();
                            write_case(*mask, *input);

                            let default = self.net_list[*default].sym;

                            self.buffer.write_tab();
                            self.buffer.write_fmt(format_args!(
                                "default: {output} = {default};\n"
                            ));
                        }
                        None => {
                            let (_, input) = inputs.last().unwrap();
                            let default = self.net_list[*input].sym;

                            self.buffer.write_tab();
                            self.buffer.write_fmt(format_args!(
                                "default: {output} = {default};\n"
                            ));
                        }
                    };

                    self.buffer.pop_tab();

                    self.buffer.write_tab();
                    self.buffer.write_str("endcase\n");

                    self.buffer.pop_tab();

                    self.buffer.write_tab();
                    self.buffer.write_str("end\n\n");
                }
            }
            Node::BitVecTrans(BitVecTrans {
                input,
                output,
                trans,
            }) => {
                self.write_local(output, None);
                let input = self.net_list[*input].sym;
                let output = output.sym;

                trans(self, input, output);
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
                inputs: (sel, (input1, input2)),
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
        1'h0: {output} = {input2};
        1'h1: {output} = {input1};
        default:
            {output} = 'h0;
    endcase
end
"
                ));
            }
            Node::DFF(DFFNode {
                inputs: (clk, data, rst, en),
                output,
                rst_val,
            }) => {
                self.write_local(output, Some(*rst_val));

                let clk = self.net_list[*clk].sym;
                let data = self.net_list[*data].sym;
                let rst = self.net_list[*rst].sym;
                let width = output.ty.width();
                let output = output.sym;

                match en {
                    Some(en) => {
                        let en = self.net_list[*en].sym;

                        self.buffer.write_template(format_args!(
                            "
always @ (posedge {clk} or posedge {rst}) begin
    if ({rst})
        {output} <= {width}'d{rst_val};
    else if ({en})
        {output} <= {data};
end
"
                        ));
                    }
                    None => {
                        self.buffer.write_template(format_args!(
                            "
always @ (posedge {clk} or posedge {rst}) begin
    if ({rst})
        {output} <= {width}'d{rst_val};
    else
        {output} <= {data};
end
"
                        ));
                    }
                }
            }
        }
    }
}
