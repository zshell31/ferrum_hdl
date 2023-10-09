use std::{borrow::Cow, cmp};

use either::Either;
use fnv::FnvHashSet;
use smallvec::SmallVec;

use crate::{
    buffer::Buffer,
    net_kind::NetKind,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{
        BinOpNode, BitNot, BitVecMask, Case, CaseInputs, Const, DFFInputs, Expr,
        LoopStart, Merger, ModInst, MultiConst, MultiPass, Mux2, Mux2Inputs, NodeKind,
        NodeOutput, Not, Pass, Splitter, DFF,
    },
    symbol::Symbol,
    visitor::{ParamKind, Visitor},
};

pub trait Backend {}

pub struct Verilog<'n> {
    pub buffer: Buffer,
    pub locals: FnvHashSet<Symbol>,
    pub net_list: &'n NetList,
}

impl<'n> Verilog<'n> {
    pub fn new(net_list: &'n NetList) -> Self {
        Self {
            buffer: Buffer::new(),
            locals: Default::default(),
            net_list,
        }
    }

    pub fn generate(mut self) -> String {
        self.visit_modules();

        self.buffer.buffer
    }

    fn write_local(&mut self, out: &NodeOutput, init: Option<NodeOutId>) {
        if !self.locals.contains(&out.sym) {
            // TODO: don't write if node is output
            self.buffer.write_tab();
            write_out(&mut self.buffer, out);
            self.buffer.write_fmt(format_args!(" {}", out.sym));
            self.buffer.write_str(";\n");

            if let Some(init) = init {
                let node = &self.net_list[init.node_id()];
                if node.from_const {
                    let sym = out.sym;
                    let init = self.inject_const(init);

                    self.buffer.write_tab();
                    self.buffer.write_str("initial begin\n");
                    self.buffer.push_tab();

                    self.buffer.write_tab();
                    self.buffer.write_fmt(format_args!("{sym} = {init};\n"));

                    self.buffer.pop_tab();
                    self.buffer.write_tab();
                    self.buffer.write_str("end\n");
                }
            }

            self.locals.insert(out.sym);
        }
    }

    fn inject_input(&self, node_out_id: NodeOutId) -> Cow<'static, str> {
        self.inject_node(node_out_id, false)
    }

    fn inject_const(&self, node_out_id: NodeOutId) -> Cow<'static, str> {
        self.inject_node(node_out_id, true)
    }

    fn inject_node(&self, node_out_id: NodeOutId, from_const: bool) -> Cow<'static, str> {
        let node_out = &self.net_list[node_out_id];
        let node_id = node_out_id.node_id();
        let node = &self.net_list[node_id];

        let should_be_injected = if !from_const {
            node.inject || node_out.inject
        } else {
            node.from_const
        };

        if should_be_injected {
            let mut buf = Buffer::new();
            self.inject_node_(
                node_out_id.node_id().module_id(),
                node_out_id,
                &mut buf,
                false,
                from_const,
            );
            return buf.buffer.into();
        }

        self.net_list[node_out_id].sym.as_str().into()
    }

    fn inject_node_(
        &self,
        module_id: ModuleId,
        node_out_id: NodeOutId,
        expr: &mut Buffer,
        nested_expr: bool,
        from_const: bool,
    ) {
        let node_out = &self.net_list[node_out_id];
        let node_id = node_out_id.node_id();
        let node = &self.net_list[node_id];

        if !from_const {
            if !node_out.inject {
                let mod_id = node_id.module_id();
                if module_id != mod_id {
                    panic!("Cannot inject non-injectable nodes from other modules (current: {}, other module: {})", self.net_list[module_id].name, self.net_list[mod_id].name);
                }
                expr.write_str(self.net_list[node_out_id].sym.as_str());
                return;
            }
        } else if !node.from_const {
            panic!("Node {:#?} is not const", node);
        }

        if let Some(const_val) = self.net_list.to_const(node_out_id) {
            expr.write_fmt(format_args!("{const_val}"));
            return;
        }

        match &node.kind {
            NodeKind::Pass(Pass { input, .. }) => {
                self.inject_node_(module_id, *input, expr, false, from_const);
            }
            NodeKind::MultiPass(MultiPass { inputs, .. }) => {
                let input = inputs[node_out_id.out_id()];
                self.inject_node_(module_id, input, expr, false, from_const);
            }
            NodeKind::BitNot(BitNot { input, .. }) => {
                expr.write_str("~");
                self.inject_node_(module_id, *input, expr, true, from_const);
            }
            NodeKind::Not(Not { input, .. }) => {
                expr.write_str("!");
                self.inject_node_(module_id, *input, expr, true, from_const);
            }
            NodeKind::BinOp(BinOpNode {
                bin_op,
                inputs: (left, right),
                ..
            }) => {
                if nested_expr {
                    expr.write_str("( ");
                }

                self.inject_node_(module_id, *left, expr, true, from_const);
                expr.write_fmt(format_args!(" {bin_op} "));
                self.inject_node_(module_id, *right, expr, true, from_const);

                if nested_expr {
                    expr.write_str(" )");
                }
            }
            NodeKind::Splitter(
                splitter @ Splitter {
                    input,
                    outputs,
                    rev,
                    ..
                },
            ) => {
                let out_id = node_out_id.out_id();
                let mut start = splitter.start(self.net_list);
                if !rev {
                    for output in outputs.iter().take(out_id) {
                        start += output.width();
                    }
                } else {
                    for output in outputs.iter().take(out_id) {
                        start -= output.width();
                    }
                }

                self.inject_node_(module_id, *input, expr, false, from_const);
                let width = outputs[out_id].width();

                if *rev {
                    start -= width;
                }

                if width == 1 {
                    expr.write_fmt(format_args!("[{start}]"));
                } else {
                    expr.write_fmt(format_args!("[{start} +: {width}]"));
                }
            }
            NodeKind::Merger(Merger { inputs, rev, .. }) => {
                expr.write_str("{ ");
                let inputs = if !rev {
                    Either::Left(inputs.iter())
                } else {
                    Either::Right(inputs.iter().rev())
                };
                expr.intersperse(", ", inputs, |expr, input| {
                    self.inject_node_(module_id, *input, expr, false, from_const);
                });
                expr.write_str(" }");
            }
            _ => {}
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
        self.buffer
            .write_str("/* Automatically generated by Ferrum. */\n\n");

        for module_id in self.net_list.modules() {
            let module = &self.net_list[module_id];
            if module.is_skip || module.inject {
                continue;
            }
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        self.locals = Default::default();

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
                buffer.write_tab();
                write_param(net_list, buffer, input, ParamKind::Input);
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
        for node_id in module.nodes() {
            let node = &self.net_list[node_id];
            if node.is_skip || node.inject {
                continue;
            }
            self.visit_node(node_id);
        }
        self.buffer.pop_tab();

        self.buffer.write_str("endmodule\n\n");
    }

    fn visit_node(&mut self, node_id: NodeId) {
        let node = &self.net_list[node_id];

        match &node.kind {
            NodeKind::DummyInput(_) | NodeKind::Input(_) => {}
            NodeKind::ModInst(ModInst {
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
                    let input_sym = self.inject_input(*input);
                    let mod_input_sym = self.net_list[mod_input].sym;

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
                        let mod_output_sym = self.net_list[mod_output].sym;

                        buffer.write_tab();
                        buffer.write_fmt(format_args!(".{mod_output_sym}({output_sym})"));
                    },
                );

                self.buffer.write_eol();

                self.buffer.pop_tab();
                self.buffer.write_tab();
                self.buffer.write_str(");\n\n");
            }
            NodeKind::LoopStart(LoopStart {
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
            NodeKind::LoopEnd(_) => {
                self.buffer.pop_tab();
                self.buffer.write_template(format_args!(
                    r#"
end
endgenerate
                "#,
                ));
            }
            NodeKind::Expr(Expr {
                input,
                output,
                skip_output_def,
                expr,
            }) => {
                if !*skip_output_def {
                    self.write_local(output, None);
                }
                let input = self.inject_input(*input);
                let output = output.sym;

                expr(&mut self.buffer, input, output);
            }
            NodeKind::Pass(Pass { input, output }) => {
                self.write_local(output, None);
                let input = self.inject_input(*input);
                let output = output.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = {input};"));
            }
            NodeKind::MultiPass(MultiPass { inputs, outputs }) => {
                for (input, output) in inputs.iter().zip(outputs.iter()) {
                    self.write_local(output, None);
                    let input = self.inject_input(*input);
                    let output = output.sym;

                    self.buffer
                        .write_template(format_args!("assign {output} = {input};"));
                }
            }
            NodeKind::Const(Const { value, output }) => {
                self.write_local(output, None);
                let output = output.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = {value};"));
            }
            NodeKind::MultiConst(MultiConst { values, outputs }) => {
                for (value, output) in values.iter().zip(outputs.iter()) {
                    if output.is_skip {
                        continue;
                    }

                    self.write_local(output, None);
                    let output = output.sym;

                    self.buffer
                        .write_template(format_args!("assign {output} = {value};"));
                }
            }
            NodeKind::Splitter(
                splitter @ Splitter {
                    input,
                    outputs,
                    rev,
                    ..
                },
            ) => {
                let input_width = self.net_list[*input].ty.width();
                let mut start = splitter.start(self.net_list);
                let input = self.inject_input(*input);

                for output in outputs.iter() {
                    if output.is_skip {
                        continue;
                    }

                    self.write_local(output, None);

                    let width = output.ty.width();
                    let output = output.sym;

                    self.buffer.write_tab();
                    if width == 1 {
                        let start = if !*rev { start } else { start - 1 };

                        self.buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start}];\n\n"
                        ));
                    } else {
                        #[allow(clippy::collapsible_else_if)]
                        let (start, width) = if !*rev {
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
                    if !*rev {
                        start = cmp::min(start + width, input_width);
                    } else {
                        start = start.saturating_sub(width);
                    }
                }
            }
            NodeKind::Merger(Merger {
                inputs,
                output,
                rev,
            }) => {
                if !inputs.is_empty() {
                    self.write_local(output, None);
                    let output = output.sym;

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!("assign {output} = {{\n"));

                    let inputs = if !*rev {
                        Either::Left(inputs.iter())
                    } else {
                        Either::Right(inputs.iter().rev())
                    };
                    let inputs = inputs
                        .map(|input| self.inject_input(*input))
                        .collect::<SmallVec<[_; 8]>>();

                    self.buffer.push_tab();
                    self.buffer.intersperse(SEP, inputs, |buffer, input| {
                        buffer.write_tab();
                        buffer.write_fmt(format_args!("{}", input));
                    });
                    self.buffer.pop_tab();

                    self.buffer.write_eol();
                    self.buffer.write_tab();
                    self.buffer.write_str("};\n\n");
                }
            }
            NodeKind::Case(Case {
                inputs:
                    CaseInputs {
                        sel,
                        variants,
                        default,
                    },
                output,
            }) => {
                if !variants.is_empty() {
                    self.write_local(output, None);
                    let output = output.sym;

                    let sel_sym = self.inject_input(*sel);
                    let sel_width = self.net_list[*sel].ty.width();

                    let has_mask = variants.iter().any(|(mask, _)| mask.mask != 0);

                    self.buffer.write_tab();
                    self.buffer.write_fmt(format_args!("always @(*) begin\n"));

                    self.buffer.push_tab();

                    self.buffer.write_tab();

                    if !has_mask {
                        self.buffer.write_fmt(format_args!("case ({sel_sym})\n"));
                    } else {
                        self.buffer.write_fmt(format_args!("casez ({sel_sym})\n"));
                    }

                    self.buffer.push_tab();

                    let mut write_case = |mask: BitVecMask, input: NodeOutId| {
                        let input = self.inject_input(input);

                        self.buffer.write_tab();
                        let mask = mask.to_bitstr(sel_width, '?');

                        self.buffer.write_fmt(format_args!(
                            "{sel_width}'b{mask} : {output} = {input};\n",
                        ));
                    };

                    for i in 0 .. (variants.len() - 1) {
                        let (mask, input) = variants[i];
                        write_case(mask, input);
                    }

                    match default {
                        Some(default) => {
                            let (mask, input) = variants.last().unwrap();
                            write_case(*mask, *input);

                            let default = self.inject_input(*default);

                            self.buffer.write_tab();
                            self.buffer.write_fmt(format_args!(
                                "default: {output} = {default};\n"
                            ));
                        }
                        None => {
                            let (_, input) = variants.last().unwrap();
                            let default = self.inject_input(*input);

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
            NodeKind::BitNot(BitNot { input, output }) => {
                self.write_local(output, None);
                let input = self.inject_input(*input);
                let output = output.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = ~{input};",));
            }
            NodeKind::Not(Not { input, output }) => {
                self.write_local(output, None);
                let input = self.inject_input(*input);
                let output = output.sym;

                self.buffer
                    .write_template(format_args!("assign {output} = !{input};",));
            }
            NodeKind::BinOp(BinOpNode {
                bin_op,
                inputs: (left, right),
                output,
            }) => {
                self.write_local(output, None);
                let output = output.sym;

                let left = self.inject_input(*left);
                let right = self.inject_input(*right);

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!(
                    "assign {output} = {left} {bin_op} {right};\n\n"
                ));
            }
            NodeKind::Mux2(Mux2 {
                inputs:
                    Mux2Inputs {
                        sel,
                        input1,
                        input2,
                    },
                output,
            }) => {
                self.write_local(output, None);

                let sel = self.inject_input(*sel);
                let input1 = self.inject_input(*input1);
                let input2 = self.inject_input(*input2);
                let output = output.sym;

                self.buffer.write_template(format_args!(
                    "
always @(*) begin
    case ({sel})
        1'h0: 
            {output} = {input2};
        default: 
            {output} = {input1};
    endcase
end
"
                ));
            }
            NodeKind::DFF(DFF {
                inputs:
                    DFFInputs {
                        clk,
                        rst,
                        en,
                        rst_val,
                        data,
                    },
                output,
            }) => {
                self.write_local(output, Some(*rst_val));
                let output = output.sym;

                let clk = self.inject_input(*clk);
                let rst = self.inject_input(*rst);

                let data = self.inject_input(*data);
                let rst_val = self.inject_input(*rst_val);

                match en {
                    Some(en) => {
                        let en = self.inject_input(*en);

                        self.buffer.write_template(format_args!(
                            "
always @(posedge {clk} or posedge {rst}) begin
    if ({rst})
        {output} <= {rst_val};
    else if ({en})
        {output} <= {data};
end
"
                        ));
                    }
                    None => {
                        self.buffer.write_template(format_args!(
                            "
always @(posedge {clk} or posedge {rst}) begin
    if ({rst})
        {output} <= {rst_val};
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
