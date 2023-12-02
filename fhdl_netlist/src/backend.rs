use std::{borrow::Cow, cmp};

use either::Either;
use fnv::FnvHashSet;
use smallvec::SmallVec;

use crate::{
    buffer::Buffer,
    bvm::BitVecMask,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{
        BinOpNode, BitNot, Case, CaseInputs, Const, DFFInputs, Merger, ModInst,
        MultiConst, Mux2, Mux2Inputs, NetKind, NodeKind, NodeOutput, Not, Splitter,
        ZeroExtend, DFF,
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

    fn write_locals(&mut self, node_id: NodeId) {
        for out in self.net_list[node_id].outputs() {
            if out.is_skip || out.inject {
                continue;
            }
            let is_input = self.net_list.is_input(node_id);
            let is_output = self.net_list.is_output(out.node_out_id());
            let sym = out.sym.unwrap();

            if !self.locals.contains(&sym) {
                if !(is_input || is_output) {
                    self.buffer.write_tab();
                    write_out(&mut self.buffer, *out);
                    self.buffer.write_fmt(format_args!(" {}", sym));
                    self.buffer.write_str(";\n");
                }

                if let NetKind::Reg(init) = &out.kind {
                    let node = &self.net_list[init.node_id()];
                    if node.is_const() {
                        let init = self.inject_input(*init);

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

                self.locals.insert(sym);
            }
        }
    }

    fn inject_input(&self, node_out_id: NodeOutId) -> Cow<'static, str> {
        let node_out = &self.net_list[node_out_id];
        let node_id = node_out_id.node_id();
        let node = &self.net_list[node_id];

        let should_be_injected = node.inject || node_out.inject;

        if should_be_injected {
            let mut buf = Buffer::new();
            self.inject_node(
                node_out_id.node_id().module_id(),
                node_out_id,
                &mut buf,
                false,
            );
            return buf.buffer.into();
        }

        self.net_list[node_out_id].sym.unwrap().as_str().into()
    }

    fn inject_node(
        &self,
        module_id: ModuleId,
        node_out_id: NodeOutId,
        expr: &mut Buffer,
        nested_expr: bool,
    ) {
        let node_out = &self.net_list[node_out_id];
        let node_id = node_out_id.node_id();
        let node = &self.net_list[node_id];

        if !node_out.inject {
            let mod_id = node_id.module_id();
            if module_id != mod_id {
                panic!("Cannot inject non-injectable nodes from other modules (current: {}, other module: {})", 
                    self.net_list[module_id].name, self.net_list[node_id.module_id()].name);
            }
            expr.write_str(self.net_list[node_out_id].sym.unwrap().as_str());
            return;
        }

        if let Some(const_val) = self.net_list.to_const(node_out_id) {
            expr.write_fmt(format_args!("{const_val}"));
            return;
        }

        match &*node.kind {
            NodeKind::BitNot(BitNot { input, .. }) => {
                expr.write_str("~");
                self.inject_node(module_id, *input, expr, true);
            }
            NodeKind::Not(Not { input, .. }) => {
                expr.write_str("!");
                self.inject_node(module_id, *input, expr, true);
            }
            NodeKind::BinOp(BinOpNode {
                bin_op,
                inputs: (left, right),
                ..
            }) => {
                if nested_expr {
                    expr.write_str("( ");
                }

                self.inject_node(module_id, *left, expr, true);
                expr.write_fmt(format_args!(" {bin_op} "));
                self.inject_node(module_id, *right, expr, true);

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

                self.inject_node(module_id, *input, expr, false);
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
                    self.inject_node(module_id, *input, expr, false);
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
    let is_mod_output = net_list.is_output(param);
    let is_input = net_list[param.node_id()].is_input();

    let kind = match kind {
        ParamKind::Input if !is_mod_output => "input ",
        ParamKind::Input if is_mod_output => "inout ",
        ParamKind::Output if !is_input => "output ",
        _ => {
            return;
        }
    };

    buffer.write_str(kind);
    write_out(buffer, out);
    buffer.write_fmt(format_args!(" {}", out.sym.unwrap()));
}

fn write_out(buffer: &mut Buffer, out: &NodeOutput) {
    match &out.kind {
        NetKind::Wire => buffer.write_str("wire"),
        NetKind::Reg(_) => buffer.write_str("reg"),
    };

    if out.ty.width() > 1 {
        buffer.write_fmt(format_args!(" [{}:0]", out.ty.width() - 1));
    }
}

const SEP: &str = ",\n";

impl<'n> Visitor for Verilog<'n> {
    fn visit_modules(&mut self) {
        self.buffer
            .write_str("/* Automatically generated by Ferrum HDL. */\n\n");

        for module_id in self.net_list.modules() {
            let module = &self.net_list[module_id];
            if module.is_skip || module.inject {
                continue;
            }
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, mod_id: ModuleId) {
        self.locals = Default::default();

        let module = &self.net_list[mod_id];

        self.buffer
            .write_fmt(format_args!("module {}\n(\n", module.name));

        let mut inputs = self.net_list.mod_inputs(mod_id).peekable();
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

        let mut outputs = self.net_list.mod_outputs(mod_id).peekable();
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
        let mut cursor = self.net_list.mod_cursor(mod_id);
        while let Some(node_id) = self.net_list.next(&mut cursor) {
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
        self.write_locals(node_id);

        let node = &self.net_list[node_id];
        match &*node.kind {
            NodeKind::Input(_) => {}
            NodeKind::ModInst(ModInst {
                name,
                module_id,
                inputs,
                outputs,
                ..
            }) => {
                let module = &self.net_list[*module_id];
                assert_eq!(inputs.len(), module.inputs_len());
                assert_eq!(outputs.len(), module.outputs_len());

                self.buffer.write_tab();

                self.buffer.write_fmt(format_args!(
                    "{} {} (\n",
                    module.name,
                    name.unwrap()
                ));

                self.buffer.push_tab();
                if !inputs.is_empty() {
                    self.buffer.write_tab();
                    self.buffer.write_str("// Inputs\n");
                }
                for (input, mod_input) in
                    inputs.iter().zip(self.net_list.mod_inputs(*module_id))
                {
                    let input_sym = self.inject_input(*input);
                    let mod_input_sym = self.net_list[mod_input].sym.unwrap();

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!(".{mod_input_sym}({input_sym}),\n"));
                }
                self.buffer.write_tab();
                self.buffer.write_str("// Outputs\n");

                self.buffer.intersperse(
                    SEP,
                    outputs.iter().zip(self.net_list.mod_outputs(*module_id)),
                    |buffer, (output, mod_output)| {
                        let output_sym = output.sym.unwrap();
                        let mod_output_sym = self.net_list[mod_output].sym.unwrap();

                        buffer.write_tab();
                        buffer.write_fmt(format_args!(".{mod_output_sym}({output_sym})"));
                    },
                );

                self.buffer.write_eol();

                self.buffer.pop_tab();
                self.buffer.write_tab();
                self.buffer.write_str(");\n\n");
            }
            //             NodeKind::LoopStart(LoopStart { genvar, count, .. }) => {
            //                 self.buffer.write_template(format_args!(
            //                     r#"
            // genvar {genvar};
            // generate
            // for ({genvar} = 0; {genvar} < {count}; {genvar} = {genvar} + 1) begin
            // "#
            //                 ));
            //                 self.buffer.push_tab();
            //             }
            //             NodeKind::LoopEnd(_) => {
            //                 self.buffer.pop_tab();
            //                 self.buffer.write_template(format_args!(
            //                     r#"
            // end
            // endgenerate
            //                 "#,
            //                 ));
            //             }
            // NodeKind::Expr(Expr {
            //     input,
            //     output,
            //     skip_output_def,
            //     expr,
            // }) => {
            //     if !*skip_output_def {
            //         self.write_local(output, None);
            //     }
            //     let input = self.inject_input(*input);
            //     let output = output.sym;

            //     expr(&mut self.buffer, input, output);
            // }
            NodeKind::Const(Const { value, output }) => {
                let output = output.sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = {value};"));
            }
            NodeKind::MultiConst(MultiConst { values, outputs }) => {
                for (value, output) in values.iter().zip(outputs.iter()) {
                    if output.is_skip {
                        continue;
                    }

                    let output = output.sym.unwrap();

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
                    let width = output.ty.width();

                    if !(output.is_skip || output.inject) {
                        let output = output.sym.unwrap();

                        self.buffer.write_tab();
                        if width == 1 {
                            let start = if !*rev { start } else { start - 1 };

                            self.buffer.write_fmt(format_args!(
                                "assign {output} = {input}[{start}];\n\n"
                            ));
                        } else {
                            #[allow(clippy::collapsible_else_if)]
                            let (start, width) = if !*rev {
                                if start <= input_width.saturating_sub(width) {
                                    (start, width)
                                } else {
                                    (start, input_width.saturating_sub(start))
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
                    let output = output.sym.unwrap();

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
            NodeKind::ZeroExtend(ZeroExtend { input, output }) => {
                let input = self.inject_input(*input);
                let output = output.sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = {{ 0, {input} }};"));
            }
            NodeKind::Case(node @ Case { output, .. }) => {
                if !node.is_empty() {
                    let CaseInputs {
                        sel,
                        default,
                        variant_inputs,
                        variants,
                    } = node.case_inputs();

                    let output = output.sym.unwrap();

                    let sel_sym = self.inject_input(sel);
                    let sel_width = self.net_list[sel].ty.width();

                    let has_mask = variants.iter().any(|mask| mask.mask != 0);

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
                        let mask = variants[i];
                        let input = variant_inputs[i];
                        write_case(mask, input);
                    }

                    match default {
                        Some(default) => {
                            let mask = variants.last().unwrap();
                            let input = variant_inputs.last().unwrap();
                            write_case(*mask, *input);

                            let default = self.inject_input(default);

                            self.buffer.write_tab();
                            self.buffer.write_fmt(format_args!(
                                "default: {output} = {default};\n"
                            ));
                        }
                        None => {
                            let input = variant_inputs.last().unwrap();
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
                let input = self.inject_input(*input);
                let output = output.sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = ~{input};",));
            }
            NodeKind::Not(Not { input, output }) => {
                let input = self.inject_input(*input);
                let output = output.sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = !{input};",));
            }
            NodeKind::BinOp(BinOpNode {
                bin_op,
                inputs: (left, right),
                output,
            }) => {
                let left = self.inject_input(*left);
                let right = self.inject_input(*right);
                let output = output.sym.unwrap();

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!(
                    "assign {output} = {left} {bin_op} {right};\n\n"
                ));
            }
            NodeKind::Mux2(node @ Mux2 { output, .. }) => {
                let Mux2Inputs {
                    sel,
                    input1,
                    input2,
                } = node.mux2_inputs();

                let sel = self.inject_input(sel);
                let input1 = self.inject_input(input1);
                let input2 = self.inject_input(input2);
                let output = output.sym.unwrap();

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
            NodeKind::DFF(node @ DFF { output, .. }) => {
                let DFFInputs {
                    clk,
                    rst,
                    en,
                    rst_val,
                    data,
                } = node.dff_inputs();
                let clk = self.inject_input(clk);
                let rst = self.inject_input(rst);
                let data = self.inject_input(data);
                let rst_val = self.inject_input(rst_val);
                let output = output.sym.unwrap();

                match en {
                    Some(en) => {
                        let en = self.inject_input(en);

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
