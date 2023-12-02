use std::borrow::Cow;

use either::Either;
use rustc_data_structures::fx::FxHashSet;
use smallvec::SmallVec;

use crate::{
    buffer::Buffer,
    bvm::BitVecMask,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{CaseInputs, DFFInputs, Mux2Inputs, NetKind, NodeKindWithId, NodeOutput},
    symbol::Symbol,
    visitor::{ParamKind, Visitor},
};

pub trait Backend {}

pub struct Verilog<'n> {
    pub buffer: Buffer,
    pub locals: FxHashSet<Symbol>,
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
        let can_skip = !self.net_list[node_id].is_mod_inst();

        for node_out_id in self.net_list[node_id].node_out_ids() {
            self.write_local(node_out_id, can_skip);
        }
    }

    fn write_local(&mut self, node_out_id: NodeOutId, can_skip: bool) {
        let node_id = node_out_id.node_id();
        let out = &self.net_list[node_out_id];
        if can_skip && (out.is_skip || out.inject) {
            return;
        }
        let is_input = self.net_list.is_input(node_id);
        let is_output = self.net_list.is_output(node_out_id);
        let sym = out.sym.unwrap();

        if !self.locals.contains(&sym) {
            if !(is_input || is_output) {
                self.buffer.write_tab();
                write_out(&mut self.buffer, out);
                self.buffer.write_fmt(format_args!(" {}", sym));
                self.buffer.write_str(";\n");
            }

            if let NetKind::Reg(init) = &out.kind {
                let init = self.net_list[node_id].input_by_ind(*init);
                let node = &self.net_list[init.node_id()];
                if node.is_const() {
                    let init = self.inject_input(*init, false);

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

    fn write_local_for_injected(&mut self, node_out_id: NodeOutId) {
        let inject = self.net_list[node_out_id].inject;

        self.write_local(node_out_id, true);
        for input in self.net_list[node_out_id.node_id()].inputs() {
            if inject {
                self.write_local_for_injected(*input);
            }
        }
    }

    fn inject_input(
        &mut self,
        node_out_id: NodeOutId,
        nested_expr: bool,
    ) -> Cow<'static, str> {
        self.write_local_for_injected(node_out_id);
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
                nested_expr,
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
        use NodeKindWithId as NodeKind;

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

        match node.kind() {
            NodeKind::BitNot(bit_not) => {
                expr.write_str("~");
                self.inject_node(module_id, bit_not.input(), expr, true);
            }
            NodeKind::Not(not) => {
                expr.write_str("!");
                self.inject_node(module_id, not.input(), expr, true);
            }
            NodeKind::BinOp(bin_op) => {
                if nested_expr {
                    expr.write_str("( ");
                }

                self.inject_node(module_id, bin_op.left(), expr, true);
                expr.write_fmt(format_args!(" {} ", bin_op.bin_op()));
                self.inject_node(module_id, bin_op.right(), expr, true);

                if nested_expr {
                    expr.write_str(" )");
                }
            }
            NodeKind::Splitter(splitter) => {
                let out_id = node_out_id.idx();
                let outputs = splitter.outputs();

                self.inject_node(module_id, splitter.input(), expr, true);
                let width = outputs[out_id].width().value();

                let start: Cow<'_, str> = splitter
                    .eval_indices(self.net_list)
                    .map(|mut indices| {
                        let (_, index) = indices.nth(out_id).unwrap();

                        index.to_string().into()
                    })
                    .expect("Cannot evaluate indices for splitter");

                if width == 1 {
                    expr.write_fmt(format_args!("[{start}]"));
                } else {
                    expr.write_fmt(format_args!("[{start} +: {width}]"));
                }
            }
            NodeKind::Merger(merger) => {
                expr.write_str("{ ");
                let inputs = if !merger.rev() {
                    Either::Left(merger.inputs())
                } else {
                    Either::Right(merger.inputs().rev())
                };
                expr.intersperse(", ", inputs, |expr, input| {
                    self.inject_node(module_id, input, expr, false);
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

    if out.ty.width().value() > 1 {
        buffer.write_fmt(format_args!(" [{}:0]", out.ty.width().value() - 1));
    }
}

const SEP: &str = ",\n";

impl<'n> Visitor for Verilog<'n> {
    fn visit_modules(&mut self) {
        self.buffer
            .write_str("/* Automatically generated by Ferrum HDL. */\n\n");

        for module_id in self.net_list.modules() {
            let module = &self.net_list[module_id];
            if module.is_skip {
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
        use NodeKindWithId as NodeKind;

        self.write_locals(node_id);

        let node = &self.net_list[node_id];
        match node.kind() {
            NodeKind::TemplateNode(_) => {
                panic!("Cannot generate verilog for TemplateNode");
            }
            NodeKind::Input(_) => {}
            NodeKind::ModInst(mod_inst) => {
                let module_id = mod_inst.module_id();
                let name = mod_inst.name();
                let module = &self.net_list[module_id];
                assert_eq!(mod_inst.inputs_len(), module.inputs_len());
                assert_eq!(mod_inst.outputs_len(), module.outputs_len());

                self.buffer.write_tab();

                self.buffer.write_fmt(format_args!(
                    "{} {} (\n",
                    module.name,
                    name.unwrap()
                ));

                self.buffer.push_tab();
                if !mod_inst.inputs_is_empty() {
                    self.buffer.write_tab();
                    self.buffer.write_str("// Inputs\n");
                }
                for (input, mod_input) in
                    mod_inst.inputs().zip(self.net_list.mod_inputs(module_id))
                {
                    let input_sym = self.inject_input(input, false);
                    let mod_input_sym = self.net_list[mod_input].sym.unwrap();

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!(".{mod_input_sym}({input_sym}),\n"));
                }
                self.buffer.write_tab();
                self.buffer.write_str("// Outputs\n");

                self.buffer.intersperse(
                    SEP,
                    mod_inst
                        .outputs()
                        .iter()
                        .zip(self.net_list.mod_outputs(module_id)),
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
            NodeKind::Const(cons) => {
                let output = cons.output().sym.unwrap();
                let value = cons.value();

                self.buffer
                    .write_template(format_args!("assign {output} = {value};"));
            }
            NodeKind::MultiConst(multi_cons) => {
                for (value, output) in
                    multi_cons.values().iter().zip(multi_cons.outputs())
                {
                    if output.is_skip {
                        continue;
                    }

                    let output = output.sym.unwrap();

                    self.buffer
                        .write_template(format_args!("assign {output} = {value};"));
                }
            }
            NodeKind::Splitter(splitter) => {
                let input = splitter.input();

                fn write_out(
                    buffer: &mut Buffer,
                    output: &NodeOutput,
                    input: &str,
                    start: u128,
                ) {
                    let width = output.width().value();
                    let output = output.sym.unwrap();

                    buffer.write_tab();
                    if width == 1 {
                        buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start}];\n\n"
                        ));
                    } else {
                        buffer.write_fmt(format_args!(
                            "assign {output} = {input}[{start} +: {width}];\n\n"
                        ));
                    }
                }

                let input = self.inject_input(input, false);

                let indices = splitter
                    .eval_indices(self.net_list)
                    .expect("Cannot evaluate indices for splitter");

                for (output, index) in indices {
                    if !(output.is_skip || output.inject) {
                        write_out(&mut self.buffer, output, input.as_ref(), index);
                    }
                }
            }
            NodeKind::Merger(merger) => {
                if !merger.inputs_is_empty() {
                    let output = merger.output().sym.unwrap();
                    let rev = merger.rev();

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!("assign {output} = {{\n"));

                    let inputs = if !rev {
                        Either::Left(merger.inputs())
                    } else {
                        Either::Right(merger.inputs().rev())
                    };
                    let inputs = inputs
                        .map(|input| self.inject_input(input, false))
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
            NodeKind::ZeroExtend(zero_extend) => {
                let input = self.inject_input(zero_extend.input(), false);
                let output = zero_extend.output().sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = {{ 0, {input} }};"));
            }
            NodeKind::Case(case) => {
                if !case.is_empty() {
                    let CaseInputs {
                        sel,
                        default,
                        variant_inputs,
                        variants,
                    } = case.inputs();

                    let output = case.output().sym.unwrap();

                    let sel_sym = self.inject_input(sel, false);
                    let sel_width = self.net_list[sel].ty.width().value();

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
                        let input = self.inject_input(input, false);

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

                            let default = self.inject_input(default, false);

                            self.buffer.write_tab();
                            self.buffer.write_fmt(format_args!(
                                "default: {output} = {default};\n"
                            ));
                        }
                        None => {
                            let input = variant_inputs.last().unwrap();
                            let default = self.inject_input(*input, false);

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
            NodeKind::BitNot(bit_not) => {
                let input = self.inject_input(bit_not.input(), true);
                let output = bit_not.output().sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = ~{input};",));
            }
            NodeKind::Not(not) => {
                let input = self.inject_input(not.input(), true);
                let output = not.output().sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = !{input};",));
            }
            NodeKind::BinOp(bin_op) => {
                let left = self.inject_input(bin_op.left(), true);
                let right = self.inject_input(bin_op.right(), true);
                let output = bin_op.output().sym.unwrap();
                let bin_op = bin_op.bin_op();

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!(
                    "assign {output} = {left} {bin_op} {right};\n\n"
                ));
            }
            NodeKind::Mux2(mux2) => {
                let Mux2Inputs {
                    sel,
                    input1,
                    input2,
                } = mux2.inputs();

                let sel = self.inject_input(sel, false);
                let input1 = self.inject_input(input1, false);
                let input2 = self.inject_input(input2, false);
                let output = mux2.output().sym.unwrap();

                self.buffer.write_template(format_args!(
                    "
always @(*) begin
    case ({sel})
        1'h1:
            {output} = {input1};
        default:
            {output} = {input2};
    endcase
end
"
                ));
            }
            NodeKind::DFF(dff) => {
                let DFFInputs {
                    clk,
                    rst,
                    en,
                    rst_val,
                    data,
                } = dff.inputs();
                let clk = self.inject_input(clk, false);
                let rst = self.inject_input(rst, false);
                let data = self.inject_input(data, false);
                let rst_val = self.inject_input(rst_val, false);
                let output = dff.output().sym.unwrap();

                match en {
                    Some(en) => {
                        let en = self.inject_input(en, false);

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
