use std::borrow::Cow;

use rustc_hash::FxHashSet;
use smallvec::SmallVec;

use crate::{
    buffer::Buffer,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{Case, DFFInputs, MuxInputs, NetKind, NodeKindWithId, NodeOutput},
    symbol::Symbol,
    visitor::{ParamKind, Visitor},
};

pub trait Backend {}

pub struct Verilog<'n> {
    pub buffer: Buffer,
    pub locals: FxHashSet<Symbol>,
    pub netlist: &'n NetList,
}

impl<'n> Verilog<'n> {
    pub fn new(net_list: &'n NetList) -> Self {
        Self {
            buffer: Buffer::new(),
            locals: Default::default(),
            netlist: net_list,
        }
    }

    pub fn generate(mut self) -> String {
        self.visit_modules();

        self.buffer.buffer
    }

    fn write_locals(&mut self, node_id: NodeId) {
        let can_skip = !self.netlist[node_id].is_mod_inst();

        for node_out_id in self.netlist[node_id].node_out_ids() {
            self.write_local(node_out_id, can_skip);
        }
    }

    fn write_local(&mut self, node_out_id: NodeOutId, can_skip: bool) {
        let node_id = node_out_id.node_id();
        let out = &self.netlist[node_out_id];
        if can_skip && (out.skip || out.inject) {
            return;
        }
        let is_input = self.netlist.is_input(node_id);
        let is_output = self.netlist.is_output(node_out_id);
        let sym = out.sym.unwrap();

        if !self.locals.contains(&sym) {
            if !(is_input || is_output) {
                self.buffer.write_tab();
                write_out(&mut self.buffer, out);
                self.buffer.write_fmt(format_args!(" {}", sym));
                self.buffer.write_str(";\n");
            }

            if let NetKind::Reg(Some(init)) = &out.kind {
                let init = self.netlist[node_id].input_by_ind(*init);
                let node = &self.netlist[init.node_id()];
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
        let inject = self.netlist[node_out_id].inject;

        self.write_local(node_out_id, true);
        for input in self.netlist[node_out_id.node_id()].inputs() {
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
        let node_out = &self.netlist[node_out_id];
        let node_id = node_out_id.node_id();
        let node = &self.netlist[node_id];

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

        self.netlist[node_out_id].sym.unwrap().as_str().into()
    }

    fn inject_node(
        &self,
        module_id: ModuleId,
        node_out_id: NodeOutId,
        expr: &mut Buffer,
        nested_expr: bool,
    ) {
        use NodeKindWithId as NodeKind;

        let node_out = &self.netlist[node_out_id];
        let node_id = node_out_id.node_id();
        let node = &self.netlist[node_id];

        if !node_out.inject {
            let mod_id = node_id.module_id();
            if module_id != mod_id {
                panic!("Cannot inject non-injectable nodes from other modules (current: {}, other module: {})", 
                    self.netlist[module_id].name, self.netlist[node_id.module_id()].name);
            }
            expr.write_str(self.netlist[node_out_id].sym.unwrap().as_str());
            return;
        }

        if let Some(const_val) = self.netlist.to_const(node_out_id) {
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
                let width = outputs[out_id].width();

                let mut indices = splitter.eval_indices(self.netlist);
                let (_, index) = indices.nth(out_id).unwrap();
                let start: Cow<'_, str> = index.to_string().into();

                if width == 1 {
                    expr.write_fmt(format_args!("[{start}]"));
                } else {
                    expr.write_fmt(format_args!("[{start} +: {width}]"));
                }
            }
            NodeKind::Merger(merger) => {
                expr.write_str("{ ");
                let inputs = merger.inputs();
                expr.intersperse(", ", inputs, |expr, input| {
                    self.inject_node(module_id, input, expr, false);
                });
                expr.write_str(" }");
            }
            _ => {}
        }
    }

    fn write_mod_span(&mut self, mod_id: ModuleId) {
        if let Some(span) = self.netlist[mod_id].span() {
            self.buffer.write_tab();
            self.buffer.write_str("// ");
            self.buffer.write_str(span);
            self.buffer.write_eol();
        }
    }

    fn write_span(&mut self, node_id: NodeId) {
        if let Some(span) = self.netlist[node_id].span() {
            self.buffer.write_tab();
            self.buffer.write_str("// ");
            self.buffer.write_str(span);
            self.buffer.write_eol();
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

    let kind = match kind {
        ParamKind::Input => "input ",
        ParamKind::Output => "output ",
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

        for module_id in self.netlist.modules() {
            let module = &self.netlist[module_id];
            if module.skip {
                continue;
            }
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, mod_id: ModuleId) {
        self.locals = Default::default();

        let module = &self.netlist[mod_id];
        let is_top = module.is_top;

        self.write_mod_span(mod_id);

        self.buffer
            .write_fmt(format_args!("module {}\n(\n", module.name));

        let mut has_inputs = false;

        // Dont skip inputs if module is top
        let mut inputs = self
            .netlist
            .mod_inputs(mod_id)
            .filter(|input| is_top || !self.netlist[*input].skip)
            .peekable();

        self.buffer.push_tab();
        if inputs.peek().is_some() {
            has_inputs = true;
            self.buffer.write_tab();
            self.buffer.write_str("// Inputs\n");

            let net_list = &self.netlist;
            self.buffer.intersperse(SEP, inputs, |buffer, input| {
                buffer.write_tab();
                write_param(net_list, buffer, input, ParamKind::Input);
            });
        }
        self.buffer.pop_tab();

        // Dont skip outputs if module is top
        let mut outputs = self
            .netlist
            .mod_outputs(mod_id)
            .filter(|output| is_top || !self.netlist[*output].skip)
            .peekable();

        self.buffer.push_tab();
        if outputs.peek().is_some() {
            if has_inputs {
                self.buffer.write_str(SEP);
            }
            self.buffer.write_tab();
            self.buffer.write_str("// Outputs\n");

            let net_list = &self.netlist;
            self.buffer.intersperse(SEP, outputs, |buffer, output| {
                buffer.write_tab();
                write_param(net_list, buffer, output, ParamKind::Output);
            });
        }
        self.buffer.pop_tab();

        self.buffer.write_str("\n);\n");
        self.buffer.write_eol();

        self.buffer.push_tab();
        let mut cursor = self.netlist.mod_cursor(mod_id);
        while let Some(node_id) = self.netlist.next(&mut cursor) {
            let node = &self.netlist[node_id];
            if node.skip || node.inject {
                continue;
            }

            self.visit_node(node_id);
        }
        self.buffer.pop_tab();

        self.buffer.write_str("endmodule\n\n");
    }

    fn visit_node(&mut self, node_id: NodeId) {
        use NodeKindWithId as NodeKind;

        self.write_span(node_id);
        self.write_locals(node_id);

        let node = &self.netlist[node_id];
        match node.kind() {
            NodeKind::Input(_) => {}
            NodeKind::Pass(pass) => {
                let input = self.inject_input(pass.input(), false);
                let output = pass.output().sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = {input};",));
            }
            NodeKind::ModInst(mod_inst) => {
                let module_id = mod_inst.module_id();
                let name = mod_inst.name();
                let module = &self.netlist[module_id];
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
                    mod_inst.inputs().zip(self.netlist.mod_inputs(module_id))
                {
                    if self.netlist[mod_input].skip {
                        continue;
                    }
                    let input_sym = self.inject_input(input, false);
                    let mod_input_sym = self.netlist[mod_input].sym.unwrap();

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
                        .zip(self.netlist.mod_outputs(module_id)),
                    |buffer, (output, mod_output)| {
                        if self.netlist[mod_output].skip {
                            return;
                        }
                        let output_sym = output.sym.unwrap();
                        let mod_output_sym = self.netlist[mod_output].sym.unwrap();

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
                    if output.skip {
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
                    let width = output.width();
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

                let indices = splitter.eval_indices(self.netlist);

                for (output, index) in indices {
                    if !(output.skip || output.inject) {
                        write_out(&mut self.buffer, output, input.as_ref(), index);
                    }
                }
            }
            NodeKind::Merger(merger) => {
                if !merger.inputs_is_empty() {
                    let output = merger.output().sym.unwrap();

                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!("assign {output} = {{\n"));

                    let inputs = merger.inputs();
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
            NodeKind::Mux(case) => {
                if !case.is_empty() {
                    let MuxInputs { sel, variants } = case.inputs();

                    let output = case.output().sym.unwrap();
                    let sel_sym = self.inject_input(sel, false);

                    self.buffer.write_tab();
                    self.buffer.write_fmt(format_args!("always @(*) begin\n"));

                    self.buffer.push_tab();

                    self.buffer.write_tab();
                    self.buffer.write_fmt(format_args!("case ({sel_sym})\n"));

                    self.buffer.push_tab();

                    for (case, input) in variants {
                        match case {
                            Case::Val(case) => {
                                let input = self.inject_input(input, false);

                                self.buffer.write_tab();
                                self.buffer.write_fmt(format_args!(
                                    "{case} : {output} = {input};\n",
                                ));
                            }
                            Case::Default => {
                                let default = self.inject_input(input, false);

                                self.buffer.write_tab();
                                self.buffer.write_fmt(format_args!(
                                    "default: {output} = {default};\n"
                                ));
                            }
                        }
                    }

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
