use ferrum_hdl::domain::{Polarity, SyncKind};
use rustc_hash::FxHashSet;

use crate::{
    buffer::Buffer,
    netlist::{Cursor, Module, NetList, NodeId, WithId},
    node::{
        BinOpInputs, Case, DFFInputs, MuxInputs, NetKind, Node, NodeKind, NodeOutput,
    },
    symbol::Symbol,
    visitor::ParamKind,
};

fn write_param(buffer: &mut Buffer, node_out: &NodeOutput, kind: ParamKind) {
    let kind = match kind {
        ParamKind::Input => "input ",
        ParamKind::Output => "output ",
    };

    buffer.write_str(kind);
    write_out(buffer, node_out);
    buffer.write_fmt(format_args!(" {}", node_out.sym.unwrap()));
}

fn write_out(buffer: &mut Buffer, out: &NodeOutput) {
    match &out.kind {
        NetKind::Wire => buffer.write_str("wire"),
        NetKind::Reg => buffer.write_str("reg"),
    };

    if out.ty.width() > 1 {
        buffer.write_fmt(format_args!(" [{}:0]", out.ty.width() - 1));
    }
}

const SEP: &str = ",\n";

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

    pub fn synth(mut self) -> String {
        self.visit_modules();

        self.buffer.buffer
    }

    fn write_locals(&mut self, module: &Module, node: WithId<NodeId, &Node>) {
        let can_skip = !node.is_mod_inst();

        for node_out in node.outputs() {
            let port = node_out.id;
            let is_input = module.is_input(port);
            let is_output = module.is_output(port);
            self.write_local(*node_out, is_input, is_output, can_skip);
        }
    }

    fn write_local(
        &mut self,
        node_out: &NodeOutput,
        is_input: bool,
        is_output: bool,
        can_skip: bool,
    ) {
        if can_skip && node_out.skip {
            return;
        }
        let sym = node_out.sym.unwrap();

        if !self.locals.contains(&sym) {
            if !(is_input || is_output) {
                self.buffer.write_tab();
                write_out(&mut self.buffer, node_out);
                self.buffer.write_fmt(format_args!(" {}", sym));
                self.buffer.write_str(";\n");
            }

            self.locals.insert(sym);
        }
    }

    fn write_mod_span(&mut self, module: &Module) {
        if let Some(span) = module.span() {
            self.buffer.write_tab();
            self.buffer.write_str("// ");
            self.buffer.write_str(span);
            self.buffer.write_eol();
        }
    }

    fn write_span(&mut self, node: &Node) {
        if let Some(span) = node.span() {
            self.buffer.write_tab();
            self.buffer.write_str("// ");
            self.buffer.write_str(span);
            self.buffer.write_eol();
        }
    }

    fn visit_modules(&mut self) {
        self.buffer
            .write_str("/* Automatically generated by Ferrum HDL. */\n\n");

        for module in self.netlist.modules().rev() {
            let module = module.borrow();
            if module.skip {
                continue;
            }

            self.visit_module(&module);
        }
    }

    fn visit_module(&mut self, module: &Module) {
        self.locals.clear();
        let is_top = module.is_top;

        self.write_mod_span(module);

        self.buffer
            .write_fmt(format_args!("module {}\n(\n", module.name));

        let mut has_inputs = false;

        // Dont skip inputs if module is top
        let mut inputs = module
            .inputs()
            .iter()
            .filter(|&&input| is_top || !module[input].skip)
            .copied()
            .peekable();

        self.buffer.push_tab();
        if inputs.peek().is_some() {
            has_inputs = true;
            self.buffer.write_tab();
            self.buffer.write_str("// Inputs\n");

            self.buffer.intersperse(SEP, inputs, |buffer, port| {
                buffer.write_tab();
                write_param(buffer, &module[port], ParamKind::Input);
            });
        }
        self.buffer.pop_tab();

        // Dont skip outputs if module is top
        let mut outputs = module
            .outputs()
            .iter()
            .filter(|&&output| is_top || !module[output].skip)
            .copied()
            .peekable();

        self.buffer.push_tab();
        if outputs.peek().is_some() {
            if has_inputs {
                self.buffer.write_str(SEP);
            }
            self.buffer.write_tab();
            self.buffer.write_str("// Outputs\n");

            self.buffer.intersperse(SEP, outputs, |buffer, port| {
                buffer.write_tab();
                write_param(buffer, &module[port], ParamKind::Output);
            });
        }
        self.buffer.pop_tab();

        self.buffer.write_str("\n);\n");
        self.buffer.write_eol();

        self.buffer.push_tab();

        let mut nodes = module.nodes();
        while let Some(node_id) = nodes.next(module) {
            let node = &module[node_id];
            if node.skip {
                continue;
            }

            self.visit_node(module, WithId::new(node_id, node));
        }
        self.buffer.pop_tab();

        self.buffer.write_str("endmodule\n\n");
    }

    fn visit_node(&mut self, module: &Module, node: WithId<NodeId, &Node>) {
        self.write_span(*node);
        self.write_locals(module, node);

        match &*node.kind {
            NodeKind::Input(_) => {}
            NodeKind::Pass(pass) => {
                let input = node.with(pass).input(module);
                let input = module[input].sym.unwrap();
                let output = pass.output[0].sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = {input};",));
            }
            NodeKind::ModInst(mod_inst) => {
                let mod_inst = node.with(mod_inst);
                let orig_mod = self
                    .netlist
                    .module(mod_inst.mod_id)
                    .map(|orig_mod| orig_mod.borrow());
                let name = mod_inst.name;

                self.buffer.write_tab();

                self.buffer.write_fmt(format_args!(
                    "{} {} (\n",
                    orig_mod.name,
                    name.unwrap()
                ));

                self.buffer.push_tab();
                if mod_inst.has_inputs() {
                    self.buffer.write_tab();
                    self.buffer.write_str("// Inputs\n");

                    self.buffer.intersperse(
                        SEP,
                        module.mod_inst_inputs(mod_inst, orig_mod.as_deref()),
                        |buffer, (mod_inst_input, orig_mod_input)| {
                            if orig_mod_input.skip {
                                return;
                            }
                            let mod_inst_sym = mod_inst_input.sym.unwrap();
                            let orig_mod_sym = orig_mod_input.sym.unwrap();

                            buffer.write_tab();
                            buffer.write_fmt(format_args!(
                                ".{orig_mod_sym}({mod_inst_sym})"
                            ));
                        },
                    );
                }

                if mod_inst.has_outputs() {
                    if mod_inst.has_inputs() {
                        self.buffer.write_str(SEP);
                    }
                    self.buffer.write_tab();
                    self.buffer.write_str("// Outputs\n");

                    self.buffer.intersperse(
                        SEP,
                        module.mod_inst_outputs(mod_inst, orig_mod.as_deref()),
                        |buffer, (mod_inst_output, orig_mod_output)| {
                            if orig_mod_output.skip {
                                return;
                            }

                            let mod_inst_sym = mod_inst_output.sym.unwrap();
                            let orig_mod_sym = orig_mod_output.sym.unwrap();

                            buffer.write_tab();
                            buffer.write_fmt(format_args!(
                                ".{orig_mod_sym}({mod_inst_sym})"
                            ));
                        },
                    );
                }

                self.buffer.write_eol();

                self.buffer.pop_tab();
                self.buffer.write_tab();
                self.buffer.write_str(");\n\n");
            }
            NodeKind::Const(cons) => {
                let output = cons.output[0].sym.unwrap();
                let value = cons.value;

                self.buffer
                    .write_template(format_args!("assign {output} = {value};"));
            }
            NodeKind::MultiConst(multi_cons) => {
                for (value, output) in multi_cons.val_outputs() {
                    if output.skip {
                        continue;
                    }

                    let output = output.sym.unwrap();

                    self.buffer
                        .write_template(format_args!("assign {output} = {value};"));
                }
            }
            NodeKind::Splitter(splitter) => {
                let splitter = node.with(splitter);
                let input = splitter.input(module);

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

                let input = module[input].sym.unwrap();

                for (index, output) in splitter.eval_indices(module) {
                    if !output.skip {
                        write_out(&mut self.buffer, output, input.as_ref(), index);
                    }
                }
            }
            NodeKind::Merger(merger) => {
                let merger = node.with(merger);
                let output = merger.output[0].sym.unwrap();

                self.buffer.write_tab();
                self.buffer
                    .write_fmt(format_args!("assign {output} = {{\n"));

                let inputs = merger.inputs(module);

                self.buffer.push_tab();
                self.buffer.intersperse(
                    SEP,
                    inputs.map(|input| module[input].sym.unwrap()),
                    |buffer, input| {
                        buffer.write_tab();
                        buffer.write_fmt(format_args!("{}", input));
                    },
                );
                self.buffer.pop_tab();

                self.buffer.write_eol();
                self.buffer.write_tab();
                self.buffer.write_str("};\n\n");
            }
            NodeKind::ZeroExtend(zero_extend) => {
                let zero_extend = node.with(zero_extend);
                let input = module[zero_extend.input(module)].sym.unwrap();
                let output = zero_extend.output[0].sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = {{ 0, {input} }};"));
            }
            NodeKind::Mux(mux) => {
                let mux = node.with(mux);
                let MuxInputs { sel, cases } = mux.inputs(module);

                let output = mux.output[0].sym.unwrap();
                let sel_sym = module[sel].sym.unwrap();

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!("always @(*) begin\n"));

                self.buffer.push_tab();

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!("case ({sel_sym})\n"));

                self.buffer.push_tab();

                for (case, input) in cases {
                    match case {
                        Case::Val(case) => {
                            let input = module[input].sym.unwrap();

                            self.buffer.write_tab();
                            self.buffer.write_fmt(format_args!(
                                "{case} : {output} = {input};\n",
                            ));
                        }
                        Case::Default => {
                            let default = module[input].sym.unwrap();

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
            NodeKind::BitNot(bit_not) => {
                let bit_not = node.with(bit_not);
                let input = module[bit_not.input(module)].sym.unwrap();
                let output = bit_not.output[0].sym.unwrap();

                self.buffer
                    .write_template(format_args!("assign {output} = ~{input};",));
            }
            NodeKind::BinOp(bin_op) => {
                let bin_op = node.with(bin_op);
                let BinOpInputs { lhs, rhs } = bin_op.inputs(module);
                let lhs = module[lhs].sym.unwrap();
                let rhs = module[rhs].sym.unwrap();
                let output = bin_op.output[0].sym.unwrap();
                let bin_op = bin_op.bin_op;

                self.buffer.write_tab();
                self.buffer.write_fmt(format_args!(
                    "assign {output} = {lhs} {bin_op} {rhs};\n\n"
                ));
            }
            NodeKind::DFF(dff) => {
                let dff = node.with(dff);
                let DFFInputs {
                    clk,
                    rst,
                    en,
                    init,
                    data,
                } = dff.inputs(module);

                let clk = module[clk].sym.unwrap();
                let data = module[data].sym.unwrap();
                let rst = rst.map(|rst| module[rst].sym.unwrap());
                let en = en.map(|en| module[en].sym.unwrap());
                let init_val = module.to_const(init);
                let output = dff.output[0].sym.unwrap();

                if let Some(init_val) = init_val {
                    self.buffer.write_tab();
                    self.buffer.write_str("initial begin\n");

                    self.buffer.push_tab();
                    self.buffer.write_tab();
                    self.buffer
                        .write_fmt(format_args!("{output} = {init_val};\n"));
                    self.buffer.pop_tab();

                    self.buffer.write_tab();
                    self.buffer.write_str("end\n");
                }

                self.buffer.write_tab();
                self.buffer
                    .write_fmt(format_args!("always @(posedge {clk}"));
                if let Some(rst) = rst {
                    if let SyncKind::Async = dff.rst_kind {
                        let polarity = dff.rst_pol;
                        self.buffer.write_fmt(format_args!(" or {polarity} {rst}"));
                    }
                }
                self.buffer.write_str(") begin\n");

                self.buffer.push_tab();

                {
                    let mut else_ = false;
                    if let Some(rst) = rst {
                        self.buffer.write_tab();
                        match dff.rst_pol {
                            Polarity::ActiveHigh => {
                                self.buffer.write_fmt(format_args!("if ({rst})\n"));
                            }
                            Polarity::ActiveLow => {
                                self.buffer.write_fmt(format_args!("if (!{rst})\n"));
                            }
                        }

                        self.buffer.push_tab();

                        self.buffer.write_tab();
                        if let Some(init_val) = init_val {
                            self.buffer
                                .write_fmt(format_args!("{output} <= {init_val};\n"));
                        } else {
                            let init = module[init].sym.unwrap();
                            self.buffer.write_fmt(format_args!("{output} <= {init};\n"));
                        }

                        self.buffer.pop_tab();

                        else_ = true;
                    }

                    let mut pop_tab = false;
                    if let Some(en) = en {
                        self.buffer.write_tab();
                        if else_ {
                            self.buffer.write_fmt(format_args!("else if ({en})\n"));
                        } else {
                            self.buffer.write_fmt(format_args!("if ({en})\n"));
                        }

                        self.buffer.push_tab();
                        pop_tab = true;
                    } else if else_ {
                        self.buffer.write_tab();
                        self.buffer.write_str("else\n");

                        self.buffer.push_tab();
                        pop_tab = true;
                    };

                    self.buffer.write_tab();
                    self.buffer.write_fmt(format_args!("{output} <= {data};\n"));

                    if pop_tab {
                        self.buffer.pop_tab();
                    }
                }

                self.buffer.pop_tab();

                self.buffer.write_tab();
                self.buffer.write_str("end\n\n");
            }
        }
    }
}