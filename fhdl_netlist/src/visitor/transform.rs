use smallvec::SmallVec;

use crate::{
    cfg::InlineMod,
    const_val::ConstVal,
    net_list::{ModuleId, NetList, NodeCursor, NodeId},
    node::{BinOp, Case, Const, MultiConst, MuxInputs, NodeKind, NodeKindWithId},
    visitor::Visitor,
};

pub struct Transform<'n> {
    netlist: &'n mut NetList,
}

impl<'n> Transform<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        Self { netlist: net_list }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }

    fn transform(
        &mut self,
        node_id: NodeId,
        cursor: &mut NodeCursor,
    ) -> Option<NodeKind> {
        if let NodeKindWithId::ModInst(mod_inst) = self.netlist[node_id].kind() {
            let module_id = mod_inst.module_id();

            // transform nodes of module before inlining module
            self.visit_module(module_id);
        };

        let mut should_be_inlined = false;
        let res = self.transform_(node_id, &mut should_be_inlined);

        if should_be_inlined {
            let node_id = self.netlist.inline_mod(node_id);
            cursor.set_node_id(node_id);
        }

        res
    }

    fn transform_(
        &mut self,
        node_id: NodeId,
        should_be_inlined: &mut bool,
    ) -> Option<NodeKind> {
        use NodeKindWithId as NodeKind;

        let node = &self.netlist[node_id];
        match node.kind() {
            NodeKind::Pass(pass) => match self.netlist.to_const(pass.input()) {
                Some(const_val) => {
                    let output = pass.output();
                    Some(Const::new(output.ty, const_val.val(), output.sym).into())
                }
                None => {
                    if !self.netlist.is_output(node.only_one_out().node_out_id()) {
                        self.netlist.reconnect(node_id);
                    }
                    None
                }
            },
            NodeKind::ModInst(mod_inst) => {
                let module_id = mod_inst.module_id();

                let values = self
                    .netlist
                    .mod_outputs(module_id)
                    .map(|node_out_id| {
                        self.netlist
                            .to_const(node_out_id)
                            .map(|const_val| const_val.val())
                    })
                    .collect::<Option<SmallVec<[_; 8]>>>();

                match values {
                    Some(values) => {
                        let outputs = self
                            .netlist
                            .mod_outputs(module_id)
                            .map(|node_out_id| self.netlist[node_out_id]);

                        Some(MultiConst::new(values, outputs).into())
                    }
                    None => {
                        match self.netlist.cfg().inline_mod {
                            InlineMod::All => {
                                *should_be_inlined = true;
                            }
                            InlineMod::Auto => {
                                *should_be_inlined = self.netlist[module_id].is_inlined
                                    || node.inputs_len() == 0
                                    || self.netlist[module_id].only_inputs
                                    || node.inputs().all(|input| {
                                        self.netlist[input.node_id()].is_const()
                                    });
                            }
                            InlineMod::None => {
                                *should_be_inlined = false;
                            }
                        };

                        None
                    }
                }
            }
            NodeKind::Not(not) => self.netlist.to_const(not.input()).map(|const_val| {
                let const_val = !const_val;
                let output = not.output();
                Const::new(output.ty, const_val.val(), output.sym).into()
            }),
            NodeKind::BitNot(bit_not) => {
                self.netlist.to_const(bit_not.input()).map(|const_val| {
                    let const_val = !const_val;
                    let output = bit_not.output();
                    Const::new(output.ty, const_val.val(), output.sym).into()
                })
            }

            NodeKind::BinOp(bin_op) => match (
                self.netlist.to_const(bin_op.left()),
                self.netlist.to_const(bin_op.right()),
            ) {
                (Some(left), Some(right)) => {
                    let const_val = match bin_op.bin_op() {
                        BinOp::Add => left + right,
                        BinOp::Sub => left - right,
                        BinOp::Mul => left * right,
                        BinOp::Div => left / right,
                        BinOp::BitAnd => left & right,
                        BinOp::Rem => left % right,
                        BinOp::BitOr => left | right,
                        BinOp::BitXor => left ^ right,
                        BinOp::And => left & right,
                        BinOp::Or => left & right,
                        BinOp::Shl => left << right,
                        BinOp::Shr => left >> right,
                        BinOp::Eq => (left == right).into(),
                        BinOp::Ne => (left != right).into(),
                        BinOp::Ge => (left >= right).into(),
                        BinOp::Gt => (left > right).into(),
                        BinOp::Le => (left <= right).into(),
                        BinOp::Lt => (left < right).into(),
                    };

                    let output = bin_op.output();
                    Some(Const::new(output.ty, const_val.val(), output.sym).into())
                }
                _ => None,
            },
            NodeKind::Splitter(splitter) => {
                if splitter.pass_all_bits(self.netlist) {
                    self.netlist.reconnect(node_id);
                    None
                } else {
                    let indices = splitter.eval_indices(self.netlist);
                    let input = self.netlist.to_const(splitter.input());

                    if let Some(input) = input {
                        let values = indices
                            .map(|(output, index)| {
                                ConstVal::new(input.val() >> index, output.width()).val()
                            })
                            .collect::<Vec<_>>();

                        Some(
                            MultiConst::new(values, splitter.outputs().iter().copied())
                                .into(),
                        )
                    } else {
                        drop(indices);

                        let input = splitter.input();

                        let input_id = input.node_id();
                        let input = &self.netlist[input_id];

                        #[allow(clippy::single_match)]
                        match input.kind() {
                            NodeKind::Merger(merger) => {
                                if splitter.rev() != merger.rev() // TODO: maybe it's unnecessary
                                    && self.netlist.is_reversible(input_id, node_id)
                                {
                                    self.netlist.reconnect_from_to(input_id, node_id);
                                }
                            }
                            _ => {}
                        }

                        None
                    }
                }
            }
            NodeKindWithId::Merger(merger) => {
                // let sym = output.sym;

                // if !*rev {
                //     // for case:
                //     // ```verilog
                //     // wire [3:0] in;
                //     // wire [3:0] out;
                //     // assing out = {in[3], in[2], in[1], in[0]};
                //     // ```
                //     //
                //     // after transform:
                //     // ```verilog
                //     // wire [3:0] in;
                //     // wire [3:0] out;
                //     // assign out = in;
                //     // ```
                //     if let Some(node_out_id) = self.is_merger_eq_input(inputs) {
                //         let out = &self[node_out_id];
                //         return Pass::new(out.ty, node_out_id, sym).into();
                //     }
                // }

                let mut val = Some(ConstVal::new(0, 0));
                merger
                    .inputs()
                    .for_each(|input| match self.netlist.to_const(input) {
                        Some(new_val) => {
                            if let Some(val) = val.as_mut() {
                                val.shift(new_val);
                            }
                        }
                        None => {
                            val = None;
                        }
                    });

                val.map(|const_val| {
                    let output = merger.output();
                    Const::new(output.ty, const_val.val(), output.sym).into()
                })
            }

            NodeKindWithId::ZeroExtend(zero_extend) => {
                let output = zero_extend.output();
                match self.netlist.to_const(zero_extend.input()) {
                    Some(const_val) => {
                        Some(Const::new(output.ty, const_val.val(), output.sym).into())
                    }
                    None => {
                        if self.netlist[zero_extend.input()].width() == output.width() {
                            self.netlist.reconnect(node_id);
                        }

                        None
                    }
                }
            }

            NodeKind::Mux(node) => {
                let MuxInputs { sel, variants } = node.inputs();

                if let Some(new_input) = self.netlist.to_const(sel).and_then(|sel| {
                    for (case, input) in variants {
                        match case {
                            Case::Val(case) => {
                                if case == sel {
                                    return Some(input);
                                }
                            }
                            Case::Default => {
                                return Some(input);
                            }
                        }
                    }

                    None
                }) {
                    let out_id = self.netlist[node_id].only_one_out().node_out_id().idx();
                    self.netlist
                        .reconnect_input_by_ind(node_id, out_id, new_input);
                }

                None
            }
            _ => None,
        }
    }
}

impl<'n> Visitor for Transform<'n> {
    fn visit_modules(&mut self) {
        if let Some(top) = self.netlist.top_module() {
            self.visit_module(top);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let mut cursor = self.netlist.mod_cursor(module_id);
        while let Some(node_id) = self.netlist.next(&mut cursor) {
            let new_node = self.transform(node_id, &mut cursor);
            if let Some(new_node) = new_node {
                self.netlist.replace(node_id, new_node);
            }
        }
    }

    fn visit_node(&mut self, _node_id: NodeId) {}
}
