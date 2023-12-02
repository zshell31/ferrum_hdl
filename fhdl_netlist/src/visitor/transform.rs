use smallvec::SmallVec;

use crate::{
    const_val::ConstVal,
    net_list::{ModuleId, NetList, NodeCursor, NodeId},
    node::{BinOp, CaseInputs, Const, MultiConst, Mux2Inputs, NodeKind, NodeKindWithId},
    visitor::Visitor,
};

pub struct Transform<'n> {
    net_list: &'n mut NetList,
}

impl<'n> Transform<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        Self { net_list }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }

    fn transform(
        &mut self,
        node_id: NodeId,
        cursor: &mut NodeCursor,
    ) -> Option<NodeKind> {
        if let NodeKindWithId::ModInst(mod_inst) = self.net_list[node_id].kind() {
            let module_id = mod_inst.module_id();

            // transform nodes of module before inlining module
            self.visit_module(module_id);
        };

        let mut should_be_inlined = false;
        let res = self.transform_(node_id, &mut should_be_inlined);

        if should_be_inlined {
            let node_id = self.net_list.inline_mod(node_id);
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

        let node = &self.net_list[node_id];
        match node.kind() {
            NodeKind::ModInst(mod_inst) => {
                let module_id = mod_inst.module_id();

                let values = self
                    .net_list
                    .mod_outputs(module_id)
                    .map(|node_out_id| {
                        self.net_list
                            .to_const(node_out_id)
                            .map(|const_val| const_val.val)
                    })
                    .collect::<Option<SmallVec<[_; 8]>>>();

                match values {
                    Some(values) => {
                        let outputs = self
                            .net_list
                            .mod_outputs(module_id)
                            .map(|node_out_id| self.net_list[node_out_id]);

                        Some(MultiConst::new(values, outputs).into())
                    }
                    None => {
                        *should_be_inlined = self.net_list[module_id].is_inlined
                            || node.inputs_len() == 0
                            || self.net_list[module_id].only_inputs
                            || node
                                .inputs()
                                .all(|input| self.net_list[input.node_id()].is_const());

                        None
                    }
                }
            }
            NodeKind::Not(not) => self.net_list.to_const(not.input()).map(|const_val| {
                let const_val = !const_val;
                let output = not.output();
                Const::new(output.ty, const_val.val.into(), output.sym).into()
            }),
            NodeKind::BitNot(bit_not) => {
                self.net_list.to_const(bit_not.input()).map(|const_val| {
                    let const_val = !const_val;
                    let output = bit_not.output();
                    Const::new(output.ty, const_val.val.into(), output.sym).into()
                })
            }

            NodeKind::BinOp(bin_op) => match (
                self.net_list.to_const(bin_op.left()),
                self.net_list.to_const(bin_op.right()),
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
                    Some(Const::new(output.ty, const_val.val.into(), output.sym).into())
                }
                _ => None,
            },
            NodeKind::Splitter(splitter) => {
                if splitter.pass_all_bits(self.net_list) {
                    self.net_list.reconnect(node_id);
                    None
                } else {
                    let indices = splitter.eval_indices(self.net_list);
                    let input = self.net_list.to_const(splitter.input());

                    match (indices, input) {
                        (Some(indices), Some(input)) => {
                            let values = indices
                                .map(|(output, index)| {
                                    ConstVal::new(
                                        input.val >> index,
                                        output.width().value(),
                                    )
                                    .val
                                })
                                .collect::<Vec<_>>();

                            Some(
                                MultiConst::new(
                                    values,
                                    splitter.outputs().iter().copied(),
                                )
                                .into(),
                            )
                        }
                        _ => None,
                    }
                }
            }
            NodeKind::Merger(merger) if merger.inputs_len() == 1 => self
                .net_list
                .to_const(merger.inputs().next().unwrap())
                .map(|const_val| {
                    let output = merger.output();
                    Const::new(output.ty, const_val.val.into(), output.sym).into()
                }),
            NodeKindWithId::Merger(merger) if merger.inputs_len() > 1 => {
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
                    .for_each(|input| match self.net_list.to_const(input) {
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
                    Const::new(output.ty, const_val.val.into(), output.sym).into()
                })
            }

            NodeKindWithId::ZeroExtend(zero_extend) => {
                let output = zero_extend.output();
                match self.net_list.to_const(zero_extend.input()) {
                    Some(const_val) => Some({
                        Const::new(output.ty, const_val.val.into(), output.sym).into()
                    }),
                    None => {
                        if self.net_list[zero_extend.input()].width() == output.width() {
                            self.net_list.reconnect(node_id);
                        }

                        None
                    }
                }
            }

            NodeKind::Mux2(node) => {
                let Mux2Inputs {
                    sel,
                    input1,
                    input2,
                } = node.inputs();

                if let Some(new_input) = self.net_list.to_const(sel).map(|const_val| {
                    if const_val.val > 0 {
                        input1
                    } else {
                        input2
                    }
                }) {
                    let out_id =
                        self.net_list[node_id].only_one_out().node_out_id().idx();
                    self.net_list
                        .reconnect_input_by_ind(node_id, out_id, new_input);
                }

                None
            }

            NodeKind::Case(node) => {
                let CaseInputs {
                    sel,
                    default,
                    variant_inputs,
                    variants,
                } = node.inputs();

                if let Some(new_input) =
                    self.net_list.to_const(sel).and_then(|const_val| {
                        for (mask, variant) in variants.iter().zip(variant_inputs) {
                            if mask.is_match(const_val) {
                                return Some(variant);
                            }
                        }

                        if let Some(default) = default {
                            return Some(default);
                        }

                        None
                    })
                {
                    let out_id =
                        self.net_list[node_id].only_one_out().node_out_id().idx();
                    self.net_list
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
        if let Some(top) = self.net_list.top_module() {
            self.visit_module(top);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let mut cursor = self.net_list.mod_cursor(module_id);
        while let Some(node_id) = self.net_list.next(&mut cursor) {
            if let Some(new_node) = self.transform(node_id, &mut cursor) {
                self.net_list.replace(node_id, new_node);
            }
        }
    }

    fn visit_node(&mut self, _node_id: NodeId) {}
}
