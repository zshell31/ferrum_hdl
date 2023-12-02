use crate::{
    arena::Vec,
    const_val::ConstVal,
    net_list::{ModuleId, NetList, NodeCursor, NodeId},
    node::{
        BinOp, BinOpNode, BitNot, CaseInputs, Const, Merger, MultiConst, NodeKind, Not,
        ZeroExtend,
    },
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
        if let NodeKind::ModInst(mod_inst) = &*self.net_list[node_id].kind {
            let module_id = mod_inst.module_id;

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
        let node = &self.net_list[node_id];
        match &*node.kind {
            NodeKind::ModInst(mod_inst) => {
                let module_id = mod_inst.module_id;

                let values = self.net_list.mod_outputs(module_id).map(|node_out_id| {
                    self.net_list
                        .to_const(node_out_id)
                        .map(|const_val| const_val.val)
                });
                let values = Vec::collect_from_opt(values);

                match values {
                    Some(values) => {
                        let outputs = Vec::collect_from(
                            self.net_list
                                .mod_outputs(module_id)
                                .map(|node_out_id| self.net_list[node_out_id]),
                        );
                        Some(MultiConst::new(values, outputs).into())
                    }
                    None => {
                        *should_be_inlined = mod_inst.inlined
                            || node
                                .inputs()
                                .all(|input| self.net_list[input.node_id()].is_const());

                        None
                    }
                }
            }
            NodeKind::Not(Not { input, output })
            | NodeKind::BitNot(BitNot { input, output }) => {
                self.net_list.to_const(*input).map(|const_val| {
                    let const_val = !const_val;
                    Const::new(output.ty, const_val.val, output.sym).into()
                })
            }

            NodeKind::BinOp(BinOpNode {
                bin_op,
                inputs: (left, right),
                output,
            }) => match (
                self.net_list.to_const(*left),
                self.net_list.to_const(*right),
            ) {
                (Some(left), Some(right)) => {
                    let const_val = match bin_op {
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

                    Some(Const::new(output.ty, const_val.val, output.sym).into())
                }
                _ => None,
            },
            NodeKind::Splitter(splitter) => {
                let mut start = splitter.start(self.net_list);
                match self.net_list.to_const(splitter.input) {
                    Some(input) => {
                        let rev = splitter.rev;
                        let values = splitter.outputs.iter().map(|output| {
                            let width = output.width();
                            let val = input.slice(start, width, rev).val;
                            if !rev {
                                start += width;
                            } else {
                                start -= width;
                            };
                            val
                        });

                        Some(
                            MultiConst::new(values, splitter.outputs.iter().copied())
                                .into(),
                        )
                    }
                    None => {
                        if splitter.pass_all_bits(self.net_list) {
                            self.net_list.reconnect(node_id);
                        }

                        None
                    }
                }
            }
            NodeKind::Merger(Merger { inputs, output, .. }) if inputs.len() == 1 => self
                .net_list
                .to_const(inputs[0])
                .map(|const_val| Const::new(output.ty, const_val.val, output.sym).into()),
            NodeKind::Merger(Merger {
                ref inputs, output, ..
            }) if inputs.len() > 1 => {
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
                inputs
                    .iter()
                    .for_each(|input| match self.net_list.to_const(*input) {
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
                    Const::new(output.ty, const_val.val, output.sym).into()
                })
            }

            NodeKind::ZeroExtend(ZeroExtend { input, output }) => {
                match self.net_list.to_const(*input) {
                    Some(const_val) => {
                        Some(Const::new(output.ty, const_val.val, output.sym).into())
                    }
                    None => {
                        if self.net_list[*input].width() == output.width() {
                            self.net_list.reconnect(node_id);
                        }

                        None
                    }
                }
            }

            NodeKind::Case(node) => {
                let CaseInputs {
                    sel,
                    default,
                    variant_inputs,
                    variants,
                } = node.case_inputs();

                if let Some(new_input) =
                    self.net_list.to_const(sel).and_then(|const_val| {
                        for (mask, variant) in variants.iter().zip(variant_inputs) {
                            if mask.is_match(const_val) {
                                return Some(*variant);
                            }
                        }

                        if let Some(default) = default {
                            return Some(default);
                        }

                        None
                    })
                {
                    let out_id =
                        self.net_list[node_id].only_one_out().node_out_id().out_id();
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
