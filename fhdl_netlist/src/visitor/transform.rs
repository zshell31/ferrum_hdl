use crate::{
    arena::Vec,
    const_val::ConstVal,
    net_list::{ModuleId, NetList, NodeCursor, NodeId},
    node::{
        BinOp, BinOpNode, BitNot, Case, CaseInputs, Const, IsNode, Merger, MultiConst,
        Node, NodeKind, Not, Pass, ZeroExtend,
    },
    params::Inputs,
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

    fn transform(&mut self, node_id: NodeId, cursor: &mut NodeCursor) -> Option<Node> {
        if let NodeKind::ModInst(mod_inst) = &self.net_list[node_id].kind {
            let module_id = mod_inst.module_id;

            // transform nodes before transforming mod instance
            self.visit_module(module_id);
        };

        let mut should_be_inlined = false;
        let res = self.transform_(node_id, &mut should_be_inlined);

        let prev = self.net_list[node_id].prev();
        if should_be_inlined && self.net_list.inline_mod(node_id) {
            cursor.set_node_id(prev);
        }

        res
    }

    fn transform_(
        &mut self,
        node_id: NodeId,
        should_be_inlined: &mut bool,
    ) -> Option<Node> {
        let node = &self.net_list[node_id];
        match &node.kind {
            NodeKind::Pass(Pass { input, output, .. }) => self
                .net_list
                .to_const(*input)
                .map(|const_val| Const::new(output.ty, const_val.val, output.sym).into()),
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
                            || mod_inst.inputs().items().all(|input| {
                                self.net_list[input.node_id()].kind.is_const()
                            });

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
                        let mut values = splitter.outputs.iter().map(|output| {
                            let width = output.width();
                            let val = input.slice(start, width, rev).val;
                            if !rev {
                                start += width;
                            } else {
                                start -= width;
                            };
                            val
                        });

                        Some(if splitter.outputs.len() > 1 {
                            let values = Vec::collect_from(values);

                            MultiConst::new(values, splitter.outputs.iter().copied())
                                .into()
                        } else {
                            let output = splitter.outputs[0];
                            Const::new(output.ty, values.next().unwrap(), output.sym)
                                .into()
                        })
                    }
                    None => {
                        let output = splitter.outputs[0];
                        if splitter.outputs.len() == 1
                            && splitter.start.is_none()
                            && self.net_list[splitter.input].ty.width() == output.width()
                        {
                            Some(Pass::new(output.ty, splitter.input, output.sym).into())
                        } else {
                            None
                        }
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
                            Some(Pass::new(output.ty, *input, output.sym).into())
                        } else {
                            None
                        }
                    }
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
            }) => self.net_list.to_const(*sel).and_then(|const_val| {
                for (mask, variant) in variants {
                    if mask.is_match(const_val) {
                        return Some(Pass::new(output.ty, *variant, output.sym).into());
                    }
                }

                if let Some(default) = default {
                    return Some(Pass::new(output.ty, *default, output.sym).into());
                }

                None
            }),
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
            self.net_list.exclude_pass_nodes(node_id);

            if let Some(new_node) = self.transform(node_id, &mut cursor) {
                self.net_list.replace_node(node_id, new_node);
            }
        }

        let mut cursor = self.net_list.mod_cursor(module_id);
        while let Some(node_id) = self.net_list.next(&mut cursor) {
            self.net_list.exclude_pass_nodes(node_id);
        }
    }

    fn visit_node(&mut self, _node_id: NodeId) {}
}
