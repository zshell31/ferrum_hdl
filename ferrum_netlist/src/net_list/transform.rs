use super::{const_val::ConstVal, NetList, NodeOutId};
use crate::{
    arena::with_arena,
    node::{
        BinOp, BinOpNode, BitNotNode, Const, Merger, MultiConst, MultiPass, Node,
        NodeKind, NotNode, Pass,
    },
};

impl NetList {
    pub(crate) fn to_const(&self, node_out_id: NodeOutId) -> Option<ConstVal> {
        match &self[node_out_id.node_id()].kind {
            NodeKind::Const(Const { value, output }) => {
                Some(ConstVal::new(*value, output.width()))
            }
            NodeKind::MultiConst(MultiConst { values, outputs }) => {
                let out_id = node_out_id.out_id();
                Some(ConstVal::new(values[out_id], outputs[out_id].width()))
            }
            _ => None,
        }
    }

    pub(super) fn transform(&mut self, mut node: Node) -> Node {
        match node.kind {
            NodeKind::Const(_) => node,
            NodeKind::Pass(Pass { input, output, .. }) => match self.to_const(input) {
                Some(const_val) => {
                    Const::new(output.ty, const_val.val, output.sym).into()
                }
                None => {
                    if self[input.node_id()].from_const {
                        node.from_const = true;
                    }

                    node
                }
            },
            NodeKind::ModInst(mod_inst) => {
                let module_id = mod_inst.module_id;
                let module = &self[module_id];

                let values = module.outputs().map(|node_out_id| {
                    self.to_const(node_out_id).map(|const_val| const_val.val)
                });
                let values = unsafe { with_arena().alloc_from_opt_iter(values) };
                let outputs = unsafe {
                    with_arena().alloc_from_iter(
                        module.outputs().map(|node_out_id| self[node_out_id]),
                    )
                };

                match values {
                    Some(values) => MultiConst::new(values, outputs).into(),
                    None => {
                        let from_const = module
                            .outputs()
                            .all(|output| self[output.node_id()].from_const);

                        if from_const {
                            let inputs = module.outputs();
                            let outputs = module.outputs().map(|output| {
                                let output = self[output];
                                (output.ty, output.sym)
                            });

                            let mut node: Node = MultiPass::new(inputs, outputs).into();
                            node.from_const = true;

                            let module = &mut self[module_id];
                            module.inject = true;

                            node
                        } else {
                            mod_inst.into()
                        }
                    }
                }
            }
            NodeKind::Not(NotNode { input, output })
            | NodeKind::BitNot(BitNotNode { input, output }) => {
                match self.to_const(input) {
                    Some(const_val) => {
                        Const::new(output.ty, const_val.val, output.sym).into()
                    }
                    None => node,
                }
            }
            NodeKind::BinOp(BinOpNode {
                bin_op,
                inputs: (left, right),
                output,
            }) => match (self.to_const(left), self.to_const(right)) {
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

                    Const::new(output.ty, const_val.val, output.sym).into()
                }
                _ => node,
            },
            NodeKind::Splitter(splitter) => {
                let mut start = splitter.start(self);
                match self.to_const(splitter.input) {
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
                        let values = unsafe { with_arena().alloc_from_iter(values) };

                        MultiConst::new(values, splitter.outputs).into()
                    }
                    None => {
                        let output = splitter.outputs[0];
                        if splitter.outputs.len() == 1
                            && self[splitter.input].ty.width() == output.width()
                        {
                            Pass::new(output.ty, splitter.input, output.sym).into()
                        } else {
                            splitter.into()
                        }
                    }
                }
            }
            NodeKind::Merger(Merger { inputs, output, .. }) if inputs.len() == 1 => {
                match self.to_const(inputs[0]) {
                    Some(const_val) => {
                        Const::new(output.ty, const_val.val, output.sym).into()
                    }
                    None => Pass::new(output.ty, inputs[0], output.sym).into(),
                }
            }
            NodeKind::Merger(Merger { inputs, .. }) if inputs.len() > 1 => {
                if inputs.iter().all(|input| self[input.node_id()].from_const) {
                    node.from_const = true;
                }
                node
            }
            _ => node,
        }
    }
}
