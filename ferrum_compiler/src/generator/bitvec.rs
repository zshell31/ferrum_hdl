use ferrum_netlist::{
    group_list::ItemId,
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{ConstNode, IsNode, Merger, ModInst, Node, PassNode, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use smallvec::SmallVec;

use crate::generator::Generator;

impl<'tcx> Generator<'tcx> {
    pub fn item_ty(&self, item_id: ItemId) -> SignalTy {
        match item_id {
            ItemId::Node(node_id) => {
                self.net_list[node_id].outputs().only_one().out.ty.into()
            }
            ItemId::Group(group) => group.sig_ty,
        }
    }

    pub fn combine_outputs(&mut self, node_id: NodeId) -> ItemId {
        let module_id = node_id.module_id();
        let outputs_len = self.net_list[node_id].outputs().len();

        if outputs_len > 1 {
            let nodes = self.net_list[node_id]
                .outputs()
                .items()
                .map(|out| {
                    (
                        out.out.ty,
                        PassNode::new(
                            out.out.ty,
                            out.node_out_id(node_id),
                            self.idents.for_module(module_id).tmp(),
                        ),
                    )
                })
                .collect::<SmallVec<[_; 8]>>();

            let ty = self
                .make_tuple_ty(nodes.iter(), |_, (ty, _)| Ok((*ty).into()))
                .unwrap();

            self.make_tuple_group(ty, nodes, |generator, (_, node)| {
                Ok(generator.net_list.add_node(module_id, node).into())
            })
            .unwrap()
        } else {
            node_id.into()
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_bitvec(&mut self, module_id: ModuleId, item_id: ItemId) -> NodeOutId {
        match item_id {
            ItemId::Node(node_id) => {
                let node_outs = self.net_list[node_id].outputs();
                if node_outs.len() > 1 {
                    let item_id = self.combine_outputs(node_id);
                    self.to_bitvec(module_id, item_id)
                } else {
                    let node_out = node_outs.only_one();
                    let width = node_out.out.ty.width();
                    let node_out_id = node_out.node_out_id(node_id);

                    let pass = PassNode::new(
                        PrimTy::BitVec(width),
                        node_out_id,
                        self.idents.for_module(module_id).tmp(),
                    );
                    let node_id = self.net_list.add_node(module_id, pass);

                    self.net_list[node_id]
                        .outputs()
                        .only_one()
                        .node_out_id(node_id)
                }
            }
            ItemId::Group(group) => {
                let width = group.width();
                let sym = self.idents.for_module(module_id).tmp();

                let merger = Merger::new(
                    width,
                    group
                        .item_ids()
                        .iter()
                        .map(|item_id| self.to_bitvec(module_id, item_id.inner)),
                    sym,
                );

                let node_id = self.net_list.add_node(module_id, merger);
                self.net_list[node_id]
                    .outputs()
                    .only_one()
                    .node_out_id(node_id)
            }
        }
    }

    pub fn maybe_to_bitvec(&mut self, module_id: ModuleId, item_id: ItemId) -> NodeOutId {
        match item_id {
            ItemId::Node(node_id) => {
                let out = self.net_list[node_id].outputs();
                if out.len() == 1 {
                    out.only_one().node_out_id(node_id)
                } else {
                    self.to_bitvec(module_id, item_id)
                }
            }
            ItemId::Group(_) => self.to_bitvec(module_id, item_id),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn from_bitvec(
        &mut self,
        module_id: ModuleId,
        node_out_id: NodeOutId,
        sig_ty: SignalTy,
    ) -> ItemId {
        let node_width = self.net_list[node_out_id].ty.width();
        assert_eq!(node_width, sig_ty.width());

        match sig_ty {
            SignalTy::Prim(ty) => {
                let pass = PassNode::new(
                    ty,
                    node_out_id,
                    self.idents.for_module(module_id).tmp(),
                );
                self.net_list.add_node(module_id, pass).into()
            }
            SignalTy::Array(ty) => {
                let item_width = ty.width();

                let mut n = 0;
                let splitter = Splitter::new(
                    node_out_id,
                    ty.tys().map(|_| {
                        n += 1;
                        let sym = self.idents.for_module(module_id).tmp();
                        (PrimTy::BitVec(item_width), sym)
                    }),
                    None,
                );

                let node_id = self.net_list.add_node(module_id, splitter);
                let outputs = self.net_list[node_id]
                    .outputs()
                    .items()
                    .map(|out| out.node_out_id(node_id))
                    .collect::<SmallVec<[_; 8]>>();

                assert_eq!(outputs.len(), n);

                self.make_array_group(
                    ty,
                    ty.tys().zip(outputs),
                    |generator, (sig_ty, node_out_id)| {
                        Ok(generator.from_bitvec(module_id, node_out_id, sig_ty))
                    },
                )
                .unwrap()
            }
            SignalTy::Struct(ty) => {
                let mut n = 0;
                let splitter = Splitter::new(
                    node_out_id,
                    ty.tys().map(|ty| {
                        n += 1;
                        let sym = self.idents.for_module(module_id).tmp();
                        (PrimTy::BitVec(ty.inner.width()), sym)
                    }),
                    None,
                );

                let node_id = self.net_list.add_node(module_id, splitter);
                let outputs = self.net_list[node_id]
                    .outputs()
                    .items()
                    .map(|out| out.node_out_id(node_id))
                    .collect::<SmallVec<[_; 8]>>();

                assert_eq!(outputs.len(), n);

                self.make_struct_group(
                    ty,
                    ty.tys()
                        .zip(outputs)
                        .map(|(ty, node_out_id)| (ty.name, (ty.inner, node_out_id))),
                    |generator, (sig_ty, node_out_id)| {
                        Ok(generator.from_bitvec(module_id, node_out_id, sig_ty))
                    },
                )
                .unwrap()
            } // SignalTy::Enum(ty) => {
              //     let pass = PassNode::new(
              //         ty.descr_ty(),
              //         node_out_id,
              //         self.idents.for_module(module_id).tmp(),
              //     );
              //     self.net_list.add_node(module_id, pass).into()
              // }
        }
    }

    pub fn maybe_from_bitvec(
        &mut self,
        module_id: ModuleId,
        node_out_id: NodeOutId,
        sig_ty: SignalTy,
    ) -> ItemId {
        self.from_bitvec(module_id, node_out_id, sig_ty)
    }

    pub fn to_const(&self, item_id: ItemId) -> Option<u128> {
        self.to_const_inner(item_id).map(|(val, _)| val)
    }

    pub fn to_const_inner(&self, item_id: ItemId) -> Option<(u128, u128)> {
        match item_id {
            ItemId::Node(node_id) => {
                let node = &self.net_list[node_id];
                match node {
                    Node::Const(ConstNode { value, output, .. }) => {
                        Some((*value, output.ty.width()))
                    }
                    Node::ModInst(ModInst { module_id, .. }) => {
                        let module = &self.net_list[*module_id];
                        if module.outputs_len() > 1 {
                            None
                        } else {
                            let output = module.outputs().next()?;
                            self.to_const_inner(output.node_id().into())
                        }
                    }
                    Node::Pass(PassNode { input, .. }) => {
                        let item_id = input.node_id().into();
                        self.to_const_inner(item_id)
                    }
                    Node::Splitter(Splitter { input, .. }) => {
                        let item_id = input.node_id().into();
                        self.to_const_inner(item_id)
                    }
                    _ => None,
                }
            }
            ItemId::Group(group) => {
                let mut res: u128 = 0;
                let mut total: u128 = 0;
                for item_id in group.item_ids().iter().rev() {
                    let (val, width) = self.to_const_inner(item_id.inner)?;
                    // TODO: use long arithmetic instead
                    if res == 0 {
                        res = val;
                    } else {
                        res = (res << width) | val;
                    }
                    total = total.checked_add(width).unwrap();
                }

                Some((res, total))
            }
        }
    }
}
