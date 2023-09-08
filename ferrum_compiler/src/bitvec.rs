use std::iter;

use ferrum_netlist::{
    group_list::{Group, GroupKind, ItemId},
    net_list::{ModuleId, NodeOutId},
    node::{IsNode, Merger, PassNode, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};

use crate::generator::Generator;

pub struct ArrayDesc {
    pub count: usize,
    pub width: u128,
}

impl<'tcx> Generator<'tcx> {
    pub fn item_width(&self, item_id: ItemId) -> u128 {
        let mut width = 0;
        let _ = self
            .group_list
            .deep_iter::<(), _>(&[item_id], &mut |_, node_id| {
                width += self.net_list[node_id].outputs().only_one().out.ty.width();

                Ok(())
            });

        width
    }

    pub fn item_ty(&self, item_id: ItemId) -> SignalTy {
        match item_id {
            ItemId::Node(node_id) => {
                self.net_list[node_id].outputs().only_one().out.ty.into()
            }
            ItemId::Group(group_id) => {
                let group = self.group_list[group_id];
                match group.kind {
                    GroupKind::Group => SignalTy::group(
                        group.item_ids.iter().map(|item_id| self.item_ty(*item_id)),
                    ),
                    GroupKind::Array => {
                        let n = group.item_ids.len();
                        let sig_ty = self.item_ty(group.item_ids[0]);
                        SignalTy::array(n as u128, sig_ty)
                    }
                }
            }
        }
    }

    pub fn array_desc(&self, item_id: ItemId) -> ArrayDesc {
        self.opt_array_desc(item_id).expect("Expected array")
    }

    pub fn opt_array_desc(&self, item_id: ItemId) -> Option<ArrayDesc> {
        match item_id {
            ItemId::Node(_) => None,
            ItemId::Group(group_id) => {
                let group = self.group_list[group_id];
                match group.kind {
                    GroupKind::Array => {
                        let count = group.item_ids.len();
                        let width = self.item_width(group.item_ids[0]);
                        Some(ArrayDesc { count, width })
                    }
                    _ => None,
                }
            }
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_bitvec(&mut self, module_id: ModuleId, item_id: ItemId) -> NodeOutId {
        match item_id {
            ItemId::Node(node_id) => {
                let node_out = self.net_list[node_id].outputs().only_one();
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
            ItemId::Group(group_id) => {
                let group = &self.group_list[group_id];

                match group.kind {
                    GroupKind::Array | GroupKind::Group => {
                        let width = self.item_width(item_id);
                        let sym = self.idents.for_module(module_id).tmp();
                        let merger = Merger::new(
                            width,
                            group
                                .item_ids
                                .iter()
                                .map(|item_id| self.to_bitvec(module_id, *item_id)),
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
            SignalTy::Array(n, sig_ty) => {
                let width = sig_ty.width();

                self.to_bitvec_inner(
                    module_id,
                    node_out_id,
                    iter::repeat((width, *sig_ty)).take(n as usize),
                )
            }
            SignalTy::Group(ty) => self.to_bitvec_inner(
                module_id,
                node_out_id,
                ty.iter().map(|ty| (ty.width(), *ty)),
            ),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_bitvec_inner(
        &mut self,
        module_id: ModuleId,
        node_out_id: NodeOutId,
        sig_ty: impl Iterator<Item = (u128, SignalTy)> + Clone,
    ) -> ItemId {
        let mut n = 0;

        let splitter = Splitter::new(
            node_out_id,
            sig_ty.clone().map(|(width, _)| {
                n += 1;
                let sym = self.idents.for_module(module_id).tmp();
                (PrimTy::BitVec(width), sym)
            }),
            None,
        );

        let node_id = self.net_list.add_node(module_id, splitter);
        let outputs = self.net_list[node_id]
            .outputs()
            .items()
            .map(|out| out.node_out_id(node_id))
            .collect::<Vec<_>>();

        assert_eq!(outputs.len(), n);

        let group = Group::new(
            GroupKind::Array,
            sig_ty.zip(outputs).map(|((_, sig_ty), node_out_id)| {
                self.from_bitvec(module_id, node_out_id, sig_ty)
            }),
        );

        self.group_list.add_group(group).into()
    }
}
