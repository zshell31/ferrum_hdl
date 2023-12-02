use either::Either;
use ferrum_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{Const, IsNode, Merger, Pass, Splitter},
    params::Outputs,
    sig_ty::{EnumTy, PrimTy, SignalTy},
};
use smallvec::SmallVec;

use crate::generator::Generator;

impl<'tcx> Generator<'tcx> {
    pub fn item_ty(&self, item_id: ItemId) -> SignalTy {
        match item_id {
            ItemId::Node(node_id) => self.net_list[node_id]
                .kind
                .outputs()
                .only_one()
                .out
                .ty
                .into(),
            ItemId::Group(group) => group.sig_ty,
        }
    }

    pub fn combine_outputs(&mut self, node_id: NodeId) -> ItemId {
        let module_id = node_id.module_id();
        let outputs_len = self.net_list[node_id].kind.outputs().len();

        if outputs_len > 1 {
            let nodes = self.net_list[node_id]
                .kind
                .outputs()
                .items()
                .map(|out| {
                    (
                        out.out.ty,
                        Pass::new(
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

            self.make_struct_group(ty, nodes, |generator, (_, node)| {
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
                let node_outs = self.net_list[node_id].kind.outputs();
                if node_outs.len() > 1 {
                    let item_id = self.combine_outputs(node_id);
                    self.to_bitvec(module_id, item_id)
                } else {
                    let node_out = node_outs.only_one();
                    let width = node_out.out.ty.width();
                    let node_out_id = node_out.node_out_id(node_id);

                    let pass = Pass::new(
                        PrimTy::BitVec(width),
                        node_out_id,
                        self.idents.for_module(module_id).tmp(),
                    );
                    let node_id = self.net_list.add_node(module_id, pass);

                    self.net_list[node_id]
                        .kind
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
                        .map(|item_id| self.to_bitvec(module_id, *item_id)),
                    sym,
                    false,
                );

                let node_id = self.net_list.add_node(module_id, merger);
                self.net_list[node_id]
                    .kind
                    .outputs()
                    .only_one()
                    .node_out_id(node_id)
            }
        }
    }

    pub fn maybe_to_bitvec(&mut self, module_id: ModuleId, item_id: ItemId) -> NodeOutId {
        match item_id {
            ItemId::Node(node_id) => {
                let out = self.net_list[node_id].kind.outputs();
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
                // TODO: подумать, можно ли избавиться от Pass здесь
                // Если да, то это бы упростило код в функции pattern_match
                let pass =
                    Pass::new(ty, node_out_id, self.idents.for_module(module_id).tmp());
                self.net_list.add_node(module_id, pass).into()
            }
            SignalTy::Array(ty) => {
                let item_width = ty.item_width();

                let mut n = 0;
                let splitter = Splitter::new(
                    node_out_id,
                    ty.tys().map(|_| {
                        n += 1;
                        let sym = self.idents.for_module(module_id).tmp();
                        (PrimTy::BitVec(item_width), sym)
                    }),
                    None,
                    true,
                );

                let node_id = self.net_list.add_node(module_id, splitter);
                let outputs = self.net_list[node_id]
                    .kind
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
                    ty.tys().iter().map(|ty| {
                        n += 1;
                        let sym = self.idents.for_module(module_id).tmp();
                        (PrimTy::BitVec(ty.inner.width()), sym)
                    }),
                    None,
                    true,
                );

                let node_id = self.net_list.add_node(module_id, splitter);
                let outputs = self.net_list[node_id]
                    .kind
                    .outputs()
                    .items()
                    .map(|out| out.node_out_id(node_id))
                    .collect::<SmallVec<[_; 8]>>();

                assert_eq!(outputs.len(), n);

                self.make_struct_group(
                    ty,
                    ty.tys()
                        .iter()
                        .zip(outputs)
                        .map(|(ty, node_out_id)| (ty.inner, node_out_id)),
                    |generator, (sig_ty, node_out_id)| {
                        Ok(generator.from_bitvec(module_id, node_out_id, sig_ty))
                    },
                )
                .unwrap()
            }
            SignalTy::Enum(ty) => {
                let pass = Pass::new(
                    ty.prim_ty(),
                    node_out_id,
                    self.idents.for_module(module_id).tmp(),
                );
                self.net_list.add_node(module_id, pass).into()
            }
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

    pub fn enum_variant_from_bitvec(
        &mut self,
        module_id: ModuleId,
        scrutinee: NodeId,
        enum_ty: EnumTy,
        variant_idx: usize,
    ) -> ItemId {
        let sig_ty = enum_ty.variant(variant_idx).inner;

        if let Some(struct_ty) = sig_ty.opt_struct_ty() {
            if struct_ty.is_empty() {
                return scrutinee.into();
            }
        }

        let scrutinee_out = self.net_list[scrutinee].kind.outputs().only_one();
        let sym = self.idents.for_module(module_id).tmp();
        let scrutinee = scrutinee_out.node_out_id(scrutinee);

        let data_part = self.net_list.add_node(
            module_id,
            Splitter::new(
                scrutinee,
                [(PrimTy::BitVec(sig_ty.width()), sym)],
                Some(enum_ty.data_width()),
                true,
            ),
        );
        let data_part = self.net_list[data_part]
            .kind
            .outputs()
            .only_one()
            .node_out_id(data_part);

        self.from_bitvec(module_id, data_part, sig_ty)
    }

    pub fn enum_variant_to_bitvec(
        &mut self,
        module_id: ModuleId,
        enum_ty: EnumTy,
        variant_idx: usize,
        data_part: ItemId,
    ) -> ItemId {
        let discr_val = enum_ty.discr_val(variant_idx);
        let discr_val = self.net_list.add_node(
            module_id,
            Const::new(
                PrimTy::BitVec(enum_ty.discr_width()),
                discr_val,
                self.idents.for_module(module_id).tmp(),
            ),
        );
        let discr_val = self.net_list[discr_val]
            .kind
            .outputs()
            .only_one()
            .node_out_id(discr_val);

        let inputs = if data_part.is_empty() {
            Either::Left([discr_val].into_iter())
        } else {
            let data_part = self.to_bitvec(module_id, data_part);
            Either::Right([discr_val, data_part].into_iter())
        };

        self.net_list
            .add_node(
                module_id,
                Merger::new(
                    enum_ty.width(),
                    inputs,
                    self.idents.for_module(module_id).tmp(),
                    false,
                ),
            )
            .into()
    }
}
