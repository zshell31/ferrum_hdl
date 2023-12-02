use either::Either;
use fhdl_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{Const, Merger, Splitter},
    sig_ty::{EnumTy, NodeTy, SignalTy, SignalTyKind},
};
use smallvec::{smallvec, SmallVec};

use crate::generator::Generator;

impl<'tcx> Generator<'tcx> {
    pub fn combine_outputs(&mut self, node_id: NodeId, sig_ty: SignalTy) -> ItemId {
        let mut outputs = self.net_list[node_id]
            .node_out_ids()
            .collect::<SmallVec<[_; 8]>>()
            .into_iter();

        let res = self.combine_outputs_(&mut outputs, sig_ty);
        assert!(outputs.next().is_none());
        res
    }

    fn combine_outputs_<I: Iterator<Item = NodeOutId>>(
        &mut self,
        outputs: &mut I,
        sig_ty: SignalTy,
    ) -> ItemId {
        match sig_ty.kind {
            SignalTyKind::Node(_) | SignalTyKind::Enum(_) => {
                outputs.next().unwrap().into()
            }
            SignalTyKind::Array(ty) => self
                .make_array_group(ty, ty.tys(), |generator, sig_ty| {
                    Ok(generator.combine_outputs_(outputs, sig_ty))
                })
                .unwrap(),
            SignalTyKind::Struct(ty) => self
                .make_struct_group(ty, ty.tys(), |generator, sig_ty| {
                    Ok(generator.combine_outputs_(outputs, sig_ty.inner))
                })
                .unwrap(),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_bitvec(&mut self, module_id: ModuleId, item_id: ItemId) -> NodeOutId {
        match item_id {
            ItemId::Node(node_out_id) => node_out_id,
            ItemId::Group(group) => {
                if group.item_ids().len() == 1 {
                    self.to_bitvec(module_id, group.item_ids()[0])
                } else {
                    let width = group.width();

                    let merger = Merger::new(
                        width,
                        group
                            .item_ids()
                            .iter()
                            .map(|item_id| self.to_bitvec(module_id, *item_id)),
                        false,
                        None,
                    );

                    self.net_list.add_and_get_out(module_id, merger)
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

        match sig_ty.kind {
            SignalTyKind::Node(_) | SignalTyKind::Enum(_) => node_out_id.into(),
            SignalTyKind::Array(ty) => {
                let outputs = if ty.count() == 1 {
                    smallvec![node_out_id]
                } else {
                    let item_width = ty.item_width();

                    let mut n = 0;
                    let splitter = Splitter::new(
                        node_out_id,
                        ty.tys().map(|_| {
                            n += 1;
                            (NodeTy::BitVec(item_width), None)
                        }),
                        None,
                        true,
                    );

                    let node_id = self.net_list.add(module_id, splitter);
                    let outputs = self.net_list[node_id]
                        .node_out_ids()
                        .collect::<SmallVec<[_; 8]>>();

                    assert_eq!(outputs.len(), n);

                    outputs
                };

                self.make_array_group(
                    ty,
                    ty.tys().zip(outputs),
                    |generator, (sig_ty, node_out_id)| {
                        Ok(generator.from_bitvec(module_id, node_out_id, sig_ty))
                    },
                )
                .unwrap()
            }
            SignalTyKind::Struct(ty) => {
                let outputs = if ty.len() == 1 {
                    smallvec![node_out_id]
                } else {
                    let mut n = 0;
                    let splitter = Splitter::new(
                        node_out_id,
                        ty.tys().iter().map(|ty| {
                            n += 1;
                            (NodeTy::BitVec(ty.inner.width()), None)
                        }),
                        None,
                        true,
                    );

                    let node_id = self.net_list.add(module_id, splitter);
                    let outputs = self.net_list[node_id]
                        .outputs()
                        .map(|out| out.node_out_id())
                        .collect::<SmallVec<[_; 8]>>();

                    assert_eq!(outputs.len(), n);

                    outputs
                };

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
        }
    }

    pub fn enum_variant_from_bitvec(
        &mut self,
        module_id: ModuleId,
        scrutinee: NodeOutId,
        enum_ty: EnumTy,
        variant_idx: usize,
    ) -> ItemId {
        let sig_ty = enum_ty.variant(variant_idx).inner;

        let data_part = self.net_list.add(
            module_id,
            Splitter::new(
                scrutinee,
                [(NodeTy::BitVec(sig_ty.width()), None)],
                Some(enum_ty.data_width()),
                true,
            ),
        );
        let data_part = self.net_list[data_part].only_one_out().node_out_id();

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
        let discr_val = self.net_list.add(
            module_id,
            Const::new(NodeTy::BitVec(enum_ty.discr_width()), discr_val, None),
        );
        let discr_val = self.net_list[discr_val].only_one_out().node_out_id();

        let inputs = if data_part.is_empty() {
            Either::Left([discr_val].into_iter())
        } else {
            let data_part = self.to_bitvec(module_id, data_part);
            Either::Right([discr_val, data_part].into_iter())
        };

        self.net_list
            .add_and_get_out(module_id, Merger::new(enum_ty.width(), inputs, false, None))
            .into()
    }
}
