use std::iter::Peekable;

use either::Either;
use fhdl_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{Const, Merger, Splitter},
    sig_ty::{EnumTy, NodeTy, SignalTy, SignalTyKind},
};
use smallvec::{smallvec, SmallVec};

use crate::generator::Generator;

pub type CombineOutputsIter = impl Iterator<Item = NodeOutId>;

pub struct CombineOutputs {
    outputs: Peekable<CombineOutputsIter>,
}

impl CombineOutputs {
    pub fn new(generator: &Generator<'_>, node_id: NodeId) -> Self {
        Self {
            outputs: generator.netlist[node_id]
                .node_out_ids()
                .collect::<Vec<_>>()
                .into_iter()
                .peekable(),
        }
    }

    pub fn next_output(
        &mut self,
        generator: &mut Generator<'_>,
        sig_ty: SignalTy,
    ) -> ItemId {
        match sig_ty.kind {
            SignalTyKind::Node(_) | SignalTyKind::Enum(_) => {
                self.outputs.next().unwrap().into()
            }
            SignalTyKind::Array(ty) => generator
                .make_array_group(ty, ty.tys(), |generator, sig_ty| {
                    Ok(self.next_output(generator, sig_ty))
                })
                .unwrap(),
            SignalTyKind::Struct(ty) => generator
                .make_struct_group(ty, ty.tys(), |generator, sig_ty| {
                    Ok(self.next_output(generator, sig_ty.inner))
                })
                .unwrap(),
        }
    }

    pub fn has_outputs(&mut self) -> bool {
        self.outputs.peek().is_some()
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn combine_outputs(&mut self, node_id: NodeId, sig_ty: SignalTy) -> ItemId {
        let mut outputs = CombineOutputs::new(self, node_id);
        let res = outputs.next_output(self, sig_ty);
        assert!(!outputs.has_outputs());
        res
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

                    self.netlist.add_and_get_out(module_id, merger)
                }
            }
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_rev_bitvec(&mut self, module_id: ModuleId, item_id: ItemId) -> NodeOutId {
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
                        true,
                        None,
                    );

                    self.netlist.add_and_get_out(module_id, merger)
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
        let node_width = self.netlist[node_out_id].ty.width();
        if let (Some(node_width), Some(sig_ty_width)) =
            (node_width.opt_value(), sig_ty.width().opt_value())
        {
            assert_eq!(node_width, sig_ty_width);
        }

        match sig_ty.kind {
            SignalTyKind::Node(node_ty) => {
                self.netlist[node_out_id].ty = node_ty;
                node_out_id.into()
            }
            SignalTyKind::Enum(_) => node_out_id.into(),
            SignalTyKind::Array(ty) => {
                let outputs = if ty.count() == 1 {
                    smallvec![node_out_id]
                } else {
                    let mut n = 0;
                    let splitter = Splitter::new(
                        node_out_id,
                        ty.tys().map(|_| {
                            n += 1;
                            (ty.item_ty().to_bitvec(), None)
                        }),
                        None,
                        true,
                    );

                    let node_id = self.netlist.add(module_id, splitter);
                    let outputs = self.netlist[node_id]
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
                            (ty.inner.to_bitvec(), None)
                        }),
                        None,
                        true,
                    );

                    let node_id = self.netlist.add(module_id, splitter);
                    let outputs = self.netlist[node_id]
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

        let data_part = self.netlist.add(
            module_id,
            Splitter::new(
                scrutinee,
                [(NodeTy::BitVec(sig_ty.width()), None)],
                Some(enum_ty.data_width()),
                true,
            ),
        );
        let data_part = self.netlist[data_part].only_one_out().node_out_id();

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
        let discr_val = self.netlist.add(
            module_id,
            Const::new(
                NodeTy::BitVec(enum_ty.discr_width().into()),
                discr_val.into(),
                None,
            ),
        );
        let discr_val = self.netlist[discr_val].only_one_out().node_out_id();

        let inputs = if data_part.is_empty() {
            Either::Left([discr_val].into_iter())
        } else {
            let data_part = self.to_bitvec(module_id, data_part);
            Either::Right([discr_val, data_part].into_iter())
        };

        self.netlist
            .add_and_get_out(module_id, Merger::new(enum_ty.width(), inputs, false, None))
            .into()
    }
}
