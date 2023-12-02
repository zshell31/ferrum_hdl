pub mod decoder;
pub mod encoder;

use std::{
    fs::File,
    io::{self, Seek, SeekFrom, Write},
    path::Path,
};

use fhdl_netlist::{
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{GenNode, NodeOutput},
    sig_ty::NodeTy,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::def_id::DefId;
use rustc_macros::{Decodable, Encodable};
use rustc_middle::ty::Ty;
use rustc_serialize::{Encodable, Encoder};

use self::encoder::NetListEncoder;
use super::Generator;

#[derive(Encodable, Decodable)]
struct Root {
    interpet_alloc_pos: usize,
}

#[derive(Debug, Default)]
pub struct Metadata<'tcx> {
    types: Vec<Ty<'tcx>>,
    types_map: FxHashMap<Ty<'tcx>, u32>,
    gen_nodes: GenNodes,
    modules: FxHashMap<DefId, ModuleId>,
}

pub type GenNodes = Vec<GenNodeKind>;

#[derive(Debug, Encodable, Decodable)]
pub enum GenNodeKind {
    CastToUnsigned { from: NodeOutId, to_ty: NodeTy },
}

impl<'tcx> Generator<'tcx> {
    pub fn add_gen_node(
        &mut self,
        module_id: ModuleId,
        gen_node_kind: GenNodeKind,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) -> NodeId {
        let idx = self.metadata.gen_nodes.len() as u32;
        self.metadata.gen_nodes.push(gen_node_kind);

        self.net_list
            .add(module_id, GenNode::new(idx, inputs, outputs))
    }

    pub fn add_gen_ty(&mut self, ty: Ty<'tcx>) -> u32 {
        match self.metadata.types_map.get(&ty) {
            Some(idx) => *idx,
            None => {
                let idx = self.metadata.types.len() as u32;
                self.metadata.types.push(ty);
                self.metadata.types_map.insert(ty, idx);
                idx
            }
        }
    }

    pub fn encode_netlist<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let mut encoder = NetListEncoder::new(self, path)?;
        let encoder = &mut encoder;

        // the root position
        encoder.emit_raw_bytes(&[0, 0, 0, 0]);

        self.metadata.types.encode(encoder);
        self.metadata.gen_nodes.encode(encoder);
        self.metadata.modules.encode(encoder);
        encoder.encode_netlist();
        let interpet_alloc_pos = encoder.encode_interpret_allocs();
        let root = Root { interpet_alloc_pos };
        let root_pos = encoder.encode(root);

        encoder.flush();
        let file = encoder.file();
        self.encode_root_pos(file, root_pos)
    }

    fn encode_root_pos(&self, mut file: &File, root_pos: usize) -> io::Result<()> {
        let pos_before_seek = file.stream_position()?;
        file.seek(SeekFrom::Start(0))?;
        file.write_all(&[
            (root_pos >> 24) as u8,
            (root_pos >> 16) as u8,
            (root_pos >> 8) as u8,
            root_pos as u8,
        ])?;

        file.seek(SeekFrom::Start(pos_before_seek))?;

        Ok(())
    }
}
