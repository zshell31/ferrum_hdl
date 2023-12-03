pub mod decoder;
pub mod encoder;
pub mod resolver;

use std::{
    fs::{self, File},
    io::{self, Seek, SeekFrom, Write},
    path::Path,
    rc::Rc,
};

use fhdl_netlist::{
    net_list::{Idx, ModuleId, NetList, NodeId, TempNodeId, TyId},
    node::TemplateNode,
    sig_ty::NodeTy,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::def_id::{CrateNum, DefId};
use rustc_macros::{Decodable, Encodable};
use rustc_middle::{mir::interpret::AllocDecodingState, ty::Ty};
use rustc_serialize::{opaque::MemDecoder, Decodable, Encodable, Encoder};
use rustc_session::config::OutputType;

use self::encoder::NetListEncoder;
use super::{temp_nodes::TemplateNodeKind, Generator};
use crate::{error::Error, generator::metadata::decoder::NetListDecoder};

const NETLIST_EXT: &str = "netlist";
const METADATA_VERSION: u8 = 1;
const ROOT_POS: u64 = 4;
const METADATA_HEADER: &[u8] = &[METADATA_VERSION, 0, 0, 0, 0, 0, 0, 0];

#[derive(Encodable, Decodable)]
struct Root {
    interpret_alloc_pos: usize,
}

#[derive(Debug, Default)]
pub struct Metadata<'tcx> {
    types: Vec<Ty<'tcx>>,
    types_ids: FxHashMap<Ty<'tcx>, TyId>,
    template_nodes: TemplateNodes,
    modules: FxHashMap<DefId, ModuleId>,
    netlist: Option<NetList>,
}

impl<'tcx> Metadata<'tcx> {
    pub fn add_module_id(&mut self, did: impl Into<DefId>, module_id: ModuleId) {
        self.modules.insert(did.into(), module_id);
    }

    pub fn find_module_id_by_def_id(&self, def_id: DefId) -> Option<ModuleId> {
        self.modules.get(&def_id).copied()
    }

    pub fn template_node(&self, id: TempNodeId) -> Option<&TemplateNodeKind> {
        self.template_nodes.get(id.idx())
    }

    pub fn netlist(&self) -> &NetList {
        self.netlist.as_ref().unwrap()
    }
}

pub type TemplateNodes = Vec<TemplateNodeKind>;

impl<'tcx> Generator<'tcx> {
    pub fn add_temp_node(
        &mut self,
        module_id: ModuleId,
        template_node_kind: TemplateNodeKind,
        output_ty: NodeTy,
    ) -> NodeId {
        let id = TempNodeId::new(self.metadata.template_nodes.len());
        self.metadata.template_nodes.push(template_node_kind);

        self.netlist
            .add(module_id, TemplateNode::new(id, output_ty))
    }

    pub fn add_generic_ty(&mut self, ty: Ty<'tcx>) -> TyId {
        match self.metadata.types_ids.get(&ty) {
            Some(idx) => *idx,
            None => {
                let id = TyId::new(self.metadata.types.len());
                self.metadata.types.push(ty);
                self.metadata.types_ids.insert(ty, id);
                id
            }
        }
    }

    pub fn encode_netlist(&self) -> Result<(), Error> {
        let output_filenames = self.tcx.output_filenames(());
        let path = output_filenames
            .output_path(OutputType::Metadata)
            .with_extension(NETLIST_EXT);
        self.encode_netlist_(path)?;

        Ok(())
    }

    fn encode_netlist_<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let mut encoder = NetListEncoder::new(self, path)?;
        let encoder = &mut encoder;

        // the version and the root position
        encoder.emit_raw_bytes(METADATA_HEADER);

        self.metadata.types.encode(encoder);
        self.metadata.template_nodes.encode(encoder);
        self.metadata.modules.encode(encoder);
        encoder.encode_netlist();
        let interpret_alloc_pos = encoder.encode_interpret_allocs();
        let root = Root {
            interpret_alloc_pos,
        };
        let root_pos = encoder.encode(root);

        encoder.flush();
        let file = encoder.file();
        self.encode_root_pos(file, root_pos)
    }

    fn encode_root_pos(&self, mut file: &File, root_pos: usize) -> io::Result<()> {
        let pos_before_seek = file.stream_position()?;
        file.seek(SeekFrom::Start(ROOT_POS))?;
        file.write_all(&[
            (root_pos >> 24) as u8,
            (root_pos >> 16) as u8,
            (root_pos >> 8) as u8,
            root_pos as u8,
        ])?;

        file.seek(SeekFrom::Start(pos_before_seek))?;

        Ok(())
    }

    pub fn find_metadata_for_crate(
        &mut self,
        crate_num: CrateNum,
    ) -> Result<Rc<Metadata<'tcx>>, Error> {
        if !self.loaded_metadata.contains_key(&crate_num) {
            self.decode_netlist(crate_num)?;
        }

        Ok(self.loaded_metadata.get(&crate_num).cloned().unwrap())
    }

    fn decode_netlist(&mut self, crate_num: CrateNum) -> Result<(), Error> {
        let crate_source = self.tcx.used_crate_source(crate_num);
        let path = crate_source.paths().next().ok_or_else(|| {
            Error::MissingMetadata(self.tcx.crate_name(crate_num).to_string())
        })?;
        let metadata_path = path.with_extension(NETLIST_EXT);
        if metadata_path.exists() {
            let metadata = self.decode_netlist_(metadata_path)?;
            self.loaded_metadata.insert(crate_num, Rc::new(metadata));

            Ok(())
        } else {
            let name = self.tcx.crate_name(crate_num).to_string();
            Err(Error::FileIsNotMetadata {
                path: metadata_path,
                name,
            })
        }
    }

    fn decode_netlist_<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Result<Metadata<'tcx>, Error> {
        let file = fs::read(path)?;
        let data = file.as_slice();

        let mut decoder = MemDecoder::new(data, 0);
        let version = u32::decode(&mut decoder);
        if version != METADATA_VERSION as u32 {
            return Err(Error::IncompatibleVersions {
                expected: METADATA_VERSION as u32,
                found: version,
            });
        }

        let root_pos = u32::decode(&mut decoder);

        let mut decoder = MemDecoder::new(data, root_pos as usize);
        let root = Root::decode(&mut decoder);
        let interpret_alloc_pos = root.interpret_alloc_pos;

        let mut decoder = MemDecoder::new(data, interpret_alloc_pos);
        let data_offsets = Vec::<u64>::decode(&mut decoder);

        let alloc_decoding_state = AllocDecodingState::new(data_offsets);
        let alloc_decoding_sess = alloc_decoding_state.new_decoding_session();

        let opaque = MemDecoder::new(data, METADATA_HEADER.len());
        let mut decoder = NetListDecoder::new(self, opaque, alloc_decoding_sess);

        let types = Decodable::decode(&mut decoder);
        let gen_nodes = Decodable::decode(&mut decoder);
        let modules = Decodable::decode(&mut decoder);
        let netlist = decoder.decode_netlist();

        Ok(Metadata {
            types,
            types_ids: Default::default(),
            template_nodes: gen_nodes,
            modules,
            netlist: Some(netlist),
        })
    }
}
