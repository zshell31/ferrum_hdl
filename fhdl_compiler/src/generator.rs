pub mod arena;
pub mod attr;
pub mod func;
pub mod item;
pub mod item_ty;
pub mod locals;
pub mod mir;

use std::{
    env, error::Error as StdError, fmt::Display, fs, io, mem::transmute,
    path::Path as StdPath,
};

use bumpalo::Bump;
use cargo_toml::Manifest;
use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    backend::Verilog,
    net_list::{ModuleId, NetList, NodeOutId},
    node::{Splitter, ZeroExtend},
    node_ty::NodeTy,
    symbol::Symbol,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def_id::{DefId, LOCAL_CRATE},
    ItemId as HirItemId, ItemKind,
};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::ty::{GenericArgs, GenericArgsRef, Ty, TyCtxt};
use rustc_span::def_id::CrateNum;
use serde::Deserialize;

use self::item_ty::ItemTy;
use crate::error::{Error, SpanError};

pub struct CompilerCallbacks {}

impl Callbacks for CompilerCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        let res = queries.global_ctxt().unwrap().enter(|tcx| {
            let is_primary = env::var("CARGO_PRIMARY_PACKAGE").is_ok();
            let should_be_synthesized = match should_be_synthesized(is_primary) {
                Ok(should_be_synthesized) => should_be_synthesized,
                Err(e) => {
                    tcx.sess.err(e.to_string());
                    return false;
                }
            };

            if should_be_synthesized {
                let arena = Bump::new();
                // SAFETY: the lifetime of the generator is shorter than the lifetime of the arena.
                let arena = unsafe { transmute::<&'_ Bump, &'tcx Bump>(&arena) };

                match init_generator(tcx, is_primary, arena) {
                    Ok(mut generator) => {
                        generator.generate();
                        true
                    }
                    Err(e) => {
                        tcx.sess.err(e.to_string());
                        false
                    }
                }
            } else {
                true
            }
        });

        if res {
            Compilation::Continue
        } else {
            Compilation::Stop
        }
    }
}

fn should_be_synthesized(is_primary: bool) -> Result<bool, Box<dyn StdError>> {
    if is_primary {
        Ok(true)
    } else {
        let manifest = env::var_os("CARGO_MANIFEST_DIR")
            .and_then(|var| var.to_str().map(|var| StdPath::new(var).join("Cargo.toml")))
            .ok_or("`CARGO_MANIFEST_DIR` is not specified")?;

        let manifest = fs::read(&manifest).map_err(|e| {
            format!(
                "Failed to read from `{}`: {}",
                manifest.to_string_lossy(),
                e
            )
        })?;

        #[allow(dead_code)]
        #[derive(Deserialize)]
        struct FhdlMetadata {
            #[serde(default)]
            synth: bool,
        }

        #[allow(dead_code)]
        #[derive(Deserialize)]
        struct Metadata {
            ferrum_hdl: Option<FhdlMetadata>,
        }

        let manifest = Manifest::<Metadata>::from_slice_with_metadata(&manifest)?;

        Ok(manifest
            .package
            .and_then(|package| package.metadata)
            .and_then(|metadata| metadata.ferrum_hdl)
            .map(|metadata| metadata.synth)
            .unwrap_or_default())
    }
}

fn init_generator<'tcx>(
    tcx: TyCtxt<'tcx>,
    is_primary: bool,
    arena: &'tcx Bump,
) -> Result<Generator<'tcx>, Error> {
    let top_module = find_top_module(tcx)?;
    let crates = Crates::find_crates(tcx, is_primary)?;

    Ok(Generator::new(tcx, top_module, crates, arena))
}

fn find_top_module(tcx: TyCtxt<'_>) -> Result<HirItemId, Error> {
    let hir = tcx.hir();
    for item_id in hir.items() {
        let item = hir.item(item_id);
        if let ItemKind::Fn(_, _, _) = item.kind {
            if item.ident.as_str() == "top_module" {
                return Ok(item_id);
            }
        }
    }

    Err(Error::MissingTopModule)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonoItem<'tcx>(DefId, GenericArgsRef<'tcx>);

impl<'tcx> MonoItem<'tcx> {
    pub fn new<T: Into<DefId>>(item_id: T, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self(item_id.into(), generic_args)
    }
}

struct Crates {
    core: CrateNum,
    std: CrateNum,
    ferrum_hdl: CrateNum,
}

impl Crates {
    fn find_crates(tcx: TyCtxt<'_>, is_primary: bool) -> Result<Self, Error> {
        const CORE: &str = "core";
        const STD: &str = "std";
        const FERRUM_HDL: &str = "ferrum_hdl";

        let mut core = None;
        let mut std = None;
        let mut ferrum_hdl = None;

        for krate in tcx.crates(()) {
            let crate_name = tcx.crate_name(*krate);
            let crate_name = crate_name.as_str();

            if crate_name == CORE {
                core = Some(*krate);
                continue;
            }
            if crate_name == STD {
                std = Some(*krate);
                continue;
            }
            if crate_name == FERRUM_HDL {
                ferrum_hdl = Some(*krate);
            }
        }

        let core = core.ok_or_else(|| Error::MissingCrate(CORE))?;
        let std = std.ok_or_else(|| Error::MissingCrate(STD))?;
        let ferrum_hdl = (if is_primary {
            ferrum_hdl
        } else {
            ferrum_hdl.or_else(|| {
                if tcx.crate_name(LOCAL_CRATE).as_str() == FERRUM_HDL {
                    Some(LOCAL_CRATE)
                } else {
                    None
                }
            })
        })
        .ok_or_else(|| Error::MissingCrate(FERRUM_HDL))?;

        Ok(Self {
            core,
            std,
            ferrum_hdl,
        })
    }

    pub(crate) fn is_core(&self, def_id: DefId) -> bool {
        def_id.krate == self.core || def_id.krate == self.std
    }

    pub(crate) fn is_ferrum_hdl(&self, def_id: DefId) -> bool {
        def_id.krate == self.ferrum_hdl
    }
}

pub struct Settings {
    pub trace_eval_expr: bool,
}

impl Settings {
    fn new() -> Self {
        Self {
            trace_eval_expr: env::var("TRACE_EVAL_EXPR").is_ok(),
        }
    }
}

pub struct Generator<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub netlist: NetList,
    pub settings: Settings,
    arena: &'tcx Bump,
    top_module: HirItemId,
    crates: Crates,
    blackbox: FxHashMap<DefId, Option<BlackboxKind>>,
    evaluated_modules: FxHashMap<MonoItem<'tcx>, ModuleId>,
    item_ty: FxHashMap<Ty<'tcx>, ItemTy<'tcx>>,
}

impl<'tcx> Generator<'tcx> {
    fn new(
        tcx: TyCtxt<'tcx>,
        top_module: HirItemId,
        crates: Crates,
        arena: &'tcx Bump,
    ) -> Self {
        Self {
            tcx,
            netlist: NetList::new(),
            settings: Settings::new(),
            arena,
            top_module,
            crates,
            blackbox: Default::default(),
            evaluated_modules: Default::default(),
            item_ty: Default::default(),
        }
    }

    pub fn generate(&mut self) {
        if let Err(e) = self.generate_inner() {
            self.emit_err(e);
        }
    }

    fn generate_inner(&mut self) -> Result<(), Error> {
        let crate_name = self.tcx.crate_name(LOCAL_CRATE);

        let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let name = "top_module";

        let path = StdPath::new(&dir).join("generated").join("verilog");
        fs::create_dir_all(&path)?;

        let mut path = path.join(name);
        path.set_extension("v");

        self.print_message(
            &"Synthesizing",
            Some(&format!(
                "{} into verilog {}",
                crate_name.as_str(),
                path.to_string_lossy()
            )),
        )?;

        self.visit_fn(
            DefId::from(self.top_module.hir_id().owner),
            GenericArgs::empty(),
            true,
        )?;

        self.netlist.assert();
        self.netlist.transform();
        self.netlist.reachability();
        self.netlist.set_names();
        self.netlist.inject_nodes();

        let verilog = Verilog::new(&self.netlist).generate();

        Ok(fs::write(path, verilog)?)
    }

    fn print_message(
        &self,
        status: &dyn Display,
        message: Option<&dyn Display>,
    ) -> io::Result<()> {
        // TODO: use Cargo settings for colors
        // Code from https://github.com/rust-lang/cargo/blob/2130a0faf0cb6aa44c5962c2f2d313fa7e459b2b/src/cargo/core/shell.rs#L451
        use std::io::Write;

        use anstream::AutoStream;
        use anstyle::{AnsiColor, Effects, Reset, Style};

        const HEADER: Style = AnsiColor::Green.on_default().effects(Effects::BOLD);

        let style = HEADER.render();
        let reset = Reset.render();

        let mut stream = AutoStream::always(std::io::stdout());
        write!(&mut stream, "{style}{status:>12}{reset}")?;
        if let Some(message) = message {
            writeln!(&mut stream, " {message}")?;
        }

        Ok(())
    }

    fn emit_err(&mut self, err: Error) {
        match err {
            Error::Span(SpanError { kind, span }) => {
                self.tcx.sess.span_err(span, kind.to_string());
            }
            _ => {
                self.tcx.sess.err(err.to_string());
            }
        };
        self.tcx.sess.abort_if_errors();
    }

    pub fn type_of(&self, def_id: DefId, generics: GenericArgsRef<'tcx>) -> Ty<'tcx> {
        self.tcx.type_of(def_id).instantiate(self.tcx, generics)
    }

    #[allow(dead_code)]
    pub fn set_mod_name(&mut self, mod_id: ModuleId, name: &str) {
        self.netlist[mod_id].name = Symbol::new(name);
    }

    pub fn trunc_or_extend(
        &mut self,
        mod_id: ModuleId,
        from: NodeOutId,
        from_ty: NodeTy,
        to_ty: NodeTy,
    ) -> NodeOutId {
        let from_width = from_ty.width();
        let to_width = to_ty.width();

        if from_width >= to_width {
            self.netlist.add_and_get_out(
                mod_id,
                Splitter::new(from, [(to_ty, None)], None, false),
            )
        } else {
            self.netlist
                .add_and_get_out(mod_id, ZeroExtend::new(to_ty, from, None))
        }
    }
}
