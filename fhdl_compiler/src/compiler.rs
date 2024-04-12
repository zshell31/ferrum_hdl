pub mod arena;
pub mod attr;
pub mod cons_;
pub mod context;
pub mod func;
pub mod item;
pub mod item_ty;
mod locals;
pub mod mir;
mod pins;
mod post_dominator;
pub mod switch;
mod switch_tuple;
mod sym_ident;
mod trie;
mod utils;

use std::{
    env,
    fmt::Display,
    fs, io, iter,
    mem::transmute,
    path::{Component, Path as StdPath, PathBuf},
    time::Instant,
};

use bumpalo::Bump;
pub use context::Context;
use fhdl_cli::CompilerArgs;
use fhdl_common::{BlackboxKind, NonEmptyStr};
use fhdl_data_structures::graph::Port;
use fhdl_netlist::{
    netlist::{Module, ModuleId, NetList},
    node::{Extend, ExtendArgs, Splitter, SplitterArgs},
    node_ty::NodeTy,
    symbol::Symbol,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{
    def_id::{DefId, LOCAL_CRATE},
    AssocItemKind, ItemKind,
};
use rustc_interface::{interface::Compiler as RustCompiler, Queries};
use rustc_middle::{
    dep_graph::DepContext,
    ty::{GenericArgs, GenericArgsRef, Ty, TyCtxt},
};
use rustc_span::{def_id::CrateNum, FileName, Span, StableSourceFileId};
pub use sym_ident::SymIdent;

use self::{
    item_ty::{ItemTy, ItemTyKind},
    mir::DefIdOrPromoted,
    pins::PinConstraints,
    post_dominator::PostDominator,
};
use crate::error::{Error, SpanError};

pub struct CompilerCallbacks {
    pub args: CompilerArgs,
}

impl Callbacks for CompilerCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &RustCompiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        let res = queries.global_ctxt().unwrap().enter(|tcx| {
            let arena = Bump::new();
            // SAFETY: the lifetime of the compiler is shorter than the lifetime of the
            // arena/options.
            let arena = unsafe { transmute::<&'_ Bump, &'tcx Bump>(&arena) };
            let args =
                unsafe { transmute::<&'_ CompilerArgs, &'tcx CompilerArgs>(&self.args) };

            match init_compiler(tcx, args, arena) {
                Ok(mut compiler) => {
                    compiler.generate();
                    true
                }
                Err(e) => {
                    tcx.sess.dcx().err(e.to_string());
                    false
                }
            }
        });

        if res {
            Compilation::Continue
        } else {
            Compilation::Stop
        }
    }
}

fn init_compiler<'tcx>(
    tcx: TyCtxt<'tcx>,
    args: &'tcx CompilerArgs,
    arena: &'tcx Bump,
) -> Result<Compiler<'tcx>, Error> {
    let crates = Crates::find_crates(tcx)?;

    Ok(Compiler::new(tcx, crates, args, arena))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonoItem<'tcx>(DefIdOrPromoted<'tcx>, GenericArgsRef<'tcx>);

impl<'tcx> MonoItem<'tcx> {
    pub fn new<T: Into<DefIdOrPromoted<'tcx>>>(
        item_id: T,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Self {
        Self(item_id.into(), generic_args)
    }
}

struct Crates {
    core: CrateNum,
    std: CrateNum,
    ferrum_hdl: CrateNum,
}

impl Crates {
    fn find_crates(tcx: TyCtxt<'_>) -> Result<Self, Error> {
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
        let ferrum_hdl = ferrum_hdl.ok_or_else(|| Error::MissingCrate(FERRUM_HDL))?;

        Ok(Self {
            core,
            std,
            ferrum_hdl,
        })
    }

    pub(crate) fn is_std(&self, def_id: DefId) -> bool {
        def_id.krate == self.core || def_id.krate == self.std
    }

    pub(crate) fn is_ferrum_hdl(&self, def_id: DefId) -> bool {
        def_id.krate == self.ferrum_hdl
    }
}

pub struct Compiler<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub netlist: NetList,
    pub args: &'tcx CompilerArgs,
    arena: &'tcx Bump,
    crates: Crates,
    blackbox: FxHashMap<DefId, Option<BlackboxKind>>,
    evaluated_modules: FxHashMap<MonoItem<'tcx>, ModuleId>,
    item_ty: FxHashMap<Ty<'tcx>, ItemTy<'tcx>>,
    allocated_ty: FxHashMap<ItemTyKind<'tcx>, ItemTy<'tcx>>,
    file_names: FxHashMap<StableSourceFileId, Option<PathBuf>>,
    pin_constr: FxHashMap<NonEmptyStr, PinConstraints>,
    post_dominator: FxHashMap<DefId, PostDominator>,
}

impl<'tcx> Compiler<'tcx> {
    fn new(
        tcx: TyCtxt<'tcx>,
        crates: Crates,
        args: &'tcx CompilerArgs,
        arena: &'tcx Bump,
    ) -> Self {
        Self {
            tcx,
            netlist: NetList::new(args.netlist.clone()),
            args,
            arena,
            crates,
            blackbox: Default::default(),
            evaluated_modules: Default::default(),
            item_ty: Default::default(),
            allocated_ty: Default::default(),
            file_names: Default::default(),
            pin_constr: Default::default(),
            post_dominator: Default::default(),
        }
    }

    pub fn generate(&mut self) {
        if let Err(e) = self.synth_inner() {
            self.emit_err(e);
        }
    }

    fn find_top_module(&self) -> Result<DefId, Error> {
        let hir = self.tcx.hir();
        for item_id in hir.items() {
            let item = hir.item(item_id);
            match item.kind {
                ItemKind::Fn(_, _, _) => {
                    let def_id = item_id.owner_id.to_def_id();
                    if item.ident.as_str() == "top" {
                        return Ok(def_id);
                    }
                    if let Some(synth) = self.find_synth(def_id) {
                        if synth.top {
                            return Ok(def_id);
                        }
                    }
                }
                ItemKind::Impl(impl_) => {
                    for impl_item in impl_.items {
                        let def_id = impl_item.id.owner_id.to_def_id();
                        if let AssocItemKind::Fn { .. } = impl_item.kind {
                            if impl_item.ident.as_str() == "top" {
                                return Ok(impl_item.id.owner_id.to_def_id());
                            }
                        }
                        if let Some(synth) = self.find_synth(def_id) {
                            if synth.top {
                                return Ok(def_id);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Err(Error::MissingTop)
    }

    fn synth_inner(&mut self) -> Result<(), Error> {
        let crate_name = self.tcx.crate_name(LOCAL_CRATE);

        let root_dir = &env::var("CARGO_MANIFEST_DIR").unwrap();
        let root_dir = StdPath::new(&root_dir);
        let name = "top";

        let synth_path = root_dir.join("synth").join("verilog");
        fs::create_dir_all(&synth_path)?;

        let mut path = synth_path.join(name);
        path.set_extension("v");

        self.print_message(
            &"Synthesizing",
            Some(&format!(
                "{} into verilog {}",
                crate_name.as_str(),
                path.to_string_lossy()
            )),
        )?;

        let elapsed = Instant::now();

        let top = self.find_top_module()?;
        let top = self.visit_fn(top.into(), GenericArgs::empty(), true)?;

        if self.args.dump_netlist {
            self.netlist.dump(false);
        }
        self.netlist.run_visitors();

        self.netlist.synth_verilog_into_file(path)?;

        self.print_message(
            &"Synthesized",
            Some(&format!("in {:.2}s", elapsed.elapsed().as_secs_f32())),
        )?;

        if !self.pin_constr.is_empty() {
            let constr_path = root_dir.join("constr");
            fs::create_dir_all(&constr_path)?;

            let top = self.netlist[top].borrow();
            self.write_pin_constraints(&top, constr_path)?;
        }

        Ok(())
    }

    pub fn print_message(
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
                self.tcx.sess.dcx().span_err(span, kind.to_string());
            }
            _ => {
                self.tcx.sess.dcx().err(err.to_string());
            }
        };
        self.tcx.sess.dcx().abort_if_errors();
    }

    pub fn type_of(&self, def_id: DefId, generics: GenericArgsRef<'tcx>) -> Ty<'tcx> {
        self.tcx.type_of(def_id).instantiate(self.tcx, generics)
    }

    pub fn trunc_or_extend(
        module: &mut Module,
        from: Port,
        from_ty: NodeTy,
        to_ty: NodeTy,
        sym: Option<Symbol>,
        is_sign: bool,
    ) -> Port {
        let from_width = from_ty.width();
        let to_width = to_ty.width();

        if from_width >= to_width {
            module.add_and_get_port::<_, Splitter>(SplitterArgs {
                input: from,
                outputs: iter::once((to_ty, sym)),
                start: None,
                rev: false,
            })
        } else {
            module.add_and_get_port::<_, Extend>(ExtendArgs {
                ty: to_ty,
                input: from,
                sym,
                is_sign,
            })
        }
    }

    pub fn span_to_string(&mut self, span: Span, fn_did: DefId) -> Option<String> {
        if self.crates.is_std(fn_did) {
            return None;
        }

        let sm = self.tcx.sess().source_map();

        let (source_file, lo_line, _, _, _) = sm.span_to_location_info(span);

        let source_file = source_file?;

        if let FileName::Real(file_name) = &source_file.name {
            let file_name = file_name.local_path_if_available();
            let file_name = if file_name.is_absolute() {
                self.file_names
                    .entry(source_file.stable_id)
                    .or_insert_with(|| {
                        let mut path = Vec::with_capacity(8);
                        let mut has_src_dir = false;
                        let mut parent_of_src = false;

                        for component in file_name.components().rev() {
                            if let Component::Normal(component) = component {
                                if has_src_dir {
                                    parent_of_src = true;
                                    path.push(component);
                                    break;
                                } else {
                                    if component == "src" {
                                        has_src_dir = true;
                                    }
                                    path.push(component);
                                }
                            }
                        }

                        if parent_of_src {
                            Some(path.into_iter().rev().collect())
                        } else {
                            None
                        }
                    })
                    .as_ref()?
                    .to_string_lossy()
            } else {
                file_name.to_string_lossy()
            };

            Some(format!("{file_name}: {lo_line}"))
        } else {
            None
        }
    }
}
