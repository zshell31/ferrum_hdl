#![feature(rustc_private)]
#![feature(iter_intersperse)]
#![feature(min_specialization)]
#![feature(type_alias_impl_trait)]

mod blackbox;
mod error;
mod eval_context;
mod generator;
mod scopes;
mod utils;

use std::{
    env,
    ops::Deref,
    path::{Path, PathBuf},
    process::{exit, Command},
};

use rustc_driver::Callbacks;
use rustc_session::{config::ErrorOutputType, EarlyDiagCtxt};

use crate::generator::CompilerCallbacks;

extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_hir_analysis;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

fn init_session() {
    let ctx = EarlyDiagCtxt::new(ErrorOutputType::default());
    rustc_driver::init_rustc_env_logger(&ctx);
}

fn arg_value<'a, T: Deref<Target = str>>(
    args: &'a [T],
    find_arg: &str,
    pred: impl Fn(&str) -> bool,
) -> Option<&'a str> {
    let mut args = args.iter().map(Deref::deref);
    while let Some(arg) = args.next() {
        let mut arg = arg.splitn(2, '=');
        if arg.next() != Some(find_arg) {
            continue;
        }

        match arg.next().or_else(|| args.next()) {
            Some(v) if pred(v) => return Some(v),
            _ => {}
        }
    }
    None
}

fn toolchain_path(home: Option<String>, toolchain: Option<String>) -> Option<PathBuf> {
    home.and_then(|home| {
        toolchain.map(|toolchain| {
            let mut path = PathBuf::from(home);
            path.push("toolchains");
            path.push(toolchain);
            path
        })
    })
}

fn get_sysroot(orig_args: &[String]) -> (bool, String) {
    // Get the sysroot, looking from most specific to this invocation to the least:
    // - command line
    // - runtime environment
    //    - SYSROOT
    //    - RUSTUP_HOME, MULTIRUST_HOME, RUSTUP_TOOLCHAIN, MULTIRUST_TOOLCHAIN
    // - sysroot from rustc in the path
    // - compile-time environment
    //    - SYSROOT
    //    - RUSTUP_HOME, MULTIRUST_HOME, RUSTUP_TOOLCHAIN, MULTIRUST_TOOLCHAIN
    let sys_root_arg = arg_value(orig_args, "--sysroot", |_| true);
    let have_sys_root_arg = sys_root_arg.is_some();
    let sys_root = sys_root_arg
    .map(PathBuf::from)
    .or_else(|| std::env::var("MIRI_SYSROOT").ok().map(PathBuf::from))
    .or_else(|| std::env::var("SYSROOT").ok().map(PathBuf::from))
    .or_else(|| {
      let home = std::env::var("RUSTUP_HOME")
        .or_else(|_| std::env::var("MULTIRUST_HOME"))
        .ok();
      let toolchain = std::env::var("RUSTUP_TOOLCHAIN")
        .or_else(|_| std::env::var("MULTIRUST_TOOLCHAIN"))
        .ok();
      toolchain_path(home, toolchain)
    })
    .or_else(|| {
      Command::new("rustc")
        .arg("--print")
        .arg("sysroot")
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .map(|s| PathBuf::from(s.trim()))
    })
    .or_else(|| option_env!("SYSROOT").map(PathBuf::from))
    .or_else(|| {
      let home = option_env!("RUSTUP_HOME")
        .or(option_env!("MULTIRUST_HOME"))
        .map(ToString::to_string);
      let toolchain = option_env!("RUSTUP_TOOLCHAIN")
        .or(option_env!("MULTIRUST_TOOLCHAIN"))
        .map(ToString::to_string);
      toolchain_path(home, toolchain)
    })
    .map(|pb| pb.to_string_lossy().to_string())
    .expect(
      "need to specify SYSROOT env var during clippy compilation, or use rustup or multirust",
    );
    (have_sys_root_arg, sys_root)
}
struct DefaultCallbacks;

impl Callbacks for DefaultCallbacks {}

fn main() {
    init_session();

    exit(rustc_driver::catch_with_exit_code(|| {
        let mut args = env::args().collect::<Vec<_>>();

        let (have_sys_root_arg, sys_root) = get_sysroot(&args);

        let wrapper_mode = args.get(1).map(Path::new).and_then(Path::file_stem)
            == Some("rustc".as_ref());
        if wrapper_mode {
            args.remove(1);
        }

        if !have_sys_root_arg {
            args.extend(["--sysroot".into(), sys_root]);
        };

        let normal_rustc = arg_value(&args, "--print", |_| true).is_some();
        let is_target_crate =
            match (env::var("SPECIFIC_CRATE"), env::var("SPECIFIC_TARGET")) {
                (Ok(krate), Ok(target)) => {
                    arg_value(&args, "--crate-name", |name| name == krate).is_some()
                        && arg_value(&args, "--crate-type", |name| name == target)
                            .is_some()
                }
                _ => true,
            };
        let run_fhdl = !normal_rustc && is_target_crate;

        if run_fhdl {
            let mut callbacks = CompilerCallbacks {};
            let compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
            compiler.run()
        } else {
            rustc_driver::RunCompiler::new(&args, &mut DefaultCallbacks).run()
        }
    }));
}
