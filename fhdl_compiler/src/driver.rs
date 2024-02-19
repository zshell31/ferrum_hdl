#![feature(rustc_private)]
#![feature(iter_intersperse)]
#![feature(min_specialization)]
#![feature(type_alias_impl_trait)]
#![feature(let_chains)]

mod blackbox;
mod compiler;
mod error;

use std::{
    env, io,
    ops::Deref,
    path::{Path, PathBuf},
    process::{exit, Command},
};

use fhdl_cli::CompilerArgs;
use rustc_driver::Callbacks;
use rustc_session::{config::ErrorOutputType, EarlyDiagCtxt};
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

use crate::compiler::CompilerCallbacks;

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
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

const LOG_ENV_VAR: &str = "FHDL_LOG";

fn init_logger(args: &CompilerArgs) {
    let filter = EnvFilter::from_env(LOG_ENV_VAR);
    let use_colors = args.color.use_colors();
    let subscriber = Registry::default().with(filter);
    let subscriber = subscriber.with(
        tracing_subscriber::fmt::layer()
            .with_writer(io::stderr)
            .with_ansi(use_colors)
            .with_target(true),
    );

    tracing::subscriber::set_global_default(subscriber).unwrap();
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

fn get_sysroot(orig_args: &[String]) -> (bool, String) {
    let sys_root_arg = arg_value(orig_args, "--sysroot", |_| true);
    let have_sys_root_arg = sys_root_arg.is_some();
    let sys_root = sys_root_arg
        .map(PathBuf::from)
        .or_else(|| env::var("SYSROOT").ok().map(PathBuf::from))
        .or_else(|| {
            let home = env::var("RUSTUP_HOME").ok()?;
            let toolchain = env::var("RUSTUP_TOOLCHAIN").ok()?;

            let mut path = PathBuf::from(home);
            path.push("toolchains");
            path.push(toolchain);

            Some(path)
        })
        .or_else(|| {
            Command::new("rustc")
                .args(["--print", "sysroot"])
                .output()
                .ok()
                .and_then(|out| String::from_utf8(out.stdout).ok())
                .map(|s| PathBuf::from(s.trim()))
        })
        .map(|pb| pb.to_string_lossy().to_string())
        .expect("need to specify SYSROOT env var during compilation or use rustup");

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
        let is_primary = env::var("CARGO_PRIMARY_PACKAGE").is_ok();
        let run_fhdl = !normal_rustc && is_primary && is_target_crate;

        if run_fhdl {
            env::set_var("RUST_BACKTRACE", "full");

            let fhdl_args = serde_json::from_str(
                &env::var("FHDL_ARGS").expect("FHDL_ARGS is not set"),
            )
            .expect("Failed to parse FHDL_ARGS");
            init_logger(&fhdl_args);

            let mut callbacks = CompilerCallbacks { args: fhdl_args };
            let compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
            compiler.run()
        } else {
            rustc_driver::RunCompiler::new(&args, &mut DefaultCallbacks).run()
        }
    }));
}
