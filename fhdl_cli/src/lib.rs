use std::env;

use clap::{Args, Parser};
use commands::Commands;

mod commands;
mod compiler;
mod styles;

pub use compiler::{Color, CompilerArgs};

pub struct Env {
    driver: &'static str,
    toolchain: String,
    cargo: String,
}

#[derive(Parser)]
#[command(name = "cargo")]
#[command(bin_name = "cargo")]
#[command(version, about, long_about = None)]
#[command(styles = styles::get_styles())]
enum CargoCli {
    Fhdl(FhdlCli),
}

#[derive(Args)]
#[command(version, about, long_about = None)]
#[command(styles = styles::get_styles())]
struct FhdlCli {
    #[command(subcommand)]
    command: Commands,
}

pub fn run_cli(driver: &'static str, toolchain: String) -> anyhow::Result<()> {
    let env = Env {
        driver,
        toolchain,
        cargo: env::var("CARGO").unwrap_or("cargo".into()),
    };

    let CargoCli::Fhdl(args) = CargoCli::parse();

    args.command.run(&env)
}
