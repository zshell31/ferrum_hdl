use clap::Subcommand;

use self::gen::GenArgs;
use crate::Env;

mod gen;

#[derive(Subcommand)]
pub enum Commands {
    /// Generate a verilog file
    Gen(GenArgs),
}

pub trait Run {
    fn run(&self, env: &Env) -> anyhow::Result<()>;
}

impl Commands {
    pub fn run(&self, env: &Env) -> anyhow::Result<()> {
        match self {
            Self::Gen(args) => args.run(env),
        }
    }
}
