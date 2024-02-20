use clap::Subcommand;

use self::synth::SynthArgs;
use crate::Env;

mod synth;

#[derive(Subcommand)]
pub enum Commands {
    /// Synthesize a verilog file
    Synth(SynthArgs),
}

pub trait Run {
    fn run(&self, env: &Env) -> anyhow::Result<()>;
}

impl Commands {
    pub fn run(&self, env: &Env) -> anyhow::Result<()> {
        match self {
            Self::Synth(args) => args.run(env),
        }
    }
}
