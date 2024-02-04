use std::{
    env,
    process::{Command, Stdio},
};

use clap::Args;

use super::Run;
use crate::{compiler::CompilerArgs, Env};

#[derive(Args)]
pub struct GenArgs {
    /// Build only this package's library
    #[arg(long)]
    lib: bool,
    #[command(flatten)]
    compiler_opts: CompilerArgs,
}

impl Run for GenArgs {
    fn run(&self, env: &Env) -> anyhow::Result<()> {
        env::set_var("RUSTUP_TOOLCHAIN", &env.toolchain);
        env::set_var("FHDL_ARGS", serde_json::to_string(&self.compiler_opts)?);

        let driver = env::current_exe()
            .map_err(|_| anyhow::anyhow!("current executable path invalid"))?
            .with_file_name(env.driver);

        let args = [if self.lib { "--lib" } else { "" }];

        let mut cmd = Command::new(&env.cargo);
        cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
        cmd.env("RUSTC_WRAPPER", driver).arg("build").args(args);

        cmd.status()
            .map_err(|_| anyhow::anyhow!("failed to run cargo"))?;

        Ok(())
    }
}
