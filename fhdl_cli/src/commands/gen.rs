use std::{
    env,
    process::{Command, Stdio},
};

use cargo_metadata::MetadataCommand;
use clap::Args;

use super::Run;
use crate::{compiler::CompilerArgs, Env};

#[derive(Debug, Args)]
pub struct GenArgs {
    /// Build only this package's library
    #[arg(long)]
    lib: bool,
    /// Use verbose output (-vv very verbose/build.rs output)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
    #[command(flatten)]
    compiler_opts: CompilerArgs,
}

#[allow(non_upper_case_globals)]
const CARGO_dev: &[(&str, &str)] = &[
    ("CARGO_PROFILE_dev_OPT_LEVEL", "0"),
    ("CARGO_PROFILE_dev_DEBUG", "full"),
    ("CARGO_PROFILE_dev_STRIP", "none"),
    ("CARGO_PROFILE_dev_DEBUG_ASSERTIONS", "false"),
    ("CARGO_PROFILE_dev_OVERFLOW_CHECKS", "false"),
    ("CARGO_PROFILE_dev_LTO", "off"),
    ("CARGO_PROFILE_dev_PANIC", "abort"),
    ("CARGO_PROFILE_dev_INCREMENTAL", "true"),
];

impl GenArgs {
    fn set_cargo_profile() {
        for (key, val) in CARGO_dev {
            env::set_var(*key, *val);
        }
    }
}

impl Run for GenArgs {
    fn run(&self, env: &Env) -> anyhow::Result<()> {
        let metadata = MetadataCommand::new()
            .no_deps()
            .other_options(["--all-features".to_string(), "--offline".to_string()])
            .exec()
            .map_err(|e| anyhow::anyhow!("Failed to get cargo metadata: {e}"))?;

        let target_dir = metadata
            .target_directory
            .join(format!("fhdl-{}", &env.toolchain));

        env::set_var("RUSTUP_TOOLCHAIN", &env.toolchain);
        env::set_var("FHDL_ARGS", serde_json::to_string(&self.compiler_opts)?);

        // Override settings for dev profile
        Self::set_cargo_profile();

        let driver = env::current_exe()
            .map_err(|_| anyhow::anyhow!("current executable path invalid"))?
            .with_file_name(env.driver);

        let args = [
            if self.lib { "--lib" } else { "" },
            match self.verbose {
                0 => "",
                1 => "-v",
                _ => "-vv",
            },
            "--profile",
            "dev",
        ]
        .into_iter()
        .filter(|arg| !arg.is_empty());

        let mut cmd = Command::new(&env.cargo);
        cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
        cmd.env("RUSTC_WRAPPER", driver)
            .arg("build")
            .args(args)
            .arg("--target-dir")
            .arg(&target_dir);

        cmd.status()
            .map_err(|_| anyhow::anyhow!("failed to run cargo"))?;

        Ok(())
    }
}
