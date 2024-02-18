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
    /// Space or comma separated list of features to activate
    #[arg(short = 'F', long, value_delimiter = ' ', value_delimiter = ',', num_args = 1..)]
    features: Option<Vec<String>>,
    /// Use verbose output (-vv very verbose/build.rs output)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
    #[command(flatten)]
    compiler_opts: CompilerArgs,
}

// TODO: move to rustc arguments
#[allow(non_upper_case_globals)]
const CARGO_dev: &[(&str, &str)] = &[
    ("CARGO_PROFILE_DEV_OPT_LEVEL", "0"),
    ("CARGO_PROFILE_DEV_DEBUG", "full"),
    ("CARGO_PROFILE_DEV_STRIP", "none"),
    ("CARGO_PROFILE_DEV_DEBUG_ASSERTIONS", "false"),
    ("CARGO_PROFILE_DEV_OVERFLOW_CHECKS", "false"),
    ("CARGO_PROFILE_DEV_LTO", "off"),
    ("CARGO_PROFILE_DEV_PANIC", "abort"),
    ("CARGO_PROFILE_DEV_INCREMENTAL", "true"),
];

impl GenArgs {
    fn set_cargo_profile(cmd: &mut Command) {
        for (key, val) in CARGO_dev {
            cmd.env(*key, *val);
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

        let fhdl_args = serde_json::to_string(&self.compiler_opts)?;

        let driver = env::current_exe()
            .map_err(|_| anyhow::anyhow!("current executable path invalid"))?
            .with_file_name(env.driver);

        let mut cmd = Command::new("xargo");
        cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
        cmd.env("RUSTC_WRAPPER", driver)
            .env("RUSTUP_TOOLCHAIN", &env.toolchain)
            .env("RUSTC_FLAGS", "-Z always-encode-mir=yes")
            .env("FHDL_ARGS", fhdl_args);

        // Override settings for dev profile
        Self::set_cargo_profile(&mut cmd);

        let features = self
            .features
            .as_ref()
            .filter(|f| !f.is_empty())
            .map(|f| f.join(","));

        let args = [
            "--lib",
            features.as_deref().unwrap_or_default(),
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

        cmd.arg("build")
            .args(args)
            .arg("--target")
            .arg("x86_64-unknown-linux-gnu")
            .arg("--target-dir")
            .arg(&target_dir);

        cmd.status()
            .map_err(|e| anyhow::anyhow!("failed to run xargo: {e}"))?;

        Ok(())
    }
}
