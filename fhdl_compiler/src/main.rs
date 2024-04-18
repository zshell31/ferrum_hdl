use std::env;

fn main() -> anyhow::Result<()> {
    fhdl_cli::run_cli("fhdl-driver", env!("FHDL_TOOLCHAIN").to_string())
}
