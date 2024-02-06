use std::{env, path::PathBuf};

macro_rules! path_str {
    ($input:expr) => {
        String::from(
            $input
                .iter()
                .collect::<PathBuf>()
                .to_str()
                .unwrap_or_else(|| panic!("Invalid path {}", stringify!($input))),
        )
    };
}

fn find_toolchain() -> Option<(String, String)> {
    let home = env::var("RUSTUP_HOME").ok()?;
    let toolchain = env::var("RUSTUP_TOOLCHAIN").ok()?;

    Some((home, toolchain))
}

pub fn main() {
    if let Some((home, toolchain)) = find_toolchain() {
        let lib = path_str!([&home, "toolchains", &toolchain, "lib"]);
        println!("cargo:rustc-link-arg-bin=fhdl-driver=-Wl,-rpath,{lib}");
        println!("cargo:rustc-env=FHDL_TOOLCHAIN={toolchain}");
    }
}
