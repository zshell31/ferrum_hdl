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

pub fn main() {
    let home = env::var("RUSTUP_HOME").unwrap();
    let toolchain = env::var("RUSTUP_TOOLCHAIN").unwrap();
    let lib = path_str!([&home, "toolchains", &toolchain, "lib"]);

    println!("cargo:rustc-link-arg-bin=fhdl-driver=-Wl,-rpath,{lib}");
    println!("cargo:rustc-env=FHDL_TOOLCHAIN={toolchain}");
}
