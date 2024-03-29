use clap::{Args, ValueEnum};
use serde::{Deserialize, Serialize};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize,
)]
pub enum InlineMod {
    All,
    #[default]
    Auto,
    None,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize, Args)]
pub struct NetListCfg {
    /// Inline modules
    #[arg(long, value_enum, default_value_t = InlineMod::Auto)]
    pub inline_mod: InlineMod,
    /// Do not embed nested multiplexers
    #[arg(long)]
    pub no_embed_muxs: bool,
}
