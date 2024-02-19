use clap::{Args, ValueEnum};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize)]
pub enum InlineMod {
    All,
    Auto,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize, Args)]
pub struct NetListCfg {
    /// Inline modules
    #[arg(long, value_enum, default_value_t = InlineMod::Auto)]
    pub inline_mod: InlineMod,
}
