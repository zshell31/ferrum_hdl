use std::io::{self, IsTerminal};

use clap::{Args, ValueEnum};
use serde::{Deserialize, Serialize};

#[derive(Debug, Args, Serialize, Deserialize)]
pub struct CompilerArgs {
    /// Use colors
    #[arg(long, value_enum, default_value_t = Color::Always)]
    pub color: Color,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize)]
pub enum Color {
    Never,
    Auto,
    Always,
}

impl Color {
    pub fn use_colors(&self) -> bool {
        match self {
            Self::Never => false,
            Self::Auto => io::stdout().is_terminal(),
            Self::Always => true,
        }
    }
}