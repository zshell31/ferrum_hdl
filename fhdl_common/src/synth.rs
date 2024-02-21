use std::collections::HashMap;

use darling::FromMeta;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;

use crate::utils::{NonEmptyAsciiStr, NonEmptyStr};

#[derive(Debug, Clone, Copy, FromMeta, Serialize, Deserialize)]
pub enum Vendor {
    Xilinx,
}

#[derive(Debug, Clone, FromMeta, Serialize, Deserialize)]
pub struct Pin {
    pub name: NonEmptyAsciiStr,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Constraint {
    pub vendor: Vendor,
    pub name: NonEmptyStr,
    pub pins: HashMap<String, SmallVec<[Pin; 1]>>,
}

impl Constraint {
    pub fn pins_count(&self) -> usize {
        self.pins.values().map(|pins| pins.len()).sum()
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct SynthAttrs {
    pub inline: bool,
    pub constr: SmallVec<[Constraint; 1]>,
}

impl SynthAttrs {
    pub fn only_inline(&self) -> bool {
        self.constr.is_empty()
    }
}
