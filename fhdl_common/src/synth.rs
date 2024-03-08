use std::rc::Rc;

use darling::FromMeta;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;

use crate::utils::{NonEmptyAsciiStr, NonEmptyStr};

#[derive(Debug, Clone, Copy, FromMeta, Serialize, Deserialize)]
pub enum Vendor {
    Xilinx,
}

#[derive(Debug, Clone, FromMeta, Serialize, Deserialize)]
pub struct Pin {
    pub name: Rc<NonEmptyAsciiStr>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Constraint {
    pub vendor: Vendor,
    pub name: NonEmptyStr,
    pub pins: IndexMap<String, SmallVec<[Pin; 1]>>,
}

impl Constraint {
    pub fn pins_count(&self) -> usize {
        self.pins.values().map(|pins| pins.len()).sum()
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct SynthAttrs {
    pub top: bool,
    pub inline: bool,
    pub constr: SmallVec<[Constraint; 1]>,
}

impl SynthAttrs {
    pub fn only_inline(&self) -> bool {
        !self.top && self.constr.is_empty()
    }
}
