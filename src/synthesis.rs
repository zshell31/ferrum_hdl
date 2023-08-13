use std::cmp::PartialEq;

use crate::{
    ast::{Expression, Ident, Parameter, ParameterKind},
    domain::DummySystem,
    traits::Synthesizable,
};
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Lit,
};

#[derive(Debug)]
pub struct SynthTyPath(&'static [&'static str]);

// TODO: maybe it's better to use some hash values for paths
impl PartialEq<SynthTyPath> for DefPath {
    fn eq(&self, other: &SynthTyPath) -> bool {
        if self.data.len() != other.0.len() {
            false
        } else {
            self.data
                .iter()
                .zip(other.0.iter())
                .all(|(def_comp, synth_comp)| match def_comp.data.name() {
                    DefPathDataName::Named(name) => name.as_str() == *synth_comp,
                    DefPathDataName::Anon { .. } => false,
                })
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SynthTyVTable {
    width: fn() -> u128,
    synthesize_lit: fn(&Lit) -> Option<Expression>,
}

impl SynthTyVTable {
    fn new<T: Synthesizable>() -> Self {
        Self {
            width: T::width,
            synthesize_lit: T::synthesize_lit,
        }
    }

    fn width(&self) -> u128 {
        (self.width)()
    }

    pub fn synthesize_lit(&self, lit: &Lit) -> Option<Expression> {
        (self.synthesize_lit)(lit)
    }

    pub fn synthesize_param(&self, kind: ParameterKind, ident: Ident) -> Parameter {
        Parameter {
            kind,
            ident,
            width: self.width(),
        }
    }
}

pub fn vtable(def_path: &DefPath, args: &[DefPath]) -> Option<SynthTyVTable> {
    if def_path == &SynthTyPath(&["bit", "Bit"]) {
        Some(SynthTyVTable::new::<crate::bit::Bit>())
    } else if def_path == &SynthTyPath(&["signal", "Clock"]) {
        Some(SynthTyVTable::new::<crate::signal::Clock<DummySystem>>())
    } else if def_path == &SynthTyPath(&["signal", "Register"]) {
        if args[1] == SynthTyPath(&["bit", "Bit"]) {
            Some(SynthTyVTable::new::<
                crate::signal::Register<DummySystem, crate::bit::Bit>,
            >())
        } else {
            None
        }
    } else if def_path == &SynthTyPath(&["signal", "register"]) {
        if args[1] == SynthTyPath(&["bit", "Bit"]) {
            Some(SynthTyVTable::new::<
                crate::signal::RegisterFn<DummySystem, crate::bit::Bit>,
            >())
        } else {
            None
        }
    } else {
        None
    }
}
