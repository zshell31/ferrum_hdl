use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use fhdl_common::{Constraint, Pin, Vendor};
use fhdl_netlist::netlist::{Module, PortPos};
use rustc_data_structures::fx::{FxHashMap, FxIndexMap};
use rustc_middle::mir::{Body, Local, VarDebugInfoContents};

use super::{Compiler, Context};
use crate::error::Error;

pub struct Idents(FxHashMap<String, Local>);

impl Idents {
    pub fn from_debug_info(mir: &Body<'_>) -> Self {
        Self(
            mir.var_debug_info
                .iter()
                .filter_map(|var_debug_info| {
                    let name = var_debug_info.name;
                    match var_debug_info.value {
                        VarDebugInfoContents::Place(place) => {
                            Some((name.to_string(), place.local))
                        }
                        VarDebugInfoContents::Const(_) => None,
                    }
                })
                .collect(),
        )
    }
}

#[derive(Debug)]
pub struct PinConstraints {
    pub vendor: Vendor,
    pub pins: FxIndexMap<PortPos, Pin>,
}

impl<'tcx> Compiler<'tcx> {
    pub fn add_pin_constraints(
        &mut self,
        constr: &[Constraint],
        idents: &Idents,
        inputs_and_outputs: usize,
        ctx: &Context<'tcx>,
    ) -> Result<(), Error> {
        for constr in constr {
            self.add_pin_constraint(constr, idents, inputs_and_outputs, ctx)?;
        }

        Ok(())
    }

    fn add_pin_constraint(
        &mut self,
        constr: &Constraint,
        idents: &Idents,
        mut inputs_and_outputs: usize,
        ctx: &Context<'tcx>,
    ) -> Result<(), Error> {
        let constr_name = &constr.name.0;
        let mut pins_map: FxIndexMap<PortPos, Pin> = Default::default();
        let total = inputs_and_outputs;
        let total_pins = constr.pins_count();

        for (ident, pins) in &constr.pins {
            let item = idents
                .0
                .get(ident.as_str())
                .map(|local| ctx.locals.get(*local))
                .ok_or_else(|| Error::MissingIdent {
                    constr: constr_name.clone(),
                    ident: ident.clone(),
                })?;

            let pins_count = pins.len();

            let mut ports = item.iter();
            let mut pins = pins.iter();
            loop {
                let port = ports.next();
                let pin = pins.next();

                match (port, pin) {
                    (Some(port), Some(pin)) => {
                        if let Some(port_pos) = ctx.module.port_pos(port) {
                            let res = inputs_and_outputs.overflowing_sub(1);
                            if res.1 {
                                return Err(Error::IncorrectTotalPinsCount {
                                    constr: constr_name.clone(),
                                    expected: total,
                                    actual: total_pins,
                                });
                            }
                            inputs_and_outputs = res.0;

                            pins_map.insert(port_pos, pin.clone());
                        } else {
                            return Err(Error::NotInputOutputIdent {
                                constr: constr_name.clone(),
                                ident: ident.to_string(),
                            });
                        }
                    }
                    (None, None) => {
                        break;
                    }
                    _ => {
                        return Err(Error::IncorrectPinsCount {
                            constr: constr_name.clone(),
                            ident: ident.to_string(),
                            expected: item.nodes(),
                            actual: pins_count,
                        });
                    }
                }
            }
        }

        if inputs_and_outputs > 0 {
            return Err(Error::IncorrectTotalPinsCount {
                constr: constr_name.clone(),
                expected: total,
                actual: total_pins,
            });
        }

        let pin_constr = PinConstraints {
            vendor: constr.vendor,
            pins: pins_map,
        };

        if self.pin_constr.contains_key(&constr.name) {
            return Err(Error::DuplicateConstrName(constr.name.0.clone()));
        }

        self.pin_constr.insert(constr.name.clone(), pin_constr);

        Ok(())
    }

    pub fn write_pin_constraints(
        &self,
        top_mod: &Module,
        dir_path: impl AsRef<Path>,
    ) -> Result<(), Error> {
        for (name, pin_constr) in &self.pin_constr {
            self.write_pin_constraint(
                top_mod,
                pin_constr,
                dir_path.as_ref(),
                name.as_str(),
            )?;
        }

        Ok(())
    }

    fn write_pin_constraint(
        &self,
        top_mod: &Module,
        pin_constr: &PinConstraints,
        path: impl AsRef<Path>,
        name: &str,
    ) -> Result<(), Error> {
        let mut path = path.as_ref().join(name);

        match pin_constr.vendor {
            Vendor::Xilinx => {
                self.write_xilinx_constraint(top_mod, &mut path, &pin_constr.pins)?;
            }
        }

        self.print_message(
            &"Writing",
            Some(&format!("pin constraints into {}", path.to_string_lossy())),
        )?;

        Ok(())
    }

    fn write_xilinx_constraint(
        &self,
        top_mod: &Module,
        path: &mut PathBuf,
        pins: &FxIndexMap<PortPos, Pin>,
    ) -> Result<(), Error> {
        path.set_extension("xdc");

        let mut buffer = String::new();
        writeln!(buffer, "# Automatically generated by Ferrum HDL.\n").unwrap();
        for (port_pos, pin) in pins {
            let port = top_mod.get_port_by_pos(*port_pos);
            let sym = top_mod[port].sym.unwrap();
            let pin = &pin.name;

            writeln!(
                buffer,
                r#"set_property PACKAGE_PIN {pin} [get_ports {{{sym}}}]"#
            )
            .unwrap();
            writeln!(
                buffer,
                "set_property IOSTANDARD LVCMOS33 [get_ports {{{sym}}}]\n"
            )
            .unwrap();
        }

        fs::write(path, buffer)?;

        Ok(())
    }
}
