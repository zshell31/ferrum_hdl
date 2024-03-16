use std::{
    fmt::Write as _,
    fs::File,
    io::{self, BufWriter, Write},
    path::Path,
};

use derive_where::derive_where;
pub use fhdl_macros::Traceable;
use rustc_hash::FxHashMap;
pub use vcd::{IdCode, Value as TraceValue};
use vcd::{TimescaleUnit, VarType, Writer as VcdWriter};

pub trait Traceable {
    fn add_vars(vars: &mut TraceVars);

    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()>;
}

impl<'a, T: Traceable> Traceable for &'a T {
    #[inline]
    fn add_vars(vars: &mut TraceVars) {
        T::add_vars(vars);
    }

    #[inline]
    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        (*self).trace(id, tracer)
    }
}

pub(crate) fn bool_to_vcd(b: bool) -> vcd::Value {
    if b {
        vcd::Value::V1
    } else {
        vcd::Value::V0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TraceTy {
    Wire,
    Bus(u32),
}

impl TraceTy {
    fn as_pair(&self) -> (u32, VarType) {
        match self {
            Self::Wire => (1, VarType::Wire),
            Self::Bus(w) => (*w, VarType::Integer),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Command {
    PushSym(&'static str),
    PushIdx(usize),
    Ty(TraceTy),
    Pop,
}

#[derive(Debug, Default)]
pub struct TraceVars {
    commands: Vec<Command>,
}

impl TraceVars {
    pub fn add_var<T: Traceable>(mut self, sym: &'static str, _var: &T) -> Self {
        self.commands.push(Command::PushSym(sym));
        T::add_vars(&mut self);
        self.commands.push(Command::Pop);

        self
    }

    #[inline]
    pub fn push_sym(&mut self, sym: &'static str) {
        self.commands.push(Command::PushSym(sym));
    }

    #[inline]
    pub fn push_idx(&mut self, idx: usize) {
        self.commands.push(Command::PushIdx(idx));
    }

    #[inline]
    pub fn add_ty(&mut self, ty: TraceTy) {
        self.commands.push(Command::Ty(ty));
    }

    #[inline]
    pub fn pop(&mut self) {
        self.commands.push(Command::Pop);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Timescale {
    S(u32),
    MS(u32),
    US(u32),
    NS(u32),
    PS(u32),
    FS(u32),
}

impl Timescale {
    fn into_pair(self) -> (u32, TimescaleUnit) {
        match self {
            Self::S(ts) => (ts, TimescaleUnit::S),
            Self::MS(ts) => (ts, TimescaleUnit::MS),
            Self::US(ts) => (ts, TimescaleUnit::US),
            Self::NS(ts) => (ts, TimescaleUnit::NS),
            Self::PS(ts) => (ts, TimescaleUnit::PS),
            Self::FS(ts) => (ts, TimescaleUnit::FS),
        }
    }
}

impl Default for Timescale {
    fn default() -> Self {
        Self::PS(1)
    }
}

#[derive(Debug)]
struct VarName {
    stack: Vec<usize>,
    inner: String,
}

impl Default for VarName {
    fn default() -> Self {
        Self {
            stack: Vec::with_capacity(8),
            inner: String::with_capacity(32),
        }
    }
}

impl VarName {
    #[inline]
    fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    #[inline]
    fn to_str(&self) -> String {
        self.inner.clone()
    }

    fn push_sym(&mut self, sym: &'static str) {
        self.stack.push(self.inner.len());
        if !self.inner.is_empty() {
            self.inner.push('$');
        }
        self.inner.push_str(sym);
    }

    fn push_idx(&mut self, idx: usize) {
        self.stack.push(self.inner.len());
        if !self.inner.is_empty() {
            self.inner.push('$');
        }
        write!(&mut self.inner, "{idx}").unwrap();
    }

    fn pop(&mut self) {
        if let Some(idx) = self.stack.pop() {
            self.inner.truncate(idx);
        }
    }
}

#[derive_where(Debug)]
pub struct Tracer {
    syms: FxHashMap<String, IdCode>,
    mod_name: &'static str,
    timescale: Timescale,
    #[derive_where(skip)]
    vcd: VcdWriter<Box<dyn Write>>,
}

impl Tracer {
    pub fn open_vcd<P: AsRef<Path>>(
        path: P,
        vars: TraceVars,
        mod_name: &'static str,
        timescale: Option<Timescale>,
    ) -> io::Result<Self> {
        let mut vcd = VcdWriter::new(
            Box::new(BufWriter::new(File::create(path)?)) as Box<dyn Write>
        );
        let timescale = timescale.unwrap_or_default();
        let (ts, unit) = timescale.into_pair();
        vcd.timescale(ts, unit)?;
        vcd.add_module(mod_name)?;

        let mut syms: FxHashMap<String, IdCode> = Default::default();
        let mut var_name = VarName::default();
        let mut sym = None;
        let mut code = None;
        for command in vars.commands {
            match command {
                Command::PushSym(sym_) => {
                    sym.get_or_insert(sym_);
                    var_name.push_sym(sym_);
                }
                Command::PushIdx(idx) => {
                    var_name.push_idx(idx);
                }
                Command::Ty(ty) => {
                    let (width, var_ty) = ty.as_pair();
                    code.get_or_insert(vcd.add_var(
                        var_ty,
                        width,
                        var_name.as_str(),
                        None,
                    )?);
                }
                Command::Pop => {
                    if Some(var_name.as_str()) == sym {
                        if let Some(start_code) = code {
                            syms.insert(var_name.to_str(), start_code);
                        }

                        sym = None;
                        code = None;
                    }
                    var_name.pop();
                }
            }
        }
        vcd.upscope()?;
        vcd.enddefinitions()?;

        Ok(Self {
            syms,
            mod_name,
            timescale,
            vcd,
        })
    }

    #[inline]
    pub fn mod_name(&self) -> &'static str {
        self.mod_name
    }

    #[inline]
    pub fn timescale(&self) -> Timescale {
        self.timescale
    }

    pub fn dump_time(&mut self, time: u64) -> io::Result<()> {
        self.vcd.timestamp(time)
    }

    pub fn trace<T: Traceable>(
        &mut self,
        sym: &'static str,
        value: &T,
    ) -> io::Result<()> {
        if let Some(id) = self.syms.get(sym) {
            let mut id = *id;
            value.trace(&mut id, self)?;
        }

        Ok(())
    }

    #[inline]
    pub fn change_wire(&mut self, id: &mut IdCode, value: TraceValue) -> io::Result<()> {
        self.vcd.change_scalar(*id, value)?;
        *id = id.next();

        Ok(())
    }

    #[inline]
    pub fn change_bus(
        &mut self,
        id: &mut IdCode,
        values: impl IntoIterator<Item = TraceValue>,
    ) -> io::Result<()> {
        self.vcd.change_vector(*id, values)?;
        *id = id.next();

        Ok(())
    }

    #[inline]
    pub fn flush(&mut self) -> io::Result<()> {
        self.vcd.flush()
    }
}
