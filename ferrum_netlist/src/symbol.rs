use std::{
    fmt::{self, Debug, Display},
    sync::{Mutex, OnceLock},
};

#[derive(Debug, Default)]
struct SymbolInterner {
    arena: String,
}

impl SymbolInterner {
    pub fn sym(&mut self, name: &str) -> Symbol {
        let sym = Symbol(self.arena.len(), name.len());
        self.arena.push_str(name);
        sym
    }

    pub fn as_str(&self, sym: &Symbol) -> &str {
        if sym.is_empty() {
            ""
        } else {
            &self.arena[sym.begin() ..= sym.end()]
        }
    }
}

static SYMBOLS: OnceLock<Mutex<SymbolInterner>> = OnceLock::new();

fn with_symbols<T>(mut f: impl FnMut(&mut SymbolInterner) -> T) -> T {
    let interner = SYMBOLS.get_or_init(|| Mutex::new(SymbolInterner::default()));

    let mut interner = interner.lock().unwrap();
    f(&mut interner)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize, usize);

impl Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({})", self)
    }
}

impl Symbol {
    pub const E: Symbol = Symbol(0, 0);

    pub fn new(name: &str) -> Self {
        // TODO: check duplicates
        with_symbols(|interner| interner.sym(name))
    }

    pub fn is_empty(&self) -> bool {
        self.1 == 0
    }

    pub fn begin(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.0 + self.1 - 1
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        with_symbols(|interner| {
            let s = interner.as_str(self);
            f.write_str(s)
        })
    }
}
