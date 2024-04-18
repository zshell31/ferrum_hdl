use std::{
    fmt::{self, Arguments, Debug, Display},
    hash::BuildHasherDefault,
    mem,
    str::pattern::{Pattern, Searcher},
};

use fhdl_data_structures::{idx_ty, index::IndexType, FxHashSet, FxHasher};
use lasso::{Capacity, Key, ThreadedRodeo};
use once_cell::sync::Lazy;

idx_ty!(Symbol, true);

static DEFAULT_SYMBOLS: Lazy<FxHashSet<&'static str>> = Lazy::new(|| {
    ["input", "output", "reg", "self", "module"]
        .into_iter()
        .collect()
});

static INTERNER: Lazy<ThreadedRodeo<Symbol, BuildHasherDefault<FxHasher>>> =
    Lazy::new(|| {
        ThreadedRodeo::with_capacity_and_hasher(
            Capacity::for_strings(32),
            Default::default(),
        )
    });

unsafe impl Key for Symbol {
    #[inline]
    fn into_usize(self) -> usize {
        self.as_usize()
    }

    #[inline]
    fn try_from_usize(int: usize) -> Option<Self> {
        IndexType::try_from_usize(int)
    }
}

impl Debug for Symbol {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Symbol {
    #[inline]
    pub fn empty() -> Self {
        Self::EMPTY
    }

    pub fn intern(sym: impl AsRef<str>) -> Self {
        let sym = sym.as_ref();
        if (sym.contains('$') && !sym.starts_with('_')) || DEFAULT_SYMBOLS.contains(sym) {
            INTERNER.get_or_intern(format!("_{}", sym))
        } else {
            INTERNER.get_or_intern(sym)
        }
    }

    pub fn intern_args(args: Arguments<'_>) -> Self {
        if let Some(s) = args.as_str() {
            Self::intern(s)
        } else {
            let s = args.to_string();
            Self::intern(s)
        }
    }

    pub fn new_from_ind(ind: usize) -> Self {
        Self::intern(ind.to_string())
    }

    pub fn as_str(&self) -> &'static str {
        match self.into_opt() {
            Some(_) => {
                let s = INTERNER.resolve(self);
                unsafe { mem::transmute::<&'_ str, &'static str>(s) }
            }
            None => "",
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        IndexType::is_empty(self)
    }

    // Split string but prefix contains delimiter
    pub fn split_once_with_delim<'a, P: Pattern<'a>>(
        &'a self,
        delimiter: P,
    ) -> Option<(&'a str, &'a str)> {
        let s = self.as_str();
        let (start, _) = delimiter.into_searcher(s).next_match()?;

        unsafe { Some((s.get_unchecked(.. start), s.get_unchecked(start ..))) }
    }
}
