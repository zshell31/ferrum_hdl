use std::{
    borrow::Cow,
    fmt::{self, Display},
    rc::Rc,
};

use derive_where::derive_where;

use crate::signal::SignalValue;

pub trait FmtKind {}

#[derive(Clone)]
#[derive_where(Debug)]
pub struct Formatter<T> {
    pub(crate) name: Rc<Cow<'static, str>>,
    #[derive_where(skip)]
    pub(crate) fmt: fn(&T, f: &mut fmt::Formatter<'_>) -> fmt::Result,
}

impl<T> Formatter<T> {
    pub(crate) fn output(&self, value: &T) {
        struct Output<'a, T> {
            value: &'a T,
            fmt: &'a Formatter<T>,
        }

        impl<'a, T> Display for Output<'a, T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}: ", self.fmt.name)?;
                (self.fmt.fmt)(self.value, f)?;

                Ok(())
            }
        }

        println!("{}", Output { value, fmt: self });
    }
}

pub trait Watchable<F: FmtKind>: Sized {
    fn formatter(name: impl Into<Cow<'static, str>>) -> Formatter<Self>;
}

macro_rules! impl_fmt_kind {
    ($( $kind:ident => $trait:ident ),+) => {
        $(
            pub struct $kind;

            impl FmtKind for $kind {}

            impl<T: SignalValue + std::fmt::$trait> Watchable<$kind> for T {
                fn formatter(name: impl Into<Cow<'static, str>>) -> Formatter<T> {
                    Formatter {
                        name: Rc::new(name.into()),
                        fmt: <T as std::fmt::$trait>::fmt
                    }
                }

            }
        )+
    };
}

impl_fmt_kind!(
    AsDebug => Debug,
    AsDisplay => Display,
    AsBinary => Binary,
    AsLowerHex => LowerHex,
    AsUpperHex => UpperHex
);
