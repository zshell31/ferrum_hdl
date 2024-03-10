use derive_where::derive_where;

use crate::{eval::EvalCtx, signal::SignalValue};

#[derive_where(Debug; T)]
pub struct SignalFn<T: SignalValue> {
    time: usize,
    cached: Option<T>,
    #[derive_where(skip)]
    f: Box<dyn FnMut(&mut EvalCtx) -> T>,
}

impl<T: SignalValue> SignalFn<T> {
    pub(crate) fn new(f: impl FnMut(&mut EvalCtx) -> T + 'static) -> Self {
        Self {
            time: usize::MAX,
            cached: None,
            f: Box::new(f),
        }
    }

    #[inline]
    fn call(&mut self, ctx: &mut EvalCtx) -> T {
        (self.f)(ctx)
    }

    pub(crate) fn next_val(&mut self, ctx: &mut EvalCtx) -> T {
        let time = ctx.time();
        if self.time != time {
            let new_val = self.call(ctx);
            self.cached.replace(new_val);
            self.time = time;
        }

        self.value()
    }

    #[inline]
    pub(crate) fn value(&self) -> T {
        self.cached.clone().unwrap()
    }

    #[inline]
    pub(crate) fn as_value_opt(&self) -> Option<&T> {
        self.cached.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signal_fn() {
        let mut ctx = EvalCtx::new();
        let mut value = 0;
        let mut signal_fn = SignalFn::new(move |_| {
            value += 1_u8;
            value
        });

        ctx.next_time();
        assert_eq!(signal_fn.next_val(&mut ctx), 1);
        assert_eq!(signal_fn.next_val(&mut ctx), 1);

        ctx.next_time();
        assert_eq!(signal_fn.next_val(&mut ctx), 2);
        assert_eq!(signal_fn.next_val(&mut ctx), 2);
    }
}
