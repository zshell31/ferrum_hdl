use derive_where::derive_where;

use crate::{signal::SignalValue, simulation::SimCtx};

#[derive_where(Debug; T)]
pub struct SignalFn<T: SignalValue> {
    cycle: usize,
    cached: Option<T>,
    #[derive_where(skip)]
    f: Box<dyn FnMut(&mut SimCtx) -> T>,
}

impl<T: SignalValue> SignalFn<T> {
    pub(crate) fn new(f: impl FnMut(&mut SimCtx) -> T + 'static) -> Self {
        Self {
            cycle: usize::MAX,
            cached: None,
            f: Box::new(f),
        }
    }

    fn call(&mut self, ctx: &mut SimCtx) -> T {
        (self.f)(ctx)
    }

    pub(crate) fn next_val(&mut self, ctx: &mut SimCtx) -> T {
        let cycle = ctx.cycle();
        if self.cycle != cycle {
            let new_val = self.call(ctx);
            self.cached.replace(new_val);
            self.cycle = cycle;
        }

        self.value()
    }

    pub(crate) fn value(&self) -> T {
        self.cached.clone().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signal_fn() {
        let mut ctx = SimCtx::new();
        let mut value = 0;
        let mut signal_fn = SignalFn::new(move |_| {
            value += 1_u8;
            value
        });

        ctx.next_cycle();
        assert_eq!(signal_fn.next_val(&mut ctx), 1);
        assert_eq!(signal_fn.next_val(&mut ctx), 1);

        ctx.next_cycle();
        assert_eq!(signal_fn.next_val(&mut ctx), 2);
        assert_eq!(signal_fn.next_val(&mut ctx), 2);
    }
}
