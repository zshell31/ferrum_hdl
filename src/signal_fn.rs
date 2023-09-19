use derive_where::derive_where;

use crate::signal::SignalValue;

#[derive_where(Debug)]
pub struct SignalFn<T: SignalValue> {
    cycle: u16,
    cached: Option<T>,
    #[derive_where(skip)]
    f: Box<dyn FnMut(u16) -> T>,
}

impl<T: SignalValue> SignalFn<T> {
    pub(crate) fn new(f: impl FnMut(u16) -> T + 'static) -> Self {
        Self {
            cycle: u16::MAX,
            cached: None,
            f: Box::new(f),
        }
    }

    fn call(&mut self, cycle: u16) -> T {
        (self.f)(cycle)
    }

    pub(crate) fn next_val(&mut self, cycle: u16) -> T {
        if self.cycle != cycle {
            let new_val = self.call(cycle);
            self.cached.replace(new_val);
            self.cycle = cycle;
        }

        self.value()
    }

    pub(crate) fn value(&self) -> T {
        self.cached.unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl SignalValue for u8 {}

    #[test]
    fn signal_fn() {
        let mut cycle = 0;
        let mut value = 0;
        let mut signal_fn = SignalFn::new(move |_| {
            value += 1;
            value
        });

        cycle += 1;
        assert_eq!(signal_fn.next_val(cycle), 1);
        assert_eq!(signal_fn.next_val(cycle), 1);

        cycle += 1;
        assert_eq!(signal_fn.next_val(cycle), 2);
        assert_eq!(signal_fn.next_val(cycle), 2);
    }
}
