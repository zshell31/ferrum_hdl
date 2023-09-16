pub trait Simulate: Sized {
    type Value;

    fn next(&mut self, cycle: u16) -> Self::Value;

    fn values(self) -> Values<Self> {
        Values {
            cycle: 0,
            source: self,
        }
    }
}

pub struct Values<S> {
    cycle: u16,
    source: S,
}

impl<S: Simulate> Values<S> {
    pub fn next_cycle(&mut self) -> S::Value {
        self.next().unwrap()
    }
}

impl<S: Simulate> Iterator for Values<S> {
    type Item = S::Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.cycle += 1;
        Some(self.source.next(self.cycle))
    }
}
