use crate::domain::{Clock, ClockDomain};

#[derive(Debug)]
pub struct EvalCtx {
    time: u64,
}

impl EvalCtx {
    pub(crate) fn new() -> Self {
        Self { time: u64::MAX }
    }

    pub(crate) fn time(&self) -> u64 {
        self.time
    }

    pub(crate) fn set_next_time(&mut self) {
        self.time = self.time.wrapping_add(1);
    }

    pub(crate) fn next_time(&self) -> u64 {
        self.time.wrapping_add(1)
    }
}

#[derive(Debug)]
pub struct EvalOpts {
    pub auto_clk: bool,
}

impl Default for EvalOpts {
    fn default() -> Self {
        Self { auto_clk: true }
    }
}

pub trait Eval<D: ClockDomain>: Sized {
    type Value;

    fn next(&mut self, ctx: &mut EvalCtx) -> Self::Value;

    #[inline]
    fn eval(self, clk: &Clock<D>) -> EvalIter<D, Self> {
        self.eval_with_opts(clk, Default::default())
    }

    fn eval_with_opts(self, clk: &Clock<D>, opts: EvalOpts) -> EvalIter<D, Self> {
        EvalIter {
            ctx: EvalCtx::new(),
            source: self,
            clk: clk.clone(),
            opts,
        }
    }
}

#[derive(Debug)]
pub struct EvalIter<D: ClockDomain, S> {
    ctx: EvalCtx,
    source: S,
    clk: Clock<D>,
    opts: EvalOpts,
}

impl<D: ClockDomain, S: Eval<D>> EvalIter<D, S> {
    pub fn eval(&mut self) -> S::Value {
        if self.opts.auto_clk {
            self.clk.invert();
        }
        self.ctx.set_next_time();
        self.source.next(&mut self.ctx)
    }

    pub fn clk(&self) -> &Clock<D> {
        &self.clk
    }

    pub fn time(&self) -> u64 {
        self.ctx.time()
    }

    pub fn next_time(&self) -> u64 {
        self.ctx.next_time()
    }

    pub fn with_time(self) -> WithTime<D, S> {
        WithTime { inner: self }
    }
}

impl<D: ClockDomain, S: Eval<D>> Iterator for EvalIter<D, S> {
    type Item = S::Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.eval())
    }
}

pub struct WithTime<D: ClockDomain, S> {
    inner: EvalIter<D, S>,
}

impl<D: ClockDomain, S: Eval<D>> Iterator for WithTime<D, S> {
    type Item = (u64, S::Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|value| (self.inner.time(), value))
    }
}
