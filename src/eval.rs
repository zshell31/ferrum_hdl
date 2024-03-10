use crate::domain::{Clock, ClockDomain};

#[derive(Debug)]
pub struct EvalCtx {
    time: usize,
}

impl EvalCtx {
    pub(crate) fn new() -> Self {
        Self { time: 0 }
    }

    pub(crate) fn time(&self) -> usize {
        self.time
    }

    pub(crate) fn next_time(&mut self) {
        self.time = self.time.wrapping_add(1);
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
    pub opts: EvalOpts,
}

impl<D: ClockDomain, S: Eval<D>> EvalIter<D, S> {
    pub fn next_time(&mut self) -> S::Value {
        if self.opts.auto_clk {
            self.clk.invert();
        }
        self.ctx.next_time();
        self.source.next(&mut self.ctx)
    }
}

impl<D: ClockDomain, S: Eval<D>> Iterator for EvalIter<D, S> {
    type Item = S::Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_time())
    }
}
