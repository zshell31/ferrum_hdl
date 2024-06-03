use crate::{
    new_hdl::{ClockDomain, Module, Reg, State},
    prelude::{ArrayExt, Bit, SignalValue},
};

#[derive(State)]
pub struct ShiftReg<D: ClockDomain, const N: usize, T: SignalValue> {
    regs: [Reg<D, T>; N],
}

#[derive(Debug, Clone, SignalValue)]
pub struct ShiftRegInput<T> {
    pub next: Bit,
    pub init: T,
    pub data: T,
}

impl<D: ClockDomain, const N: usize, T: SignalValue + Default> Module
    for ShiftRegMut<'_, D, N, T>
{
    type Input = ShiftRegInput<T>;
    type Output = [T; N];

    fn logic(self, ShiftRegInput { next, init, data }: Self::Input) -> Self::Output {
        let mut sr = <[T; N]>::make(T::default);
        let mut prev = data;

        for (idx, reg) in self.regs.into_iter().enumerate() {
            sr[idx] = reg.reg_en(init.clone(), next, |_| prev.clone());
            prev = sr[idx].clone();
        }
        let sr = sr;
        sr
    }
}
