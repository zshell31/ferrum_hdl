use std::{cmp, mem::MaybeUninit, ptr, rc::Rc};

use crate::const_helpers::{Assert, IsTrue};

#[derive(Debug, Clone)]
pub struct Memory<const N: usize, T>
where
    Assert<{ N > 0 }>: IsTrue,
{
    vals: Rc<[T; N]>,
}

impl<const N: usize, T: Default> Memory<N, T>
where
    Assert<{ N > 0 }>: IsTrue,
{
    pub fn empty() -> Self {
        let mut vals = unsafe { Box::<[MaybeUninit<T>; N]>::new_uninit().assume_init() };
        for val in vals.iter_mut() {
            unsafe {
                ptr::write(val.as_mut_ptr(), T::default());
            }
        }
        let vals: *mut [T; N] = Box::into_raw(vals).cast();
        Self {
            vals: unsafe { Rc::from_raw(vals as *const [T; N]) },
        }
    }

    pub fn init<const M: usize>(vals: [T; M]) -> Self
    where
        T: Default,
    {
        let mut mem = Self::empty();

        if M != 0 {
            let count = cmp::min(M, N);
            let dst = unsafe { Rc::get_mut_unchecked(&mut mem.vals) };

            for (i, src) in vals.into_iter().take(count).enumerate() {
                dst[i] = src;
            }
        }

        mem
    }
}
