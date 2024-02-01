use std::{cell::OnceCell, fmt::Arguments, mem};

use bumpalo::{
    collections::{vec::Vec as BumpVec, CollectIn},
    Bump,
};
use smallvec::SmallVec;

#[derive(Default)]
pub struct Arena(Bump);

macro_rules! assert_is_not_empty {
    ($ty:ident) => {
        assert!(mem::size_of::<T>() != 0);
    };
}

impl Arena {
    fn with_capacity(n: usize) -> Self {
        Self(Bump::with_capacity(n))
    }

    pub fn alloc<T>(&self, val: T) -> &mut T {
        assert_is_not_empty!(T);
        self.0.alloc(val)
    }

    pub fn alloc_str(&self, src: &str) -> &mut str {
        self.0.alloc_str(src)
    }

    pub fn alloc_args(&self, args: Arguments<'_>) -> &str {
        use bumpalo::core_alloc::fmt::Write;
        let b = &self.0;
        let mut s = bumpalo::collections::String::new_in(b);
        let _ = s.write_fmt(args);
        s.into_bump_str()
    }

    pub fn alloc_slice<T: Copy>(&self, vals: &[T]) -> &mut [T] {
        assert_is_not_empty!(T);
        self.0.alloc_slice_copy(vals)
    }

    pub fn alloc_vec<T, I: IntoIterator<Item = T>>(&self, iter: I) -> BumpVec<'_, T> {
        assert_is_not_empty!(T);
        iter.into_iter().collect_in(&self.0)
    }

    pub fn alloc_vec_opt<T, I: IntoIterator<Item = Option<T>>>(
        &self,
        iter: I,
    ) -> Option<BumpVec<'_, T>> {
        assert_is_not_empty!(T);
        iter.into_iter().collect_in::<Option<BumpVec<_>>>(&self.0)
    }

    pub fn alloc_from_iter<T: Copy, I: IntoIterator<Item = T>>(
        &self,
        iter: I,
    ) -> &mut [T] {
        assert_is_not_empty!(T);
        let mut iter = iter.into_iter();
        let size_hint = iter.size_hint();

        match size_hint {
            (min, Some(max)) if min == max => {
                let len = min;

                if len == 0 {
                    return &mut [];
                }

                self.0.alloc_slice_fill_with(len, |_| {
                    iter.next().expect("Iterator supplied too few elements")
                })
            }
            (_, _) => {
                let vec: SmallVec<[_; 8]> = iter.collect();
                if vec.is_empty() {
                    return &mut [];
                }

                self.0.alloc_slice_copy(&vec)
            }
        }
    }

    pub fn alloc_from_res_iter<E, T: Copy, I: IntoIterator<Item = Result<T, E>>>(
        &self,
        iter: I,
    ) -> Result<&mut [T], E> {
        assert_is_not_empty!(T);
        let vec: SmallVec<[_; 8]> = iter.into_iter().collect::<Result<_, E>>()?;

        Ok(self.0.alloc_slice_copy(&vec))
    }

    pub fn alloc_from_opt_iter<T: Copy, I: IntoIterator<Item = Option<T>>>(
        &self,
        iter: I,
    ) -> Option<&mut [T]> {
        assert_is_not_empty!(T);
        let vec: SmallVec<[_; 8]> = iter.into_iter().collect::<Option<_>>()?;

        Some(self.0.alloc_slice_copy(&vec))
    }
}

const ARENA_CAP: usize = 256;

/// # Safety
/// This function should be invoked only in single-threadead application because it initializes global Arena that
/// does not implement Sync
pub unsafe fn with_arena() -> &'static Arena {
    static mut INSTANCE: OnceCell<Arena> = OnceCell::new();
    INSTANCE.get_or_init(|| Arena::with_capacity(ARENA_CAP))
}
