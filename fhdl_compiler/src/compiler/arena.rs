use bumpalo::collections::{CollectIn, Vec as BumpVec};

use super::Compiler;
use crate::error::Error;

impl<'tcx> Compiler<'tcx> {
    #[inline]
    pub fn alloc<T>(&self, val: T) -> &'tcx T {
        self.arena.alloc(val)
    }

    pub fn alloc_from_iter<T>(&self, it: impl IntoIterator<Item = T>) -> &'tcx [T] {
        let v = it.into_iter().collect_in::<BumpVec<'_, T>>(self.arena);

        v.into_bump_slice()
    }

    pub fn alloc_from_iter_res_with_gen<T, U>(
        &mut self,
        it: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Compiler<'tcx>, T) -> Result<U, Error>,
    ) -> Result<&'tcx [U], Error> {
        let arena = self.arena;
        let v = it
            .into_iter()
            .map(|item| f(self, item))
            .collect_in::<Result<BumpVec<'_, U>, Error>>(arena)?;

        Ok(v.into_bump_slice())
    }

    pub fn alloc_from_iter_opt_res_with_gen<T, U>(
        &mut self,
        it: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Compiler<'tcx>, T) -> Option<Result<U, Error>>,
    ) -> Result<&'tcx [U], Error> {
        let arena = self.arena;
        let v = it
            .into_iter()
            .filter_map(|item| f(self, item))
            .collect_in::<Result<BumpVec<'_, U>, Error>>(arena)?;

        Ok(v.into_bump_slice())
    }
}
