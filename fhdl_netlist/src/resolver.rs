use smallvec::{Array, SmallVec};

use crate::{
    net_list::{ModuleId, ParamId, TyId},
    sig_ty::NodeTy,
};

pub trait Resolver {
    type Error;

    fn resolve_node_ty(&mut self, ty_id: TyId) -> Result<NodeTy, Self::Error>;

    fn resolve_const_param(
        &mut self,
        ty_id: TyId,
        param_id: ParamId,
    ) -> Result<u128, Self::Error>;

    fn resolve_module_id(&mut self, module_id: ModuleId)
        -> Result<ModuleId, Self::Error>;
}

pub trait Resolve<R: Resolver>: Sized {
    fn resolve(&self, resolver: &mut R) -> Result<Self, R::Error>;
}

impl<R: Resolver> Resolve<R> for u128 {
    fn resolve(&self, _resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(*self)
    }
}

impl<R: Resolver, T: Resolve<R>> Resolve<R> for Option<T> {
    fn resolve(&self, resolver: &mut R) -> Result<Self, R::Error> {
        match self {
            Some(value) => Ok(Some(value.resolve(resolver)?)),
            None => Ok(None),
        }
    }
}

impl<R: Resolver, T: Resolve<R>> Resolve<R> for Vec<T> {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        self.iter().map(|item| item.resolve(resolver)).collect()
    }
}

impl<R: Resolver, A: Array> Resolve<R> for SmallVec<A>
where
    A::Item: Resolve<R>,
{
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        self.iter().map(|item| item.resolve(resolver)).collect()
    }
}
