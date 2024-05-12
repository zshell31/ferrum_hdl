use ferrum_hdl::domain::{Polarity, SyncKind};
use itertools::Itertools;
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::{
    mir::UnevaluatedConst,
    ty::{
        fast_reject::{simplify_type, TreatParams},
        AssocKind, GenericArgsRef, Ty,
    },
};

use super::Compiler;
use crate::compiler::{cons_::const_val_to_u128, utils::AssocItemsExt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DomainId(u32);

impl DomainId {
    #[cfg(test)]
    pub fn empty() -> Self {
        Self(0)
    }
}

#[derive(Debug)]
pub struct Domain {
    pub freq: usize,
    pub rst_kind: SyncKind,
    pub rst_pol: Polarity,
}

#[derive(Default)]
pub struct Domains<'tcx> {
    types: FxHashMap<Ty<'tcx>, DomainId>,
    domains: Vec<Domain>,
}

impl<'tcx> Compiler<'tcx> {
    pub fn find_domain_by_type(
        &mut self,
        dom_ty: Ty<'tcx>,
        generics: GenericArgsRef<'tcx>,
    ) -> DomainId {
        #[allow(clippy::map_entry)]
        if !self.domains.types.contains_key(&dom_ty) {
            let domain = self.create_domain(dom_ty, generics);
            let dom_id = DomainId(self.domains.domains.len() as u32);
            self.domains.domains.push(domain);
            self.domains.types.insert(dom_ty, dom_id);
        }

        self.domains.types.get(&dom_ty).copied().unwrap()
    }

    pub fn find_domain_by_id(&self, dom_id: DomainId) -> &Domain {
        &self.domains.domains[dom_id.0 as usize]
    }

    fn create_domain(&self, dom_ty: Ty<'tcx>, generics: GenericArgsRef<'tcx>) -> Domain {
        let key = match simplify_type(self.tcx, dom_ty, TreatParams::AsCandidateKey) {
            Some(key) => key,
            None => panic!("Domain '{dom_ty:?}' cannot be simplified"),
        };

        let trait_impls = self.tcx.trait_impls_of(self.lang_items.domain);
        let trait_impl = match trait_impls
            .non_blanket_impls()
            .get(&key)
            .and_then(|impls| impls.iter().exactly_one().ok())
        {
            Some(trait_impl) => *trait_impl,
            None => {
                panic!("Domain '{dom_ty:?}' does not implement 'ClockDomain'")
            }
        };

        let assoc_items = self.tcx.associated_items(trait_impl);

        let freq = match assoc_items
            .find_by_lang_item(self.lang_items.freq, AssocKind::Const)
            .and_then(|freq| {
                self.const_eval_resolve(UnevaluatedConst::new(freq.def_id, generics))
            })
            .and_then(const_val_to_u128)
            .map(|freq| freq as usize)
        {
            Some(freq) => freq,
            None => panic!("Domain '{dom_ty:?}' does not have FREQ"),
        };

        let rst_kind = match assoc_items
            .find_by_lang_item(self.lang_items.rst_kind, AssocKind::Const)
            .and_then(|rst_kind| {
                self.const_eval_resolve(UnevaluatedConst::new(rst_kind.def_id, generics))
            })
            .and_then(const_val_to_u128)
            .and_then(SyncKind::from_val)
        {
            Some(rst_kind) => rst_kind,
            None => panic!("Domain '{dom_ty:?}' does not have RST_KIND"),
        };

        let rst_pol = match assoc_items
            .find_by_lang_item(self.lang_items.rst_pol, AssocKind::Const)
            .and_then(|rst_pol| {
                self.const_eval_resolve(UnevaluatedConst::new(rst_pol.def_id, generics))
            })
            .and_then(const_val_to_u128)
            .and_then(Polarity::from_val)
        {
            Some(rst_pol) => rst_pol,
            None => panic!("Domain '{dom_ty:?}' does not have RST_POLARITY"),
        };

        Domain {
            freq,
            rst_kind,
            rst_pol,
        }
    }
}
