use std::{
    collections::{BTreeMap, BTreeSet},
    vec::Vec,
};

use portal_jsc_swc_batch::{ImportMapping, ModuleMapping};
use quote::ToTokens;
use swc_ecma_ast::Module;
use swc_ecma_visit::VisitMutWith;
#[derive(Default)]
#[non_exhaustive]
pub struct VarManager {
    pub idents: BTreeMap<proc_macro2::Ident, BTreeSet<proc_macro2::TokenStream>>,
    pub cur_id: usize,
}
impl ToTokens for VarManager {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let idents = self
            .idents
            .iter()
            .map(|(a, b)| quote::quote! {#a : #(#b )+*});
        tokens.extend(quote::quote! {
            <#(#idents),*>
        });
    }
}
impl VarManager {
    pub fn next_id(&mut self) -> proc_macro2::Ident {
        let n = self.cur_id;
        self.cur_id += 1;
        quote::format_ident!("T{n}")
    }
}
