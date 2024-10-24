use std::{
    collections::HashMap,
    ops::{Index, IndexMut}, sync::Arc,
};

use arena_traits::Arena;
use swc_atoms::Atom;
use swc_common::Span;
use swc_ecma_ast::Ident;
pub trait AtomResolver{
    fn resolve(&mut self, len: usize) -> Atom;
}
pub struct DefaultAtomResolver{
    
}
impl AtomResolver for DefaultAtomResolver{
    fn resolve(&mut self, len: usize) -> Atom {
        Atom::new(format!("${len}"))
    }
}
pub struct LAM<T> {
    pub map: HashMap<Ident, T>,
    pub default: T,
    pub resolver: Box<dyn AtomResolver>
}
impl<T: Default> LAM<T>{
    pub fn new(a: impl AtomResolver + 'static) -> Self{
        Self { map: HashMap::new(), default: T::default(), resolver: Box::new(a) }
    }
}
impl<T: Default> Default for LAM<T>{
    fn default() -> Self {
        Self::new(DefaultAtomResolver{
            
        })
    }
}
impl<T> Index<Ident> for LAM<T> {
    type Output = T;

    fn index(&self, index: Ident) -> &Self::Output {
        match self.map.get(&index) {
            Some(a) => a,
            None => &self.default,
        }
    }
}
impl<T: Default> IndexMut<Ident> for LAM<T> {
    fn index_mut(&mut self, index: Ident) -> &mut Self::Output {
        self.map.entry(index).or_insert(T::default())
    }
}
impl<T: Default> Arena<Ident> for LAM<T> {
    fn alloc(&mut self, a: Self::Output) -> Ident {
        let len = self.map.len();
        let root = Ident::new_private(self.resolver.resolve(len), Span::dummy_with_cmt());
        self[root.clone()] = a;
        return root;
    }

    fn iter(&self) -> impl Iterator<Item = Ident> {
        self.map.keys().cloned()
    }
}
