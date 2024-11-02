use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    sync::Arc,
};

use arena_traits::{Arena, IndexAlloc, IndexIter};
use swc_atoms::Atom;
use swc_common::{Mark, Span, SyntaxContext};
use swc_ecma_ast::{Id, Ident};
pub trait AtomResolver {
    fn resolve(&self, len: usize) -> Atom;
}
pub struct DefaultAtomResolver {}
impl AtomResolver for DefaultAtomResolver {
    fn resolve(&self, len: usize) -> Atom {
        Atom::new(format!("${len}"))
    }
}
#[derive(Clone)]
pub struct LAM<T> {
    pub map: HashMap<Id, T>,
    pub default: T,
    pub resolver: Arc<dyn AtomResolver>,
}
impl<T: Default> LAM<T> {
    pub fn new(a: impl AtomResolver + 'static) -> Self {
        Self {
            map: HashMap::new(),
            default: T::default(),
            resolver: Arc::new(a),
        }
    }
}
impl<T: Default> Default for LAM<T> {
    fn default() -> Self {
        Self::new(DefaultAtomResolver {})
    }
}
impl<T> Index<Id> for LAM<T> {
    type Output = T;

    fn index(&self, index: Id) -> &Self::Output {
        match self.map.get(&index) {
            Some(a) => a,
            None => &self.default,
        }
    }
}
impl<T: Default> IndexMut<Id> for LAM<T> {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        self.map.entry(index).or_insert(T::default())
    }
}
impl<T: Default> IndexIter<Id> for LAM<T> {

    fn iter<'a>(&'a self) -> Box<(dyn Iterator<Item = Id> + 'a)> {
        Box::new(self.map.keys().cloned())
    }
}
impl<T: Default> IndexAlloc<Id> for LAM<T>{
    fn alloc(&mut self, a: Self::Output) -> Id {
        let len = self.map.len();
        let root = (
            self.resolver.resolve(len),
            SyntaxContext::empty().apply_mark(Mark::fresh(Mark::root())),
        );
        self[root.clone()] = a;
        return root;
    }
}