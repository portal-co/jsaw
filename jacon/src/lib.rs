#![no_std]

use core::fmt::Display;

use alloc::{
    collections::btree_map::BTreeMap,
    string::{String, ToString},
    sync::Arc,
    vec::{self, Vec},
};
use itertools::Itertools;
extern crate alloc;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct TargetBlock {
    pub id: u32,
}
impl Display for TargetBlock {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "return io.jbock.util.Either.right({});", self.id)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct Return<T> {
    pub value: T,
}
impl<T: Display> Display for Return<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "return io.jbock.util.Either.left({});", &self.value)
    }
}
#[derive(Clone)]
pub struct Blocks<K> {
    pub map: BTreeMap<TargetBlock, Term<K>>,
    pub entry: TargetBlock,
}
impl<K: Display> Display for Blocks<K> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "pc.portal.jacon.Blocks.blocks({},[{}])",
            self.entry.id,
            (0..(self.map.keys().map(|a| a.id).max().unwrap_or_default()))
                .map(|a| {
                    let Some(a) = self.map.get(&TargetBlock { id: a }) else {
                        return alloc::format!("null");
                    };

                    alloc::format!("(() -> {{{a}}})")
                })
                .join(",")
        )
    }
}
#[derive(Clone)]
pub struct Term<K> {
    pub blocks: Vec<K>,
    pub term: Arc<dyn Fn(&[String]) -> String>,
}
impl<K> Term<K> {
    pub fn raw(a: String) -> Self {
        Self {
            blocks: alloc::vec![],
            term: Arc::new(move |_| a.clone()),
        }
    }
    pub fn core(k: K) -> Self {
        Self {
            blocks: alloc::vec![k],
            term: Arc::new(|a| a[0].clone()),
        }
    }
    pub fn prepend_stmt(self, a: String) -> Self {
        let Self { blocks, term } = self;
        Self {
            blocks,
            term: Arc::new(move |s| {
                let t = term(s);
                alloc::format!("{a};{t}")
            }),
        }
    }
    pub fn r#if(self, cond: String, r#else: Term<K>) -> Self {
        let a = self.blocks.len();
        let b = self.term.clone();
        let c = r#else.term;
        Self {
            blocks: self
                .blocks
                .into_iter()
                .chain(r#else.blocks.into_iter())
                .collect(),
            term: Arc::new(move |s| {
                let (a, d) = s.split_at(a);
                let b = b(a);
                let c = c(a);
                alloc::format!("if({cond}){{{b}}}else{{{c}}}")
            }),
        }
    }
    pub fn map<U, E>(self, f: impl FnMut(K) -> Result<U, E>) -> Result<Term<U>, E> {
        Ok(Term {
            blocks: self.blocks.into_iter().map(f).collect::<Result<_, E>>()?,
            term: self.term,
        })
    }
    pub fn as_ref(&self) -> Term<&K> {
        Term {
            blocks: self.blocks.iter().collect(),
            term: self.term.clone(),
        }
    }
    pub fn as_mut(&mut self) -> Term<&mut K> {
        Term {
            blocks: self.blocks.iter_mut().collect(),
            term: self.term.clone(),
        }
    }
}
impl<K: Display> Display for Term<K> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            (self.term)(
                &self
                    .blocks
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
            )
        )
    }
}
