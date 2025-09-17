#![no_std]

use core::fmt::Display;

use alloc::{
    collections::btree_map::BTreeMap,
    string::{String, ToString},
    sync::Arc,
    vec::{self, Vec},
};
// use itertools::Itertools;
extern crate alloc;

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
                let c = c(d);
                alloc::format!("if({cond}){{{b}}}else{{{c}}}")
            }),
        }
    }
    pub fn map<U, E>(self, f: &mut (dyn FnMut(K) -> Result<U, E> + '_)) -> Result<Term<U>, E> {
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
