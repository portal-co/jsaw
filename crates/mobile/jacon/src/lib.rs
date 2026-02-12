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
pub use portal_jsc_mob_backend_common::*;
