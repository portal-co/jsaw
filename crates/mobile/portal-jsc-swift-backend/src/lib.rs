#![no_std]
use core::fmt::Display;
use alloc::{
    collections::btree_map::BTreeMap,
    format,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use itertools::Itertools;
use symbolism::Symbol;
extern crate alloc;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct SwiftName {
    pub id: u64,
}
impl Display for SwiftName {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "`js_{}`",
            Symbol::from_int(self.id).to_string().replace(" ", "$")
        )
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct TargetBlock {
    pub name: SwiftName,
}
impl Display for TargetBlock {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "label = .{}
            break",
            &self.name
        )
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct Return<T> {
    pub value: T,
}
impl<T: Display> Display for Return<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "return {};", &self.value)
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
            "var label = .unused;while 1{{switch label{{{}\ndefault:{}}}}}",
            self.map
                .iter()
                .map(|(t, k)| format!("case .{}: {k}", t.name))
                .join("\n"),
            self.entry
        )
    }
}
pub use portal_jsc_mob_backend_common::*;
