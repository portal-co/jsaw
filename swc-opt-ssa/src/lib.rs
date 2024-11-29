use std::collections::BTreeSet;

use id_arena::{Arena, Id};
use swc_ssa::{SCatch, SPostcedent, STarget, STerm, SValue};
pub mod into;
pub mod impls;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptType {
    Number,
    U32 { bits_usable: u8 },
    BigInt,
    U64 { bits_usable: u8 },
    Bool,
}
impl OptType {
    pub fn parent(&self) -> Option<OptType> {
        match self {
            OptType::U32 { bits_usable } => {
                if *bits_usable == 0 {
                    Some(OptType::Number)
                } else {
                    Some(OptType::U32 {
                        bits_usable: *bits_usable - 1,
                    })
                }
            }
            OptType::U64 { bits_usable } => {
                if *bits_usable == 0 {
                    Some(OptType::BigInt)
                } else {
                    Some(OptType::U64 {
                        bits_usable: *bits_usable - 1,
                    })
                }
            }
            _ => None,
        }
    }
}
#[derive(Clone)]
pub enum OptValue<I = Id<OptValueW>, B = Id<OptBlock>> {
    Deopt(I),
    Assert {
        val: I,
        ty: Option<OptType>,
    },
    Emit {
        val: SValue<I, B>,
        ty: Option<OptType>,
    },
}
#[derive(Clone)]
pub struct OptValueW(pub OptValue);
#[derive(Default)]
pub struct OptBlock {
    pub params: Vec<(Id<OptValueW>, Option<OptType>)>,
    pub insts: Vec<Id<OptValueW>>,
    pub postcedent: OptPostcedent,
}
pub type OptPostcedent = SPostcedent<Id<OptValueW>,Id<OptBlock>>;
pub type OptTarget = STarget<Id<OptValueW>,Id<OptBlock>>;
pub type OptTerm = STerm<Id<OptValueW>,Id<OptBlock>>;
pub type OptCatch = SCatch<Id<OptValueW>,Id<OptBlock>>;
#[derive(Default)]
pub struct OptCfg {
    pub values: Arena<OptValueW>,
    pub blocks: Arena<OptBlock>,
    pub decls: BTreeSet<swc_ecma_ast::Id>,
}
pub struct OptFunc {
    pub cfg: OptCfg,
    pub entry: Id<OptBlock>,
    pub is_generator: bool,
    pub is_async: bool,
}
impl OptCfg {
    pub fn add_blockparam(&mut self, k: Id<OptBlock>, ty: Option<OptType>) -> Id<OptValueW> {
        let v = self.values.alloc(OptValueW(OptValue::Emit {
            val: SValue::Param {
                block: k,
                idx: self.blocks[k].params.len(),
                ty: (),
            },
            ty,
        }));
        self.blocks[k].params.push((v,ty));
        return v;
    }
}
