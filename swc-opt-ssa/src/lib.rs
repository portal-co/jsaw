use std::collections::BTreeSet;

use id_arena::{Arena, Id};
use swc_ssa::{SCatch, SPostcedent, STarget, STerm, SValue};
pub mod impls;
pub mod into;
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptType {
    Number,
    U32 { bits_usable: u8 },
    BigInt,
    U64 { bits_usable: u8 },
    Bool,
    Array { elem_ty: Box<Option<OptType>> },
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
            OptType::Array { elem_ty } => match elem_ty.as_ref() {
                Some(a) => Some(OptType::Array {
                    elem_ty: Box::new(a.parent()),
                }),
                None => None,
            },
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
pub type OptPostcedent = SPostcedent<Id<OptValueW>, Id<OptBlock>>;
pub type OptTarget = STarget<Id<OptValueW>, Id<OptBlock>>;
pub type OptTerm = STerm<Id<OptValueW>, Id<OptBlock>>;
pub type OptCatch = SCatch<Id<OptValueW>, Id<OptBlock>>;
#[derive(Default)]
pub struct OptCfg {
    pub values: Arena<OptValueW>,
    pub blocks: Arena<OptBlock>,
    pub decls: BTreeSet<swc_ecma_ast::Id>,
}
impl OptValueW{
    pub fn ty(&self, cfg: &OptCfg) -> Option<OptType>{
        match &self.0 {
            OptValue::Deopt(d) => {
                let x = cfg.values[*d].ty(cfg);
                x.and_then(|y| y.parent())
            }
            OptValue::Assert { val, ty } => ty.clone(),
            OptValue::Emit { val, ty } => ty.clone(),
        }
    }
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
            ty: ty.clone(),
        }));
        self.blocks[k].params.push((v, ty));
        return v;
    }
}
