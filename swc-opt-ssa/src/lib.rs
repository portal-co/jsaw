use std::collections::BTreeSet;

use id_arena::{Arena, Id};
use swc_ecma_ast::Lit;
use swc_ssa::{ch::ConstVal, simplify::SValGetter, SCatch, SPostcedent, STarget, STerm, SValue};
use swc_tac::Item;
pub mod impls;
pub mod into;
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum ObjType{
    Array,
    Object(Vec<String>)
}
#[derive(Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum OptType {
    Number,
    U32 {
        bits_usable: u8,
    },
    BigInt,
    U64 {
        bits_usable: u8,
    },
    Bool,
    Array {
        elem_ty: Box<Option<OptType>>,
    },
    Object {
        nest: ObjType,
        extensible: bool,
        elem_tys: Vec<Option<OptType>>,
    },
    Lit(Lit)
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
            OptType::Object {
                nest,
                extensible,
                elem_tys,
            } => {
                if !*extensible {
                    Some(OptType::Object {
                        nest: nest.clone(),
                        extensible: true,
                        elem_tys: elem_tys.clone(),
                    })
                } else {
                    match nest {
                        ObjType::Array => {
                            let mut elem_tys = elem_tys.clone();
                            if elem_tys.len() != 0 {
                                let Some(f) = elem_tys.iter().find_map(|a| a.clone()) else {
                                    return Some(OptType::Array {
                                        elem_ty: Box::new(None),
                                    });
                                };
                                for t in elem_tys.iter_mut().rev() {
                                    // if *t != f{
                                    match &*t {
                                        Some(a) if *a != f => *t = a.parent(),
                                        _ => {
                                            continue;
                                        }
                                    };
                                    return Some(OptType::Object {
                                        nest: ObjType::Array,
                                        extensible: true,
                                        elem_tys,
                                    });

                                    // }
                                }
                                return Some(OptType::Array {
                                    elem_ty: Box::new(Some(f)),
                                });
                            }
                            None
                        }
                        ObjType::Object(s) => {
                            let mut elem_tys = elem_tys.clone();
                            let mut s = s.clone();
                            let Some(p) = elem_tys.pop() else {
                                return None;
                            };
                            let q = s.pop().unwrap();
                            if let Some(p) = p {
                                elem_tys.push(p.parent());
                                s.push(q);
                            };
                            Some(OptType::Object {
                                nest: ObjType::Object(s),
                                extensible: true,
                                elem_tys,
                            })
                        }
                    }
                }
            }
            OptType::Lit(l) => match l{
                Lit::BigInt(i) => {
                    if *i.value > 0u8.into() && i.value.as_ref().clone() >> 64 == 0u8.into(){
                        let a: u64 = i.value.as_ref().clone().try_into().unwrap();
                        return Some(OptType::U64 { bits_usable: a.leading_zeros() as u8 })
                    }
                    Some(OptType::BigInt)
                }
                Lit::Num(n) => {
                    if let Some(a) = num_traits::cast(n.value){
                        let a: u32 = a;
                        return Some(OptType::U32 { bits_usable: a.leading_zeros() as u8 });
                    }
                    Some(OptType::Number)
                }
                _ => None
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
impl OptValueW {
    pub fn ty(&self, cfg: &OptCfg) -> Option<OptType> {
        match &self.0 {
            OptValue::Deopt(d) => {
                let x = cfg.values[*d].ty(cfg);
                x.and_then(|y| y.parent())
            }
            OptValue::Assert { val, ty } => ty.clone(),
            OptValue::Emit { val, ty } => ty.clone(),
        }
    }
    pub fn constant(&self, cfg: &OptCfg) -> Option<Lit>{
        match &self.0{
            OptValue::Deopt(a) => cfg.values[*a].constant(cfg),
            OptValue::Assert { val, ty } => cfg.values[*val].constant(cfg),
            OptValue::Emit { val, ty } => match val{
                SValue::Item(i) => match i{
                    Item::Lit { lit } => Some(lit.clone()),
                    _ => None
                }
                _ => None,
            },
        }
    }
}
impl SValGetter<Id<OptValueW>,Id<OptBlock>> for OptCfg{
    fn val(&self, id: Id<OptValueW>) -> Option<&SValue<Id<OptValueW>,Id<OptBlock>>> {
        match &self.values[id].0{
            OptValue::Deopt(a) => self.val(*a),
            OptValue::Assert { val, ty } => self.val(*val),
            OptValue::Emit { val, ty } => Some(val),
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
