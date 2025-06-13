use std::collections::BTreeMap;

use bitflags::bitflags;
use swc_ecma_ast::Lit;

bitflags! {
    #[derive(Default,Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
    pub struct ParentFlags: u64{
        const STRICT = 0x1;
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash,Debug)]
#[non_exhaustive]
pub enum ObjType {
    Array,
    Object(Vec<String>),
}
#[derive(Clone, PartialEq, Eq, Hash,Debug)]
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
        /// Represents whether an object type has been extended
        extended: bool,
        elem_tys: Vec<Option<OptType>>,
    },
    Lit(Lit),
    Variant {
        parts: BTreeMap<String, Vec<Option<OptType>>>,
    },
}
impl OptType {
    pub fn parent(&self, flags: ParentFlags) -> Option<OptType> {
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
                    elem_ty: Box::new(a.parent(flags)),
                }),
                None => None,
            },
            OptType::Object {
                nest,
                extended: extensible,
                elem_tys,
            } => {
                if !*extensible {
                    Some(OptType::Object {
                        nest: nest.clone(),
                        extended: true,
                        elem_tys: elem_tys.clone(),
                    })
                } else {
                    match nest {
                        ObjType::Array => {
                            let mut elem_tys = elem_tys.clone();
                            if elem_tys.len() != 0 {
                                let Some(first_type) = elem_tys.iter().find_map(|a| a.clone()) else {
                                    return Some(OptType::Array {
                                        elem_ty: Box::new(None),
                                    });
                                };
                                for t in elem_tys.iter_mut().rev() {
                                    // if *t != f{
                                    match &*t {
                                        Some(a) if *a != first_type => *t = a.parent(flags),
                                        _ => {
                                            continue;
                                        }
                                    };
                                    return Some(OptType::Object {
                                        nest: ObjType::Array,
                                        extended: true,
                                        elem_tys,
                                    });

                                    // }
                                }
                                return Some(OptType::Array {
                                    elem_ty: Box::new(Some(first_type)),
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
                                elem_tys.push(p.parent(flags));
                                s.push(q);
                            };
                            Some(OptType::Object {
                                nest: ObjType::Object(s),
                                extended: true,
                                elem_tys,
                            })
                        }
                    }
                }
            }
            OptType::Lit(l) => match l {
                Lit::BigInt(i) => {
                    if *i.value > 0u8.into() && i.value.as_ref().clone() >> 64 == 0u8.into() {
                        let a: u64 = i.value.as_ref().clone().try_into().unwrap();
                        return Some(OptType::U64 {
                            bits_usable: a.leading_zeros() as u8,
                        });
                    }
                    Some(OptType::BigInt)
                }
                Lit::Num(n) => {
                    if let Some(a) = num_traits::cast(n.value) {
                        let a: u32 = a;
                        return Some(OptType::U32 {
                            bits_usable: a.leading_zeros() as u8,
                        });
                    }
                    Some(OptType::Number)
                }
                _ => None,
            },
            OptType::Variant { parts } => {
                let mut parts = parts.clone();
                for (_, p) in parts.iter_mut() {
                    for p in p.iter_mut().rev() {
                        if let Some(r) = p.take() {
                            *p = r.parent(flags);
                            return Some(OptType::Variant { parts: parts });
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }
}
