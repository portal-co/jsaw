use std::collections::BTreeMap;

use id_arena::Id;
use swc_atoms::Atom;
use swc_common::Span;
use swc_ecma_ast::Id as Ident;
use swc_tac::{Item, LId, TBlock, TCatch, TCfg, TFunc, TTerm, ValFlags};

use crate::{SBlock, SFunc, STarget, SValue, SValueW};

impl TryFrom<SFunc> for TFunc {
    type Error = anyhow::Error;

    fn try_from(value: SFunc) -> Result<Self, Self::Error> {
        let mut cfg = TCfg::default();
        cfg.decls.extend(value.cfg.decls.iter().cloned());
        let mut rew = Rew {
            blocks: Default::default(),
        };
        let entry = rew.trans(&value, &mut cfg, BlockEntry::Block(value.entry))?;
        let params = value.cfg.blocks[value.entry]
            .params
            .iter()
            .map(|v| mangle_value(&value, v.0))
            .collect();
        for (v, t) in value.cfg.ts.clone().into_iter() {
            cfg.type_annotations.insert(mangle_value(&value, v), t);
        }
        cfg.ts_retty = value.cfg.ts_retty;
        cfg.generics = value.cfg.generics;

        Ok(Self {
            cfg,
            entry,
            params,
            is_generator: value.is_generator,
            is_async: value.is_async,
            ts_params: value.ts_params,
        })
    }
}
#[derive(Default)]
#[non_exhaustive]
pub struct Rew {
    pub blocks: BTreeMap<BlockEntry, Id<TBlock>>,
}

#[derive(Clone, Ord, PartialEq, PartialOrd, Eq)]
pub enum BlockEntry {
    Block(Id<SBlock>),
    Target(STarget, Option<Ident>),
}
impl Rew {
    pub fn trans(&mut self, a: &SFunc, b: &mut TCfg, k: BlockEntry) -> anyhow::Result<Id<TBlock>> {
        loop {
            if let Some(x) = self.blocks.get(&k) {
                return Ok(*x);
            }
            let k2 = b.blocks.alloc(Default::default());
            self.blocks.insert(k.clone(), k2);
            match &k {
                BlockEntry::Block(id) => {
                    let catch = match &a.cfg.blocks[*id].postcedent.catch {
                        crate::SCatch::Throw => TCatch::Throw,
                        crate::SCatch::Just { target } => {
                            let error = (Atom::new("$error"), Default::default());
                            b.decls.insert(error.clone());
                            TCatch::Jump {
                                pat: error.clone(),
                                k: self.trans(
                                    a,
                                    b,
                                    BlockEntry::Target(target.clone(), Some(error)),
                                )?,
                            }
                        }
                    };
                    b.blocks[k2].catch = catch;
                    for val in a.cfg.blocks[*id].stmts.iter() {
                        match &a.cfg.values[*val].0 {
                            SValue::Param { block, idx, ty } => todo!(),
                            SValue::Item { item, span } => {
                                let i =
                                    item.clone().map(&mut |v| anyhow::Ok(mangle_value(a, v)))?;
                                b.blocks[k2].stmts.push((
                                    LId::Id {
                                        id: mangle_value(a, *val),
                                    },
                                    ValFlags::SSA_LIKE,
                                    i,
                                    span.clone().unwrap_or_else(|| Span::dummy_with_cmt()),
                                ));
                                b.decls.insert(mangle_value(a, *val));
                            }
                            SValue::Assign { target, val } => {
                                let target = target.clone().map(&mut |v| {
                                    let m = mangle_value(a, v);
                                    b.decls.insert(m.clone());
                                    return anyhow::Ok(m);
                                })?;
                                b.blocks[k2].stmts.push((
                                    target,
                                    Default::default(),
                                    Item::Just {
                                        id: mangle_value(a, *val),
                                    },
                                    Span::dummy_with_cmt(),
                                ));
                            }
                            SValue::LoadId(i) => {
                                b.blocks[k2].stmts.push((
                                    LId::Id {
                                        id: mangle_value(a, *val),
                                    },
                                    ValFlags::SSA_LIKE,
                                    Item::Just { id: i.clone() },
                                    Span::dummy_with_cmt(),
                                ));
                                b.decls.insert(mangle_value(a, *val));
                            }
                            SValue::StoreId { target, val } => {
                                b.blocks[k2].stmts.push((
                                    LId::Id { id: target.clone() },
                                    Default::default(),
                                    Item::Just {
                                        id: mangle_value(a, *val),
                                    },
                                    Span::dummy_with_cmt(),
                                ));
                            }
                            SValue::Benc(v) => {
                                b.blocks[k2].stmts.push((
                                    LId::Id {
                                        id: mangle_value(a, *val),
                                    },
                                    ValFlags::SSA_LIKE,
                                    Item::Just {
                                        id: mangle_value(a, *v),
                                    },
                                    Span::dummy_with_cmt(),
                                ));
                            }
                        }
                    }
                    let term = match &a.cfg.blocks[*id].postcedent.term {
                        crate::STerm::Throw(id) => swc_tac::TTerm::Throw(mangle_value(a, *id)),
                        crate::STerm::Return(id) => {
                            swc_tac::TTerm::Return(id.clone().map(|v| mangle_value(a, v)))
                        }
                        crate::STerm::Jmp(starget) => TTerm::Jmp(self.trans(
                            a,
                            b,
                            BlockEntry::Target(starget.clone(), None),
                        )?),
                        crate::STerm::CondJmp {
                            cond,
                            if_true,
                            if_false,
                        } => {
                            let if_true =
                                self.trans(a, b, BlockEntry::Target(if_true.clone(), None))?;
                            let if_false =
                                self.trans(a, b, BlockEntry::Target(if_false.clone(), None))?;
                            TTerm::CondJmp {
                                cond: mangle_value(a, *cond),
                                if_true,
                                if_false,
                            }
                        }
                        crate::STerm::Switch { x, blocks, default } => {
                            let default =
                                self.trans(a, b, BlockEntry::Target(default.clone(), None))?;
                            let blocks = blocks
                                .iter()
                                .map(|(a2, b2)| {
                                    anyhow::Ok((
                                        mangle_value(a, *a2),
                                        self.trans(a, b, BlockEntry::Target(b2.clone(), None))?,
                                    ))
                                })
                                .collect::<anyhow::Result<_>>()?;
                            TTerm::Switch {
                                x: mangle_value(a, *x),
                                blocks,
                                default,
                            }
                        }
                        crate::STerm::Default => swc_tac::TTerm::Default,
                    };
                    b.blocks[k2].term = term;
                }
                BlockEntry::Target(starget, val) => {
                    let stmts = val
                        .iter()
                        .cloned()
                        .chain(starget.args.iter().map(|b| mangle_value(a, *b)))
                        .enumerate()
                        .map(|(a2, b)| {
                            (
                                LId::Id {
                                    id: mangle_param(starget.block, a2),
                                },
                                Default::default(),
                                Item::Just { id: b },
                                Span::dummy_with_cmt(),
                            )
                        });
                    b.blocks[k2].stmts.extend(stmts);
                    let term =
                        swc_tac::TTerm::Jmp(self.trans(a, b, BlockEntry::Block(starget.block))?);
                    b.blocks[k2].term = term;
                }
            }
        }
    }
}
pub fn mangle_param(k: Id<SBlock>, i: usize) -> Ident {
    (
        Atom::new(format!("k{}p{}", k.index(), i)),
        Default::default(),
    )
}
pub fn mangle_value(a: &SFunc, v: Id<SValueW>) -> Ident {
    if let SValue::Param { block, idx, ty } = &a.cfg.values[v].0 {
        return mangle_param(*block, *idx);
    }
    return (Atom::new(format!("v{}", v.index())), Default::default());
}
