use std::collections::BTreeMap;

use id_arena::Id;
use swc_atoms::Atom;
use swc_ecma_ast::Id as Ident;
use swc_tac::{Item, LId, TBlock, TCatch, TCfg, TFunc, TTerm};

use crate::{SBlock, SFunc, STarget, SValue};

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
        Ok(Self {
            cfg,
            entry,
            params,
            is_generator: value.is_generator,
            is_async: value.is_async,
        })
    }
}

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
                        match &a.cfg.values[*val] {
                            SValue::Param { block, idx, ty } => todo!(),
                            SValue::Item(item) => {
                                let i =
                                    item.clone().map(&mut |v| anyhow::Ok(mangle_value(a, v)))?;
                                b.blocks[k2].stmts.push((
                                    LId::Id {
                                        id: mangle_value(a, *val),
                                    },
                                    i,
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
                                    Item::Just {
                                        id: mangle_value(a, *val),
                                    },
                                ));
                            }
                            SValue::LoadId(i) => {
                                b.blocks[k2].stmts.push((
                                    LId::Id {
                                        id: mangle_value(a, *val),
                                    },
                                    Item::Just { id: i.clone() },
                                ));
                                b.decls.insert(mangle_value(a, *val));
                            }
                            SValue::StoreId { target, val } => {
                                b.blocks[k2].stmts.push((
                                    LId::Id { id: target.clone() },
                                    Item::Just {
                                        id: mangle_value(a, *val),
                                    },
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
                                Item::Just { id: b },
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
pub fn mangle_value(a: &SFunc, v: Id<SValue>) -> Ident {
    if let SValue::Param { block, idx, ty } = &a.cfg.values[v] {
        return mangle_param(*block, *idx);
    }
    return (Atom::new(format!("v{}", v.index())), Default::default());
}
