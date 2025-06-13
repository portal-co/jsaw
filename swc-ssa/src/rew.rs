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
        for (value_id, ts_type) in value.cfg.ts.clone().into_iter() {
            cfg.type_annotations.insert(mangle_value(&value, value_id), ts_type);
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
    pub fn trans(&mut self, func: &SFunc, cfg: &mut TCfg, block_entry: BlockEntry) -> anyhow::Result<Id<TBlock>> {
        loop {
            if let Some(existing_block) = self.blocks.get(&block_entry) {
                return Ok(*existing_block);
            }
            let new_block_id = cfg.blocks.alloc(Default::default());
            self.blocks.insert(block_entry.clone(), new_block_id);
            match &block_entry {
                BlockEntry::Block(block_id) => {
                    let catch_clause = match &func.cfg.blocks[*block_id].postcedent.catch {
                        crate::SCatch::Throw => TCatch::Throw,
                        crate::SCatch::Just { target } => {
                            let error = (Atom::new("$error"), Default::default());
                            cfg.decls.insert(error.clone());
                            TCatch::Jump {
                                pat: error.clone(),
                                k: self.trans(
                                    func,
                                    cfg,
                                    BlockEntry::Target(target.clone(), Some(error)),
                                )?,
                            }
                        }
                    };
                    cfg.blocks[new_block_id].catch = catch_clause;
                    for statement in func.cfg.blocks[*block_id].stmts.iter() {
                        match &func.cfg.values[*statement].0 {
                            SValue::Param { block, idx, ty } => todo!(),
                            SValue::Item { item, span } => {
                                let item_id = item.clone().map2(
                                    &mut (),
                                    &mut |_, value| anyhow::Ok(mangle_value(func, value)),
                                    &mut |_, field| field.try_into(),
                                )?;
                                cfg.blocks[new_block_id].stmts.push((
                                    LId::Id {
                                        id: mangle_value(func, *statement),
                                    },
                                    ValFlags::SSA_LIKE,
                                    item_id,
                                    span.clone().unwrap_or_else(|| Span::dummy_with_cmt()),
                                ));
                                cfg.decls.insert(mangle_value(func, *statement));
                            }
                            SValue::Assign { target, val } => {
                                let target_id = target.clone().map(&mut |value| {
                                    let mangled = mangle_value(func, value);
                                    cfg.decls.insert(mangled.clone());
                                    return anyhow::Ok(mangled);
                                })?;
                                cfg.blocks[new_block_id].stmts.push((
                                    target_id,
                                    Default::default(),
                                    Item::Just {
                                        id: mangle_value(func, *val),
                                    },
                                    Span::dummy_with_cmt(),
                                ));
                            }
                            SValue::LoadId(i) => {
                                cfg.blocks[new_block_id].stmts.push((
                                    LId::Id {
                                        id: mangle_value(func, *statement),
                                    },
                                    ValFlags::SSA_LIKE,
                                    Item::Just { id: i.clone() },
                                    Span::dummy_with_cmt(),
                                ));
                                cfg.decls.insert(mangle_value(func, *statement));
                            }
                            SValue::StoreId { target, val } => {
                                cfg.blocks[new_block_id].stmts.push((
                                    LId::Id { id: target.clone() },
                                    Default::default(),
                                    Item::Just {
                                        id: mangle_value(func, *val),
                                    },
                                    Span::dummy_with_cmt(),
                                ));
                            }
                            SValue::Benc(v) => {
                                cfg.blocks[new_block_id].stmts.push((
                                    LId::Id {
                                        id: mangle_value(func, *statement),
                                    },
                                    ValFlags::SSA_LIKE,
                                    Item::Just {
                                        id: mangle_value(func, *v),
                                    },
                                    Span::dummy_with_cmt(),
                                ));
                            }
                        }
                    }
                    let term = match &func.cfg.blocks[*block_id].postcedent.term {
                        crate::STerm::Throw(id) => swc_tac::TTerm::Throw(mangle_value(func, *id)),
                        crate::STerm::Return(id) => {
                            swc_tac::TTerm::Return(id.clone().map(|value| mangle_value(func, value)))
                        }
                        crate::STerm::Jmp(starget) => TTerm::Jmp(self.trans(
                            func,
                            cfg,
                            BlockEntry::Target(starget.clone(), None),
                        )?),
                        crate::STerm::CondJmp {
                            cond,
                            if_true,
                            if_false,
                        } => {
                            let if_true =
                                self.trans(func, cfg, BlockEntry::Target(if_true.clone(), None))?;
                            let if_false =
                                self.trans(func, cfg, BlockEntry::Target(if_false.clone(), None))?;
                            TTerm::CondJmp {
                                cond: mangle_value(func, *cond),
                                if_true,
                                if_false,
                            }
                        }
                        crate::STerm::Switch { x, blocks, default } => {
                            let default =
                                self.trans(func, cfg, BlockEntry::Target(default.clone(), None))?;
                            let blocks = blocks
                                .iter()
                                .map(|(a2, b2)| {
                                    anyhow::Ok((
                                        mangle_value(func, *a2),
                                        self.trans(func, cfg, BlockEntry::Target(b2.clone(), None))?,
                                    ))
                                })
                                .collect::<anyhow::Result<_>>()?;
                            TTerm::Switch {
                                x: mangle_value(func, *x),
                                blocks,
                                default,
                            }
                        }
                        crate::STerm::Default => swc_tac::TTerm::Default,
                    };
                    cfg.blocks[new_block_id].term = term;
                }
                BlockEntry::Target(starget, val) => {
                    let stmts = val
                        .iter()
                        .cloned()
                        .chain(starget.args.iter().map(|b| mangle_value(func, *b)))
                        .enumerate()
                        .map(|(arg_index, arg_value)| {
                            (
                                LId::Id {
                                    id: mangle_param(starget.block, arg_index),
                                },
                                Default::default(),
                                Item::Just { id: arg_value },
                                Span::dummy_with_cmt(),
                            )
                        });
                    cfg.blocks[new_block_id].stmts.extend(stmts);
                    let term =
                        swc_tac::TTerm::Jmp(self.trans(func, cfg, BlockEntry::Block(starget.block))?);
                    cfg.blocks[new_block_id].term = term;
                }
            }
        }
    }
}
pub fn mangle_param(block_id: Id<SBlock>, index: usize) -> Ident {
    (
        Atom::new(format!("k{}p{}", block_id.index(), index)),
        Default::default(),
    )
}
pub fn mangle_value(func: &SFunc, value_id: Id<SValueW>) -> Ident {
    if let SValue::Param { block, idx, ty } = &func.cfg.values[value_id].0 {
        return mangle_param(*block, *idx);
    }
    return (Atom::new(format!("v{}", value_id.index())), Default::default());
}
