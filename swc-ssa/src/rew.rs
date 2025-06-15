use std::{collections::BTreeMap, sync::OnceLock};

use id_arena::Id;
use swc_atoms::Atom;
use swc_common::{Mark, Span, SyntaxContext};
use swc_ecma_ast::Id as Ident;
use swc_tac::{Item, LId, TBlock, TCatch, TCfg, TFunc, TStmt, TTerm, ValFlags};

use crate::{SBlock, SFunc, STarget, SValue, SValueW};

impl TryFrom<SFunc> for TFunc {
    type Error = anyhow::Error;

    fn try_from(value: SFunc) -> Result<Self, Self::Error> {
        let mut cfg = TCfg::default();
        cfg.decls.extend(value.cfg.decls.iter().cloned());
        let mut rew = Rew {
            blocks: Default::default(),
            ctxt: Default::default(),
        };
        let entry = rew.trans(&value, &mut cfg, BlockEntry::Block(value.entry))?;
        let ctxt = rew
            .ctxt
            .get_or_init(|| SyntaxContext::empty().apply_mark(Mark::new()))
            .clone();
        let params = value.cfg.blocks[value.entry]
            .params
            .iter()
            .map(|v| mangle_value(ctxt, &value, v.0))
            .collect();
        for (value_id, ts_type) in value.cfg.ts.clone().into_iter() {
            cfg.type_annotations
                .insert(mangle_value(ctxt, &value, value_id), ts_type);
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
    pub ctxt: OnceLock<SyntaxContext>,
}

#[derive(Clone, Ord, PartialEq, PartialOrd, Eq)]
pub enum BlockEntry {
    Block(Id<SBlock>),
    Target(STarget, Option<Ident>),
}
impl Rew {
    pub fn trans(
        &mut self,
        func: &SFunc,
        cfg: &mut TCfg,
        block_entry: BlockEntry,
    ) -> anyhow::Result<Id<TBlock>> {
        let ctxt = self
            .ctxt
            .get_or_init(|| SyntaxContext::empty().apply_mark(Mark::new()))
            .clone();
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
                        match &func.cfg.values[*statement].value {
                            SValue::Param { block, idx, ty } => todo!(),
                            SValue::Item { item, span } => {
                                let item_id = item.clone().map2(
                                    &mut (),
                                    &mut |_, value| anyhow::Ok(mangle_value(ctxt, func, value)),
                                    &mut |_, field| field.try_into(),
                                )?;
                                cfg.blocks[new_block_id].stmts.push(TStmt {
                                    left: LId::Id {
                                        id: mangle_value(ctxt, func, *statement),
                                    },
                                    flags: ValFlags::SSA_LIKE,
                                    right: item_id,
                                    span: span.clone().unwrap_or_else(|| Span::dummy_with_cmt()),
                                });
                                cfg.decls.insert(mangle_value(ctxt, func, *statement));
                            }
                            SValue::Assign { target, val } => {
                                let target_id = target.clone().map(&mut |value| {
                                    let mangled = mangle_value(ctxt, func, value);
                                    cfg.decls.insert(mangled.clone());
                                    return anyhow::Ok(mangled);
                                })?;
                                cfg.blocks[new_block_id].stmts.push(TStmt {
                                    left: target_id,
                                    flags: Default::default(),
                                    right: Item::Just {
                                        id: mangle_value(ctxt, func, *val),
                                    },
                                    span: Span::dummy_with_cmt(),
                                });
                            }
                            SValue::LoadId(i) => {
                                cfg.blocks[new_block_id].stmts.push(TStmt {
                                    left: LId::Id {
                                        id: mangle_value(ctxt, func, *statement),
                                    },
                                    flags: ValFlags::SSA_LIKE,
                                    right: Item::Just { id: i.clone() },
                                    span: Span::dummy_with_cmt(),
                                });
                                cfg.decls.insert(mangle_value(ctxt, func, *statement));
                            }
                            SValue::StoreId { target, val } => {
                                cfg.blocks[new_block_id].stmts.push(TStmt {
                                    left: LId::Id { id: target.clone() },
                                    flags: Default::default(),
                                    right: Item::Just {
                                        id: mangle_value(ctxt, func, *val),
                                    },
                                    span: Span::dummy_with_cmt(),
                                });
                            }
                            SValue::Benc(v) => {
                                cfg.blocks[new_block_id].stmts.push(TStmt {
                                    left: LId::Id {
                                        id: mangle_value(ctxt, func, *statement),
                                    },
                                    flags: ValFlags::SSA_LIKE,
                                    right: Item::Just {
                                        id: mangle_value(ctxt, func, *v),
                                    },
                                    span: Span::dummy_with_cmt(),
                                });
                            }
                        }
                    }
                    let term = match &func.cfg.blocks[*block_id].postcedent.term {
                        crate::STerm::Throw(id) => {
                            swc_tac::TTerm::Throw(mangle_value(ctxt, func, *id))
                        }
                        crate::STerm::Return(id) => swc_tac::TTerm::Return(
                            id.clone().map(|value| mangle_value(ctxt, func, value)),
                        ),
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
                                cond: mangle_value(ctxt, func, *cond),
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
                                        mangle_value(ctxt, func, *a2),
                                        self.trans(
                                            func,
                                            cfg,
                                            BlockEntry::Target(b2.clone(), None),
                                        )?,
                                    ))
                                })
                                .collect::<anyhow::Result<_>>()?;
                            TTerm::Switch {
                                x: mangle_value(ctxt, func, *x),
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
                        .chain(starget.args.iter().map(|b| mangle_value(ctxt, func, *b)))
                        .enumerate()
                        .map(|(arg_index, arg_value)| TStmt {
                            left: LId::Id {
                                id: mangle_param(ctxt, starget.block, arg_index),
                            },
                            flags: Default::default(),
                            right: Item::Just { id: arg_value },
                            span: Span::dummy_with_cmt(),
                        });
                    cfg.blocks[new_block_id].stmts.extend(stmts);
                    let term = swc_tac::TTerm::Jmp(self.trans(
                        func,
                        cfg,
                        BlockEntry::Block(starget.block),
                    )?);
                    cfg.blocks[new_block_id].term = term;
                }
            }
        }
    }
}
pub fn mangle_param(ctxt: SyntaxContext, block_id: Id<SBlock>, index: usize) -> Ident {
    (Atom::new(format!("k{}p{}", block_id.index(), index)), ctxt)
}
pub fn mangle_value(ctxt: SyntaxContext, func: &SFunc, value_id: Id<SValueW>) -> Ident {
    if let SValue::Param { block, idx, ty } = &func.cfg.values[value_id].value {
        return mangle_param(ctxt, *block, *idx);
    }
    return (Atom::new(format!("v{}", value_id.index())), ctxt);
}
