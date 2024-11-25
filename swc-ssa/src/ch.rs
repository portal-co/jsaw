use std::collections::HashMap;

use swc_ecma_ast::Expr;
use swc_ecma_utils::{ExprExt, Value};

use crate::*;
pub struct CH {
    pub all: BTreeMap<Id<SBlock>, HashMap<(Vec<Option<Lit>>), Id<SBlock>>>,
}
pub fn ch(a: &SFunc) -> anyhow::Result<SFunc> {
    let mut n = SwcFunc::default();
    let entry = CH {
        all: BTreeMap::new(),
    }
    .init(&a.cfg, &mut n, a.entry)?;
    n.decls.extend(a.cfg.decls.clone().into_iter());
    return Ok(SFunc {
        cfg: n,
        entry,
        is_generator: a.is_generator,
        is_async: a.is_async,
    });
}
impl CH {
    pub fn init(
        &mut self,
        inp: &SwcFunc,
        out: &mut SwcFunc,
        k: Id<SBlock>,
    ) -> anyhow::Result<Id<SBlock>> {
        let lits = inp.blocks[k].params.iter().map(|_| None).collect();
        return self.go(inp, out, k, lits, &Default::default());
    }
    pub fn go(
        &mut self,
        inp: &SwcFunc,
        out: &mut SwcFunc,
        k: Id<SBlock>,
        lits: Vec<Option<Lit>>,
        lsk: &BTreeSet<Id<SBlock>>,
    ) -> anyhow::Result<Id<SBlock>> {
        let is_loop = lsk.contains(&k);
        let mut lsk = lsk.clone();
        lsk.insert(k);
        loop {
            if let Some(x) = self.all.get(&k).and_then(|x| x.get(&lits)) {
                return Ok(*x);
            }
            let n = out.blocks.alloc(Default::default());
            self.all.entry(k).or_default().insert(lits.clone(), n);
            let mut params = inp.blocks[k]
                .params
                .iter()
                .map(|a| a.0)
                .zip(lits.iter())
                .map(|(a, l)| {
                    (
                        a,
                        match l {
                            Some(l) => {
                                let v =
                                    out.values.alloc(SValue::Item(Item::Lit { lit: l.clone() }));
                                out.blocks[n].stmts.push(v);
                                v
                            }
                            None => out.add_blockparam(n),
                        },
                    )
                })
                .collect::<BTreeMap<_, _>>();
            for s in inp.blocks[k].stmts.iter().cloned() {
                let v =
                    match inp.values[s].clone() {
                        SValue::Param { block, idx, ty } => todo!(),
                        SValue::Item(item) => SValue::Item(item.map(&mut |a| {
                            params.get(&a).cloned().context("in getting a variable")
                        })?),
                        SValue::Assign { target, val } => SValue::Assign {
                            target: target.map(&mut |a| {
                                params.get(&a).cloned().context("in getting a variable")
                            })?,
                            val: params.get(&val).cloned().context("in getting a variable")?,
                        },
                        SValue::LoadId(i) => SValue::LoadId(i),
                        SValue::StoreId { target, val } => SValue::StoreId {
                            target,
                            val: params.get(&val).cloned().context("in getting a variable")?,
                        },
                    };
                let v = if is_loop {
                    v
                } else {
                    match v.const_in(out) {
                        None => v,
                        Some(a) => SValue::Item(Item::Lit { lit: a }),
                    }
                };
                let v = out.values.alloc(v);
                out.blocks[n].stmts.push(v);
                params.insert(s, v);
            }
            let tgt = |this: &mut Self,
                       inp: &SwcFunc,
                       out: &mut SwcFunc,
                       t: &STarget,
                       p: usize|
             -> anyhow::Result<STarget> {
                let mut funcs = (0..p).map(|_| None).collect::<Vec<_>>();
                let args = t
                    .args
                    .iter()
                    .filter_map(|b| params.get(b))
                    .filter_map(|b| {
                        'a: {
                            if let SValue::Item(Item::Lit { lit }) = &out.values[*b] {
                                funcs.push(Some(lit.clone()));
                                return None;
                            };
                        };
                        funcs.push(None);
                        return Some(b);
                    })
                    .cloned()
                    .collect();
                anyhow::Ok(STarget {
                    block: this.go(inp, out, t.block, funcs, &lsk)?,
                    args,
                })
            };
            let catch = match &inp.blocks[k].postcedent.catch {
                SCatch::Throw => SCatch::Throw,
                SCatch::Just { target } => SCatch::Just {
                    target: tgt(self, inp, out, target, 1)?,
                },
            };
            out.blocks[n].postcedent.catch = catch;
            let term = match &inp.blocks[k].postcedent.term {
                STerm::Throw(id) => {
                    STerm::Throw(params.get(id).cloned().context("in getting a variable")?)
                }
                STerm::Return(id) => STerm::Return(match id.as_ref() {
                    None => None,
                    Some(val) => Some(params.get(&val).cloned().context("in getting a variable")?),
                }),
                STerm::Jmp(starget) => STerm::Jmp(tgt(self, inp, out, starget, 0)?),
                STerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => {
                    let cond = params.get(cond).cloned().context("in getting the cond")?;
                    match &out.values[cond] {
                        SValue::Item(Item::Lit { lit }) => {
                            match Expr::Lit(lit.clone()).as_pure_bool(&Default::default()) {
                                Value::Known(k) => STerm::Jmp(tgt(
                                    self,
                                    inp,
                                    out,
                                    if k { if_true } else { if_false },
                                    0,
                                )?),
                                _ => STerm::CondJmp {
                                    cond: cond,
                                    if_true: tgt(self, inp, out, if_true, 0)?,
                                    if_false: tgt(self, inp, out, if_false, 0)?,
                                },
                            }
                        }
                        _ => STerm::CondJmp {
                            cond: cond,
                            if_true: tgt(self, inp, out, if_true, 0)?,
                            if_false: tgt(self, inp, out, if_false, 0)?,
                        },
                    }
                }
                STerm::Switch { x, blocks, default } => STerm::Switch {
                    x: params.get(x).cloned().context("in getting the value")?,
                    blocks: blocks
                        .iter()
                        .map(|(val, t)| {
                            anyhow::Ok((
                                params.get(&val).cloned().context("in getting a variable")?,
                                tgt(self, inp, out, t, 0)?,
                            ))
                        })
                        .collect::<anyhow::Result<_>>()?,
                    default: tgt(self, inp, out, default, 0)?,
                },
                STerm::Default => STerm::Default,
            };
            out.blocks[n].postcedent.term = term;
        }
    }
}
