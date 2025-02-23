use std::collections::{BTreeMap, HashMap};

use id_arena::Id;

use crate::{Block, Catch, Cfg, Ctx, Term};
#[derive(Default)]
pub struct Recfg {
    pub map: BTreeMap<Id<Block>, Id<Block>>,
}
impl Recfg {
    pub fn go(&mut self, i: &Cfg, o: &mut Cfg, k: Id<Block>) -> anyhow::Result<Id<Block>> {
        loop {
            if let Some(a) = self.map.get(&k) {
                return Ok(*a);
            }
            let l = o.blocks.alloc(Default::default());
            o.blocks[l].end.orig_span = i.blocks[k].end.orig_span.clone();
            self.map.insert(k, l);
            let catch = match &i.blocks[k].end.catch {
                crate::Catch::Throw => Catch::Throw,
                crate::Catch::Jump { pat, k } => Catch::Jump {
                    pat: pat.clone(),
                    k: self.go(i, o, *k)?,
                },
            };
            o.blocks[l].end.catch = catch.clone();
            let mut ctx = Ctx {
                catch,
                ..Default::default()
            };
            let l = ctx.transform_all(o, i.blocks[k].stmts.clone(), l)?;
            let term = match &i.blocks[k].end.term {
                crate::Term::Jmp(id) => Term::Jmp(self.go(i, o, *id)?),
                crate::Term::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => Term::CondJmp {
                    cond: cond.clone(),
                    if_true: self.go(i, o, *if_true)?,
                    if_false: self.go(i, o, *if_false)?,
                },
                crate::Term::Switch { x, blocks, default } => Term::Switch {
                    x: x.clone(),
                    blocks: blocks
                        .iter()
                        .map(|(a, b)| Ok((a.clone(), self.go(i, o, *b)?)))
                        .collect::<anyhow::Result<HashMap<_, _>>>()?,
                    default: self.go(i, o, *default)?,
                },
                a => a.clone(),
            };
            o.blocks[l].end.term = term;
        }
    }
}
