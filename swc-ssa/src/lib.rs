use std::{
    collections::{BTreeMap, HashMap, HashSet},
    convert::Infallible,
    iter::once,
};

use anyhow::Context;
use id_arena::{Arena, Id};
use ssa_traits::Value;
use swc_cfg::Catch;
use swc_ecma_ast::Ident;
use swc_tac::{Item, LId, TBlock, TCfg};

pub struct SwcFunc {
    pub blocks: Arena<SBlock>,
    pub values: Arena<SValue>,
}
pub struct SBlock {
    pub params: Vec<(Id<SValue>, ())>,
    pub stmts: Vec<Id<SValue>>,
    pub term: STerm,
    pub catch: SCatch,
}
pub enum SValue {
    Param {
        block: Id<SBlock>,
        idx: usize,
        ty: (),
    },
    Item(Item<Id<SValue>>),
    Assign {
        target: LId<Id<SValue>>,
        val: Id<SValue>,
    },
    LoadId(Ident),
    StoreId {
        target: Ident,
        val: Id<SValue>,
    },
}
pub enum SCatch {
    Throw,
    Just { target: STarget },
}
pub struct STarget {
    pub block: Id<SBlock>,
    pub args: Vec<Id<SValue>>,
}
#[derive(Default)]
pub enum STerm {
    Throw(Id<SValue>),
    Return(Option<Id<SValue>>),
    Jmp(STarget),
    CondJmp {
        cond: Id<SValue>,
        if_true: STarget,
        if_false: STarget,
    },
    Switch {
        x: Id<SValue>,
        blocks: BTreeMap<Id<SValue>, STarget>,
        default: STarget,
    },
    #[default]
    Default,
}
impl SwcFunc {
    pub fn add_blockparam(&mut self, k: Id<SBlock>) -> Id<SValue> {
        let val = SValue::Param {
            block: k,
            idx: self.blocks[k].params.len(),
            ty: (),
        };
        let val = self.values.alloc(val);
        self.blocks[k].params.push((val, ()));
        return val;
    }
}
pub struct Trans {
    pub map: BTreeMap<Id<TBlock>, Id<SBlock>>,
    pub all: HashSet<Ident>,
}
impl Trans {
    pub fn apply_shim(
        &self,
        o: &mut SwcFunc,
        state: &HashMap<Ident, Id<SValue>>,
        s: &Option<(Id<SBlock>, Vec<Ident>)>,
        x: Id<SBlock>,
    ) {
        let Some((a, b)) = s else {
            o.blocks[x].catch = SCatch::Throw;
            return;
        };
        let k = SCatch::Just {
            target: STarget {
                block: *a,
                args: b.iter().filter_map(|x| state.get(x)).cloned().collect(),
            },
        };
        o.blocks[x].catch = k;
    }
    pub fn load(
        &self,
        state: &HashMap<Ident, Id<SValue>>,
        o: &mut SwcFunc,
        t: Id<SBlock>,
        a: Ident,
    ) -> Id<SValue> {
        match state.get(&a).cloned() {
            Some(b) => b,
            None => {
                let v = o.values.alloc(SValue::LoadId(a));
                o.blocks[t].stmts.push(v);
                return v;
            }
        }
    }
    pub fn trans(
        &mut self,
        i: &TCfg,
        o: &mut SwcFunc,
        k: Id<TBlock>,
    ) -> anyhow::Result<Id<SBlock>> {
        loop {
            if let Some(a) = self.map.get(&k) {
                return Ok(*a);
            }
            let mut t = o.blocks.alloc(SBlock {
                params: vec![],
                stmts: vec![],
                term: STerm::Default,
                catch: SCatch::Throw,
            });
            self.map.insert(k, t);
            let shim: Option<(Id<SBlock>, Vec<Ident>)> = match &i.blocks[k].catch {
                swc_tac::TCatch::Throw => None,
                swc_tac::TCatch::Jump { pat, k } => {
                    let a = o.blocks.alloc(SBlock {
                        params: vec![],
                        stmts: vec![],
                        term: STerm::Default,
                        catch: SCatch::Throw,
                    });
                    let mut state2 = once(pat.clone())
                        .chain(self.all.iter().filter(|a| *a != pat).cloned())
                        .collect::<Vec<_>>();
                    let mut v = state2
                        .iter()
                        .cloned()
                        .map(|a2| (a2, o.add_blockparam(a)))
                        .collect::<HashMap<_, _>>();
                    let p = self
                        .all
                        .iter()
                        .filter_map(|x| v.get(x))
                        .cloned()
                        .collect::<Vec<_>>();
                    let t = STerm::Jmp(STarget {
                        block: self.trans(i, o, *k)?,
                        args: p,
                    });
                    o.blocks[a].term = t;
                    Some((a, state2))
                }
            };
            let mut state = self
                .all
                .iter()
                .map(|a| (a.clone(), o.add_blockparam(t)))
                .collect::<HashMap<_, _>>();
            self.apply_shim(o, &state, &shim, t);
            for (a, b) in i.blocks[k].stmts.iter() {
                let b = b
                    .clone()
                    .map::<_, Infallible>(&mut |a| Ok(self.load(&state, o, t, a)))
                    .unwrap();
                let b = o.values.alloc(SValue::Item(b));
                o.blocks[t].stmts.push(b);
                match a.clone() {
                    LId::Id { id } => match state.get_mut(&id) {
                        Some(a) => {
                            *a = b;
                            let u = o.blocks.alloc(SBlock {
                                params: vec![],
                                stmts: vec![],
                                term: STerm::Default,
                                catch: SCatch::Throw,
                            });
                            self.apply_shim(o, &state, &shim, u);
                            o.blocks[t].term = STerm::Jmp(STarget {
                                block: u,
                                args: vec![],
                            });
                            t = u;
                        }
                        None => {
                            let c = o.values.alloc(SValue::StoreId {
                                target: id.clone(),
                                val: b,
                            });
                            o.blocks[t].stmts.push(c);
                        }
                    },
                    a => {
                        // let obj = self.load(&state, o, t, obj.clone());
                        // let mem = self.load(&state, o, t, mem.clone());
                        let c = a
                            .map::<_, Infallible>(&mut |a| Ok(self.load(&state, o, t, a)))
                            .unwrap();
                        let c = o.values.alloc(SValue::Assign { target: c, val: b });
                        o.blocks[t].stmts.push(c);
                    }
                };
            }
            let params = self
                .all
                .iter()
                .filter_map(|a| state.get(a))
                .cloned()
                .collect::<Vec<_>>();
            let term = match &i.blocks[k].term {
                swc_tac::TTerm::Return(ident) => match ident.as_ref() {
                    None => STerm::Return(None),
                    Some(a) => STerm::Return(Some(self.load(&state, o, t, a.clone()))),
                },
                swc_tac::TTerm::Throw(ident) => {
                    STerm::Throw(self.load(&state, o, t, ident.clone()))
                }
                swc_tac::TTerm::Jmp(id) => {
                    let id = self.trans(i, o, *id)?;
                    STerm::Jmp(STarget {
                        block: id,
                        args: params,
                    })
                }
                swc_tac::TTerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => {
                    let if_true = self.trans(i, o, *if_true)?;
                    let if_true = STarget {
                        block: if_true,
                        args: params.clone(),
                    };
                    let if_false = self.trans(i, o, *if_false)?;
                    let if_false = STarget {
                        block: if_false,
                        args: params,
                    };
                    let cond = self.load(&state, o, t, cond.clone());
                    STerm::CondJmp {
                        cond,
                        if_true,
                        if_false,
                    }
                }
                swc_tac::TTerm::Switch { x, blocks, default } => {
                    let x = self.load(&state, o, t, x.clone());
                    let blocks = blocks
                        .iter()
                        .map(|(a, b)| {
                            let c = self.trans(i, o, *b)?;
                            let d = self.load(&state, o, t, a.clone());
                            Ok((
                                d,
                                STarget {
                                    block: c,
                                    args: params.clone(),
                                },
                            ))
                        })
                        .collect::<anyhow::Result<BTreeMap<_, _>>>()?;
                    let default = self.trans(i, o, *default)?;
                    let default = STarget {
                        block: default,
                        args: params,
                    };
                    STerm::Switch { x, blocks, default }
                }
                swc_tac::TTerm::Default => STerm::Default,
            };
            o.blocks[t].term = term;
        }
    }
}
