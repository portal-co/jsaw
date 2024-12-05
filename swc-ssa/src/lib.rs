use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    default,
    iter::once,
};

use anyhow::Context;
use id_arena::{Arena, Id};
use ssa_traits::Value;
use swc_atoms::Atom;
use swc_cfg::Catch;
use swc_common::Span;
use swc_ecma_ast::{Id as Ident, Lit, Null};
use swc_tac::{Item, LId, TBlock, TCallee, TCfg, TFunc};
pub mod ch;
pub mod impls;
pub mod rew;
pub mod simplify;

pub struct SFunc {
    pub cfg: SwcFunc,
    pub entry: Id<SBlock>,
    pub is_generator: bool,
    pub is_async: bool,
}
impl TryFrom<TFunc> for SFunc {
    type Error = anyhow::Error;

    fn try_from(value: TFunc) -> Result<Self, Self::Error> {
        let mut decls = value.cfg.decls.clone();
        let mut d = BTreeSet::new();
        for e in value.cfg.externs().collect::<BTreeSet<_>>() {
            decls.remove(&e);
            d.insert(e);
        }
        let mut cfg = SwcFunc {
            blocks: Default::default(),
            values: Default::default(),
            decls: d,
        };
        let entry2 = cfg.blocks.alloc(Default::default());
        let params = value
            .params
            .iter()
            .cloned()
            .map(|a| (a, cfg.add_blockparam(entry2)))
            .collect::<BTreeMap<_, _>>();
        let undef = cfg.values.alloc(SValue::Item(Item::Undef).into());
        let mut trans = Trans {
            map: BTreeMap::new(),
            all: decls.clone(),
            undef,
        };
        let entry = trans.trans(&value.cfg, &mut cfg, value.entry)?;
        let target = STarget {
            block: entry,
            args: decls
                .iter()
                .cloned()
                .map(|a| match params.get(&a) {
                    Some(v) => *v,
                    None => undef,
                })
                .collect(),
        };
        cfg.blocks[entry2].postcedent.term = STerm::Jmp(target);

        Ok(Self {
            cfg,
            entry: entry2,
            is_generator: value.is_generator,
            is_async: value.is_async,
        })
    }
}
#[derive(Default)]
pub struct SwcFunc {
    pub blocks: Arena<SBlock>,
    pub values: Arena<SValueW>,
    pub decls: BTreeSet<Ident>,
}
#[derive(Default)]
pub struct SBlock {
    pub params: Vec<(Id<SValueW>, ())>,
    pub stmts: Vec<Id<SValueW>>,
    pub postcedent: SPostcedent,
}
#[derive(Clone)]
pub struct SPostcedent<I = Id<SValueW>, B = Id<SBlock>> {
    pub term: STerm<I, B>,
    pub catch: SCatch<I, B>,
}
impl<I, B> Default for SPostcedent<I, B> {
    fn default() -> Self {
        Self {
            term: Default::default(),
            catch: Default::default(),
        }
    }
}
#[derive(Clone)]
pub enum SValue<I = Id<SValueW>, B = Id<SBlock>> {
    Param { block: B, idx: usize, ty: () },
    Item(Item<I>),
    Assign { target: LId<I>, val: I },
    LoadId(Ident),
    StoreId { target: Ident, val: I },
}
#[repr(transparent)]
#[derive(Clone)]
pub struct SValueW(pub SValue);
impl From<SValue> for SValueW {
    fn from(value: SValue) -> Self {
        Self(value)
    }
}
impl From<SValueW> for SValue {
    fn from(value: SValueW) -> Self {
        value.0
    }
}
#[derive(Clone)]
pub enum SCatch<I = Id<SValueW>, B = Id<SBlock>> {
    Throw,
    Just { target: STarget<I, B> },
}
impl<I, B> Default for SCatch<I, B> {
    fn default() -> Self {
        Self::Throw
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct STarget<I = Id<SValueW>, B = Id<SBlock>> {
    pub block: B,
    pub args: Vec<I>,
}
#[derive(Clone)]
pub enum STerm<I = Id<SValueW>, B = Id<SBlock>> {
    Throw(I),
    Return(Option<I>),
    Jmp(STarget<I, B>),
    CondJmp {
        cond: I,
        if_true: STarget<I, B>,
        if_false: STarget<I, B>,
    },
    Switch {
        x: I,
        blocks: Vec<(I, STarget<I, B>)>,
        default: STarget<I, B>,
    },
    Default,
}
impl<I, B> Default for STerm<I, B> {
    fn default() -> Self {
        Self::Default
    }
}
impl SwcFunc {
    pub fn add_blockparam(&mut self, k: Id<SBlock>) -> Id<SValueW> {
        let val = SValue::Param {
            block: k,
            idx: self.blocks[k].params.len(),
            ty: (),
        };
        let val = self.values.alloc(val.into());
        self.blocks[k].params.push((val, ()));
        return val;
    }
}
pub struct Trans {
    pub map: BTreeMap<Id<TBlock>, Id<SBlock>>,
    pub all: BTreeSet<Ident>,
    pub undef: Id<SValueW>,
}
impl Trans {
    pub fn apply_shim(
        &self,
        o: &mut SwcFunc,
        state: &BTreeMap<Ident, Id<SValueW>>,
        s: &Option<(Id<SBlock>, Vec<Ident>)>,
        x: Id<SBlock>,
    ) {
        let Some((a, b)) = s else {
            o.blocks[x].postcedent.catch = SCatch::Throw;
            return;
        };
        let k = SCatch::Just {
            target: STarget {
                block: *a,
                args: b.iter().filter_map(|x| state.get(x)).cloned().collect(),
            },
        };
        o.blocks[x].postcedent.catch = k;
    }
    pub fn load(
        &self,
        state: &BTreeMap<Ident, Id<SValueW>>,
        o: &mut SwcFunc,
        t: Id<SBlock>,
        a: Ident,
        cache: &BTreeMap<Ident, Id<SValueW>>,
    ) -> Id<SValueW> {
        if let Some(k) = cache.get(&a) {
            return *k;
        }
        match state.get(&a).cloned() {
            Some(b) => b,
            None => {
                let v = o.values.alloc(SValue::LoadId(a).into());
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
                postcedent: SPostcedent::default(),
            });
            self.map.insert(k, t);
            let shim: Option<(Id<SBlock>, Vec<Ident>)> = match &i.blocks[k].catch {
                swc_tac::TCatch::Throw => None,
                swc_tac::TCatch::Jump { pat, k } => {
                    let a = o.blocks.alloc(SBlock {
                        params: vec![],
                        stmts: vec![],
                        postcedent: SPostcedent::default(),
                    });
                    let mut state2 = once(pat.clone())
                        .chain(self.all.iter().filter(|a| *a != pat).cloned())
                        .collect::<Vec<_>>();
                    let mut v = state2
                        .iter()
                        .cloned()
                        .map(|a2| (a2, o.add_blockparam(a)))
                        .collect::<BTreeMap<_, _>>();
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
                    o.blocks[a].postcedent.term = t;
                    Some((a, state2))
                }
            };
            let mut state = self
                .all
                .iter()
                .map(|a| (a.clone(), o.add_blockparam(t)))
                .collect::<BTreeMap<_, _>>();
            self.apply_shim(o, &state, &shim, t);
            let mut cache = BTreeMap::new();
            for (a, b) in i.blocks[k].stmts.iter() {
                let mut b = b.clone();
                if let Item::Call { callee, args } = &mut b {
                    if let TCallee::Val(v) = callee {
                        if !i.blocks.iter().any(|k| {
                            k.1.stmts.iter().any(|a| match &a.0 {
                                LId::Id { id } => id == v,
                                _ => false,
                            })
                        }) {
                            *callee = TCallee::Static(v.clone());
                        }
                    }
                }
                let b = b
                    .map::<_, Infallible>(&mut |a| Ok(self.load(&state, o, t, a, &cache)))
                    .unwrap();
                let b = o.values.alloc(SValue::Item(b).into());
                o.blocks[t].stmts.push(b);
                match a.clone() {
                    LId::Id { id } => match state.get_mut(&id) {
                        Some(a) => {
                            *a = b;
                            let u = o.blocks.alloc(SBlock {
                                params: vec![],
                                stmts: vec![],
                                postcedent: Default::default(),
                            });
                            self.apply_shim(o, &state, &shim, u);
                            o.blocks[t].postcedent.term = STerm::Jmp(STarget {
                                block: u,
                                args: vec![],
                            });
                            t = u;
                        }
                        None => {
                            cache.insert(id.clone(), b);
                            let c = o.values.alloc(
                                SValue::StoreId {
                                    target: id.clone(),
                                    val: b,
                                }
                                .into(),
                            );
                            o.blocks[t].stmts.push(c);
                        }
                    },
                    a => {
                        // let obj = self.load(&state, o, t, obj.clone());
                        // let mem = self.load(&state, o, t, mem.clone());
                        let c = a
                            .map::<_, Infallible>(&mut |a| Ok(self.load(&state, o, t, a, &cache)))
                            .unwrap();
                        let c = o.values.alloc(SValue::Assign { target: c, val: b }.into());
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
                    Some(a) => STerm::Return(Some(self.load(&state, o, t, a.clone(), &cache))),
                },
                swc_tac::TTerm::Throw(ident) => {
                    STerm::Throw(self.load(&state, o, t, ident.clone(), &cache))
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
                    let cond = self.load(&state, o, t, cond.clone(), &cache);
                    STerm::CondJmp {
                        cond,
                        if_true,
                        if_false,
                    }
                }
                swc_tac::TTerm::Switch { x, blocks, default } => {
                    let x = self.load(&state, o, t, x.clone(), &cache);
                    let blocks = blocks
                        .iter()
                        .map(|(a, b)| {
                            let c = self.trans(i, o, *b)?;
                            let d = self.load(&state, o, t, a.clone(), &cache);
                            Ok((
                                d,
                                STarget {
                                    block: c,
                                    args: params.clone(),
                                },
                            ))
                        })
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    let default = self.trans(i, o, *default)?;
                    let default = STarget {
                        block: default,
                        args: params,
                    };
                    STerm::Switch { x, blocks, default }
                }
                swc_tac::TTerm::Default => STerm::Default,
            };
            o.blocks[t].postcedent.term = term;
        }
    }
}
