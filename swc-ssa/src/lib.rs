use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    default,
    iter::{empty, once},
    mem::take,
};

use anyhow::Context;
use cfg_traits::Term;
use id_arena::{Arena, Id};
use ssa_traits::Value;
use swc_atoms::Atom;
use swc_cfg::Catch;
use swc_common::Span;
use swc_ecma_ast::{Id as Ident, Lit, Null, TsType, TsTypeAnn, TsTypeParamDecl};
use swc_tac::{Item, LId, TBlock, TCallee, TCfg, TFunc, ValFlags};
pub mod ch;
pub mod idw;
pub mod impls;
pub mod rew;
pub mod simplify;

pub fn benj(a: &mut SwcFunc) {
    for ki in a.blocks.iter().map(|a| a.0).collect::<Vec<_>>() {
        let mut t = take(&mut a.blocks[ki].postcedent);
        for r in t.targets_mut() {
            if r.block.index() <= ki.index() {
                for w in r.args.iter_mut() {
                    let v = a.values.alloc(SValueW(SValue::Benc(*w)));
                    a.blocks[ki].stmts.push(v);
                    *w = v;
                }
            }
        }
        a.blocks[ki].postcedent = t;
    }
}

pub struct SFunc {
    pub cfg: SwcFunc,
    pub entry: Id<SBlock>,
    pub is_generator: bool,
    pub is_async: bool,
    pub ts_params: Vec<Option<TsType>>,
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
            ts: Default::default(),
            decls: d,
            generics: None,
            ts_retty: None,
        };
        let entry2 = cfg.blocks.alloc(Default::default());
        let params = value
            .params
            .iter()
            .cloned()
            .map(|a| (a, cfg.add_blockparam(entry2)))
            .collect::<BTreeMap<_, _>>();
        let undef = cfg.values.alloc(
            SValue::Item {
                item: Item::Undef,
                span: None,
            }
            .into(),
        );
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
        cfg.generics = value.cfg.generics;
        cfg.ts_retty = value.cfg.ts_retty;
        Ok(Self {
            cfg,
            entry: entry2,
            is_generator: value.is_generator,
            is_async: value.is_async,
            ts_params: value.ts_params,
        })
    }
}
#[derive(Default)]
pub struct SwcFunc {
    pub blocks: Arena<SBlock>,
    pub values: Arena<SValueW>,
    pub ts: BTreeMap<Id<SValueW>, TsType>,
    pub decls: BTreeSet<Ident>,
    pub generics: Option<TsTypeParamDecl>,
    pub ts_retty: Option<TsTypeAnn>,
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
#[non_exhaustive]
pub enum SValue<I = Id<SValueW>, B = Id<SBlock>> {
    Param { block: B, idx: usize, ty: () },
    Item { item: Item<I>, span: Option<Span> },
    Assign { target: LId<I>, val: I },
    LoadId(Ident),
    StoreId { target: Ident, val: I },
    Benc(I),
}
impl<I: Copy, B> SValue<I, B> {
    pub fn vals<'a>(&'a self) -> Box<dyn Iterator<Item = I> + 'a> {
        match self {
            SValue::Param { block, idx, ty } => Box::new(empty()),
            SValue::Item { item, span } => Box::new(item.refs().map(|a| *a)),
            SValue::Assign { target, val } => {
                let v = once(*val);
                let w: Box<dyn Iterator<Item = &I> + '_> = match target {
                    swc_tac::LId::Id { id } => todo!(),
                    swc_tac::LId::Member { obj, mem } => Box::new([obj, &mem[0]].into_iter()),
                };
                Box::new(v.chain(w.cloned()))
            }
            SValue::LoadId(_) => Box::new(empty()),
            SValue::StoreId { target, val } => Box::new(once(*val)),
            SValue::Benc(a) => Box::new(once(*a)),
        }
    }
}
impl<I, B> SValue<I, B> {
    pub fn vals_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut I> + 'a> {
        match self {
            SValue::Param { block, idx, ty } => Box::new(empty()),
            SValue::Item { item, span } => item.refs_mut(),
            SValue::Assign { target, val } => {
                let v = once(val);
                let w: Box<dyn Iterator<Item = &mut I> + '_> = match target {
                    swc_tac::LId::Id { id } => todo!(),
                    swc_tac::LId::Member { obj, mem } => Box::new([obj, &mut mem[0]].into_iter()),
                };
                Box::new(v.chain(w))
            }
            SValue::LoadId(_) => Box::new(empty()),
            SValue::StoreId { target, val } => Box::new(once(val)),
            SValue::Benc(a) => Box::new(once(a)),
        }
    }
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
#[non_exhaustive]
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
#[non_exhaustive]
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

#[non_exhaustive]
pub struct Trans {
    pub map: BTreeMap<Id<TBlock>, Id<SBlock>>,
    pub all: BTreeSet<Ident>,
    pub undef: Id<SValueW>,
}
impl Trans {
    pub fn from_undef(undef: Id<SValueW>) -> Self {
        Self {
            map: Default::default(),
            all: Default::default(),
            undef: undef,
        }
    }
    pub fn apply_shim(
        &self,
        o: &mut SwcFunc,
        state: &BTreeMap<Ident, (Id<SValueW>, ValFlags)>,
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
                args: b
                    .iter()
                    .filter_map(|x| state.get(x))
                    .map(|a| &a.0)
                    .cloned()
                    .collect(),
            },
        };
        o.blocks[x].postcedent.catch = k;
    }
    pub fn load(
        &self,
        state: &BTreeMap<Ident, (Id<SValueW>, ValFlags)>,
        i: &TCfg,
        o: &mut SwcFunc,
        t: Id<SBlock>,
        a: Ident,
        cache: &BTreeMap<Ident, Id<SValueW>>,
    ) -> Id<SValueW> {
        if let Some(k) = cache.get(&a) {
            return *k;
        }
        let x = match state.get(&a).cloned() {
            Some(b) => b.0,
            None => {
                let v = o.values.alloc(SValue::LoadId(a).into());
                o.blocks[t].stmts.push(v);
                return v;
            }
        };
        if let Some(ty) = i.type_annotations.get(&a).cloned() {
            o.ts.insert(x, ty);
        };
        x
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
                .map(|a| (a.clone(), (o.add_blockparam(t), ValFlags::all())))
                .collect::<BTreeMap<_, _>>();
            self.apply_shim(o, &state, &shim, t);
            let mut cache = BTreeMap::new();
            for (a, flags, b, s) in i.blocks[k].stmts.iter() {
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
                    .map::<_, Infallible>(&mut |a| Ok(self.load(&state, i, o, t, a, &cache)))
                    .unwrap();
                let b = o.values.alloc(
                    SValue::Item {
                        item: b,
                        span: Some(*s),
                    }
                    .into(),
                );
                o.blocks[t].stmts.push(b);
                let flags = match a.clone() {
                    LId::Id { id } => match state.get_mut(&id) {
                        Some((a, f)) => {
                            *f &= *flags;
                            let f = *f;
                            *a = b;
                            if !f.contains(ValFlags::SSA_LIKE) {
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
                            Some(f)
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
                            None
                        }
                    },
                    a => {
                        // let obj = self.load(&state, o, t, obj.clone());
                        // let mem = self.load(&state, o, t, mem.clone());
                        let c = a
                            .map::<_, Infallible>(
                                &mut |a| Ok(self.load(&state, i, o, t, a, &cache)),
                            )
                            .unwrap();
                        let c = o.values.alloc(SValue::Assign { target: c, val: b }.into());
                        o.blocks[t].stmts.push(c);
                        None
                    }
                };
            }
            let params = self
                .all
                .iter()
                .filter_map(|a| state.get(a))
                .map(|a| &a.0)
                .cloned()
                .collect::<Vec<_>>();
            let term = match &i.blocks[k].term {
                swc_tac::TTerm::Return(ident) => match ident.as_ref() {
                    None => STerm::Return(None),
                    Some(a) => STerm::Return(Some(self.load(&state, i, o, t, a.clone(), &cache))),
                },
                swc_tac::TTerm::Throw(ident) => {
                    STerm::Throw(self.load(&state, i, o, t, ident.clone(), &cache))
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
                    let cond = self.load(&state, i, o, t, cond.clone(), &cache);
                    STerm::CondJmp {
                        cond,
                        if_true,
                        if_false,
                    }
                }
                swc_tac::TTerm::Switch { x, blocks, default } => {
                    let x = self.load(&state, i, o, t, x.clone(), &cache);
                    let blocks = blocks
                        .iter()
                        .map(|(a, b)| {
                            let c = self.trans(i, o, *b)?;
                            let d = self.load(&state, i, o, t, a.clone(), &cache);
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
