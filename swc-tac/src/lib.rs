use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::Infallible;
use std::default;
use std::iter::{empty, once};

use anyhow::Context;
use arena_traits::{Arena as TArena, IndexAlloc};
use bitflags::bitflags;
use id_arena::{Arena, Id};
use lam::LAM;
use linearize::{StaticMap, static_map};
use portal_jsc_common::{Asm, Native};
use portal_jsc_swc_util::{ImportMapper, NoImportMapper, ResolveNatives};
use swc_atoms::Atom;
use swc_cfg::{Block, Catch, Cfg, Func};
use swc_common::pass::Either;
use swc_common::{Span, Spanned};
use swc_ecma_ast::{
    AssignOp, BinaryOp, Callee, Expr, ExprOrSpread, Function, Lit, MemberExpr, MemberProp, Number,
    Pat, SimpleAssignTarget, Stmt, Str, TsType, TsTypeAnn, TsTypeParamDecl, UnaryOp,
};

use swc_ecma_ast::Id as Ident;

pub mod lam;
pub mod rew;
pub type LId = portal_jsc_common::LId<Ident>;

#[cfg(feature = "simpl")]
pub mod simpl;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, linearize::Linearize)]
#[non_exhaustive]
pub enum ImportMapperReq {
    Native,
}

pub fn imp(a: MemberProp) -> Expr {
    match a {
        swc_ecma_ast::MemberProp::Ident(ident_name) => {
            let e = Expr::Lit(Lit::Str(Str {
                span: ident_name.span,
                value: ident_name.sym,
                raw: None,
            }));
            e
        }
        swc_ecma_ast::MemberProp::PrivateName(private_name) => {
            todo!()
        }
        swc_ecma_ast::MemberProp::Computed(computed_prop_name) => *computed_prop_name.expr,
    }
}
bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
    pub struct ValFlags: u64{
        const SSA_LIKE = 0x1;
    }
}
#[derive(Clone, Debug)]
pub struct TFunc {
    pub cfg: TCfg,
    pub entry: Id<TBlock>,
    pub params: Vec<Ident>,
    pub ts_params: Vec<Option<TsType>>,
    pub is_generator: bool,
    pub is_async: bool,
}
impl TFunc {
    pub fn try_from_with_mapper<'a>(
        value: Func,
        import_mapper: StaticMap<ImportMapperReq, Option<&'a (dyn ImportMapper + 'a)>>,
    ) -> anyhow::Result<Self> {
        let mut cfg = TCfg::default();
        let entry = Trans {
            map: BTreeMap::new(),
            ret_to: None,
            recatch: TCatch::Throw,
            this: None,
            import_mapper,
        }
        .trans(&value.cfg, &mut cfg, value.entry)?;
        cfg.ts_retty = value.cfg.ts_retty;
        cfg.generics = value.cfg.generics;
        let mut ts_params = vec![];
        let params = value
            .params
            .iter()
            .filter_map(|x| match &x.pat {
                Pat::Ident(i) => {
                    ts_params.push(i.type_ann.as_ref().map(|a| (&*a.type_ann).clone()));
                    Some(i.id.clone().into())
                }
                _ => None,
            })
            .collect::<Vec<Ident>>();
        Ok(Self {
            cfg,
            entry,
            params,
            is_generator: value.is_generator,
            is_async: value.is_async,
            ts_params,
        })
    }
}
impl TryFrom<Func> for TFunc {
    type Error = anyhow::Error;

    fn try_from(value: Func) -> Result<Self, Self::Error> {
        TFunc::try_from_with_mapper(value, linearize::static_map! {_ => None})
    }
}
impl TryFrom<Function> for TFunc {
    type Error = anyhow::Error;

    fn try_from(value: Function) -> Result<Self, Self::Error> {
        let a: Func = value.try_into()?;
        return a.try_into();
    }
}
#[derive(Default, Clone, Debug)]
pub struct TCfg {
    pub blocks: Arena<TBlock>,
    pub regs: LAM<()>,
    pub decls: BTreeSet<Ident>,
    pub type_annotations: BTreeMap<Ident, TsType>,
    pub generics: Option<TsTypeParamDecl>,
    pub ts_retty: Option<TsTypeAnn>,
}
impl TCfg {
    pub fn remark(&mut self) {
        let mut a: BTreeMap<LId, usize> = BTreeMap::new();
        for s in self.blocks.iter().flat_map(|a| &a.1.stmts) {
            if match &s.left {
                LId::Id { id } => !self.decls.contains(&id),
                LId::Member { obj, mem } => {
                    !self.decls.contains(&obj) || !self.decls.contains(&mem[0])
                }
                _ => todo!(),
            } {
                continue;
            }
            *a.entry(s.left.clone()).or_default() += 1usize;
        }
        for s in self.blocks.iter_mut().flat_map(|a| &mut a.1.stmts) {
            if match &s.left {
                LId::Id { id } => !self.decls.contains(&id),
                LId::Member { obj, mem } => {
                    !self.decls.contains(&obj) || !self.decls.contains(&mem[0])
                }
                _ => todo!(),
            } {
                continue;
            }
            if a.remove(&s.left) == Some(1) {
                s.flags |= ValFlags::SSA_LIKE
            } else {
                s.flags &= !ValFlags::SSA_LIKE;
            }
        }
    }
    pub fn def(&self, i: portal_jsc_common::LId<Ident>) -> Option<&Item> {
        self.blocks.iter().flat_map(|a| &a.1.stmts).find_map(|a| {
            if a.left == i && a.flags.contains(ValFlags::SSA_LIKE) {
                Some(&a.right)
            } else {
                None
            }
        })
    }
    pub fn refs<'a>(&'a self) -> impl Iterator<Item = Ident> + 'a {
        let a = self.blocks.iter().flat_map(|k| {
            let i: Box<dyn Iterator<Item = Ident> + '_> = match &k.1.term {
                TTerm::Return(a) => Box::new(a.iter().cloned()),
                TTerm::Throw(b) => Box::new(Some(b.clone()).into_iter()),
                TTerm::Jmp(id) => Box::new(std::iter::empty()),
                TTerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => Box::new(once(cond.clone())),
                TTerm::Switch { x, blocks, default } => {
                    Box::new(once(x.clone()).chain(blocks.iter().map(|a| a.0.clone())))
                }
                TTerm::Default => Box::new(std::iter::empty()),
            };
            i.chain(k.1.stmts.iter().flat_map(
                |TStmt {
                     left: a,
                     flags: _,
                     right: b,
                     span: _,
                 }| {
                    let a = a.as_ref().refs().cloned();
                    let b = b.refs().cloned();
                    a.chain(b)
                },
            ))
        });
        return a;
    }
    pub fn externs<'a>(&'a self) -> impl Iterator<Item = Ident> + 'a {
        self.refs().filter(|a| !self.decls.contains(a))
    }
    pub fn update(&mut self) {
        for x in self.blocks.iter() {
            for k in x.1.stmts.iter() {
                k.left
                    .clone()
                    .map(&mut |a| {
                        self.regs[a] = ();
                        Ok::<(), Infallible>(())
                    })
                    .unwrap();
            }
        }
    }
}
#[derive(Clone, Debug)]
pub struct TStmt {
    pub left: LId,
    pub flags: ValFlags,
    pub right: Item,
    pub span: Span,
}
#[derive(Clone, Default, Debug)]
pub struct TBlock {
    pub stmts: Vec<TStmt>,
    pub catch: TCatch,
    pub term: TTerm,
    pub orig_span: Option<Span>,
}
#[derive(Clone, Default, Debug)]
pub enum TCatch {
    #[default]
    Throw,
    Jump {
        pat: Ident,
        k: Id<TBlock>,
    },
}
#[derive(Clone, Default, Debug)]
pub enum TTerm {
    Return(Option<Ident>),
    Throw(Ident),
    Jmp(Id<TBlock>),
    CondJmp {
        cond: Ident,
        if_true: Id<TBlock>,
        if_false: Id<TBlock>,
    },
    Switch {
        x: Ident,
        blocks: HashMap<Ident, Id<TBlock>>,
        default: Id<TBlock>,
    },
    #[default]
    Default,
}
#[derive(Clone, Ord, PartialEq, PartialOrd, Eq, Debug)]
pub enum PropKey<I = Ident> {
    Lit(Ident),
    Computed(I),
}
impl<I> PropKey<I> {
    pub fn as_ref(&self) -> PropKey<&I> {
        match self {
            PropKey::Lit(a) => PropKey::Lit(a.clone()),
            PropKey::Computed(c) => PropKey::Computed(c),
        }
    }
    pub fn as_mut(&mut self) -> PropKey<&mut I> {
        match self {
            PropKey::Lit(a) => PropKey::Lit(a.clone()),
            PropKey::Computed(c) => PropKey::Computed(c),
        }
    }
    pub fn map<J: Ord, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<PropKey<J>, E> {
        Ok(match self {
            PropKey::Lit(l) => PropKey::Lit(l),
            PropKey::Computed(x) => PropKey::Computed(f(x)?),
        })
    }
}
#[derive(Clone, Debug)]
pub enum TCallee<I = Ident> {
    Val(I),
    Member { r#fn: I, member: I },
    Static(Ident),
}
impl<I> TCallee<I> {
    pub fn as_ref(&self) -> TCallee<&I> {
        match self {
            TCallee::Val(a) => TCallee::Val(a),
            TCallee::Member { r#fn, member } => TCallee::Member { r#fn, member },
            TCallee::Static(a) => TCallee::Static(a.clone()),
        }
    }
    pub fn as_mut(&mut self) -> TCallee<&mut I> {
        match self {
            TCallee::Val(a) => TCallee::Val(a),
            TCallee::Member { r#fn, member } => TCallee::Member { r#fn, member },
            TCallee::Static(a) => TCallee::Static(a.clone()),
        }
    }
    pub fn map<J: Ord, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<TCallee<J>, E> {
        Ok(match self {
            TCallee::Val(a) => TCallee::Val(f(a)?),
            TCallee::Member { r#fn, member } => TCallee::Member {
                r#fn: f(r#fn)?,
                member: f(member)?,
            },
            TCallee::Static(a) => TCallee::Static(a),
        })
    }
}
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Item<I = Ident, F = TFunc> {
    Just { id: I },
    Bin { left: I, right: I, op: BinaryOp },
    Un { arg: I, op: UnaryOp },
    Mem { obj: I, mem: I },
    Func { func: F },
    Lit { lit: Lit },
    Call { callee: TCallee<I>, args: Vec<I> },
    Obj { members: Vec<(PropKey<I>, I)> },
    Arr { members: Vec<I> },
    Yield { value: Option<I>, delegate: bool },
    Await { value: I },
    Asm { value: Asm<I> },
    Undef,
    This,
    Intrinsic { value: Native<I> },
}
impl<I> Item<I> {
    pub fn map<J: Ord, E>(self, f: &mut (dyn FnMut(I) -> Result<J, E> + '_)) -> Result<Item<J>, E> {
        self.map2(f, &mut |a, b| a(b), &mut |_, b| Ok(b))
    }
}
impl<I, F> Item<I, F> {
    pub fn as_ref(&self) -> Item<&I, &F> {
        match self {
            Item::Just { id } => Item::Just { id },
            Item::Bin { left, right, op } => Item::Bin {
                left,
                right,
                op: *op,
            },
            Item::Un { arg, op } => Item::Un { arg, op: *op },
            Item::Mem { obj, mem } => Item::Mem { obj, mem },
            Item::Func { func } => Item::Func { func },
            Item::Lit { lit } => Item::Lit { lit: lit.clone() },
            Item::Call { callee, args } => Item::Call {
                callee: callee.as_ref(),
                args: args.iter().collect(),
            },
            Item::Obj { members } => Item::Obj {
                members: members.iter().map(|(a, b)| (a.as_ref(), b)).collect(),
            },
            Item::Arr { members } => Item::Arr {
                members: members.iter().collect(),
            },
            Item::Yield { value, delegate } => Item::Yield {
                value: value.as_ref(),
                delegate: *delegate,
            },
            Item::Await { value } => Item::Await { value },
            Item::Asm { value } => Item::Asm {
                value: value.as_ref(),
            },
            Item::Undef => Item::Undef,
            Item::This => Item::This,
            Item::Intrinsic { value } => Item::Intrinsic {
                value: value.as_ref(),
            },
        }
    }
    pub fn as_mut(&mut self) -> Item<&mut I, &mut F> {
        match self {
            Item::Just { id } => Item::Just { id },
            Item::Bin { left, right, op } => Item::Bin {
                left,
                right,
                op: *op,
            },
            Item::Un { arg, op } => Item::Un { arg, op: *op },
            Item::Mem { obj, mem } => Item::Mem { obj, mem },
            Item::Func { func } => Item::Func { func },
            Item::Lit { lit } => Item::Lit { lit: lit.clone() },
            Item::Call { callee, args } => Item::Call {
                callee: callee.as_mut(),
                args: args.iter_mut().collect(),
            },
            Item::Obj { members } => Item::Obj {
                members: members.iter_mut().map(|(a, b)| (a.as_mut(), b)).collect(),
            },
            Item::Arr { members } => Item::Arr {
                members: members.iter_mut().collect(),
            },
            Item::Yield { value, delegate } => Item::Yield {
                value: value.as_mut(),
                delegate: *delegate,
            },
            Item::Await { value } => Item::Await { value },
            Item::Asm { value } => Item::Asm {
                value: value.as_mut(),
            },
            Item::Undef => Item::Undef,
            Item::This => Item::This,
            Item::Intrinsic { value } => Item::Intrinsic {
                value: value.as_mut(),
            },
        }
    }
    pub fn map2<J: Ord, G, E, C: ?Sized>(
        self,
        cx: &mut C,
        f: &mut (dyn FnMut(&mut C, I) -> Result<J, E> + '_),
        g: &mut (dyn FnMut(&mut C, F) -> Result<G, E> + '_),
    ) -> Result<Item<J, G>, E> {
        Ok(match self {
            Item::Just { id } => Item::Just { id: f(cx, id)? },
            Item::Bin { left, right, op } => Item::Bin {
                left: f(cx, left)?,
                right: f(cx, right)?,
                op,
            },
            Item::Un { arg, op } => Item::Un {
                arg: f(cx, arg)?,
                op,
            },
            Item::Mem { obj, mem } => Item::Mem {
                obj: f(cx, obj)?,
                mem: f(cx, mem)?,
            },
            Item::Func { func } => Item::Func { func: g(cx, func)? },
            Item::Lit { lit } => Item::Lit { lit },
            Item::Call { callee, args } => Item::Call {
                callee: callee.map(&mut |a| f(cx, a))?,
                args: args
                    .into_iter()
                    .map(|a| f(cx, a))
                    .collect::<Result<Vec<J>, E>>()?,
            },
            Item::Obj { members } => Item::Obj {
                members: members
                    .into_iter()
                    .map(|(a, b)| Ok((a.map(&mut |a| f(cx, a))?, f(cx, b)?)))
                    .collect::<Result<_, E>>()?,
            },
            Item::Arr { members } => Item::Arr {
                members: members
                    .into_iter()
                    .map(|a| f(cx, a))
                    .collect::<Result<_, E>>()?,
            },
            Item::Yield { value, delegate } => Item::Yield {
                value: match value {
                    None => None,
                    Some(a) => Some(f(cx, a)?),
                },
                delegate: delegate,
            },
            Item::Await { value } => Item::Await {
                value: f(cx, value)?,
            },
            Item::Undef => Item::Undef,
            Item::Asm { value } => Item::Asm {
                value: value.map(&mut |a| f(cx, a))?,
            },
            Item::This => Item::This,
            Item::Intrinsic { value } => Item::Intrinsic {
                value: value.map(&mut |a| f(cx, a))?,
            },
        })
    }
    pub fn refs<'a>(&'a self) -> Box<dyn Iterator<Item = &'a I> + 'a> {
        use crate as swc_tac;
        match self {
            swc_tac::Item::Just { id } => Box::new(once(id)),
            swc_tac::Item::Bin { left, right, op } => Box::new([left, right].into_iter()),
            swc_tac::Item::Un { arg, op } => Box::new(once(arg)),
            swc_tac::Item::Mem { obj, mem } => Box::new([obj, mem].into_iter()),
            swc_tac::Item::Func { func } => Box::new(empty()),
            swc_tac::Item::Lit { lit } => Box::new(empty()),
            swc_tac::Item::Call { callee, args } => Box::new(
                match callee {
                    swc_tac::TCallee::Val(a) => vec![a],
                    swc_tac::TCallee::Member { r#fn, member } => vec![r#fn, member],
                    swc_tac::TCallee::Static(_) => vec![],
                }
                .into_iter()
                .chain(args.iter()),
            ),
            swc_tac::Item::Obj { members } => Box::new(members.iter().flat_map(|m| {
                let v = once(&m.1);
                let w: Box<dyn Iterator<Item = &I> + '_> = match &m.0 {
                    swc_tac::PropKey::Lit(_) => Box::new(empty()),
                    swc_tac::PropKey::Computed(c) => Box::new(once(c)),
                };
                v.chain(w)
            })),
            swc_tac::Item::Arr { members } => Box::new(members.iter()),
            swc_tac::Item::Yield { value, delegate } => Box::new(value.iter()),
            swc_tac::Item::Await { value } => Box::new(once(value)),
            swc_tac::Item::Undef | Item::This => Box::new(empty()),
            Item::Asm { value } => Box::new(value.refs()),
            Item::Intrinsic { value } => {
                let mut v = Vec::default();
                value
                    .as_ref()
                    .map(&mut |a| Ok::<_, Infallible>(v.push(a)))
                    .unwrap();
                Box::new(v.into_iter())
            }
        }
    }
    pub fn refs_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut I> + 'a> {
        use crate as swc_tac;
        match self {
            swc_tac::Item::Just { id } => Box::new(once(id)),
            swc_tac::Item::Bin { left, right, op } => Box::new([left, right].into_iter()),
            swc_tac::Item::Un { arg, op } => Box::new(once(arg)),
            swc_tac::Item::Mem { obj, mem } => Box::new([obj, mem].into_iter()),
            swc_tac::Item::Func { func } => Box::new(empty()),
            swc_tac::Item::Lit { lit } => Box::new(empty()),
            swc_tac::Item::Call { callee, args } => Box::new(
                match callee {
                    swc_tac::TCallee::Val(a) => vec![a],
                    swc_tac::TCallee::Member { r#fn, member } => vec![r#fn, member],
                    swc_tac::TCallee::Static(_) => vec![],
                }
                .into_iter()
                .chain(args.iter_mut()),
            ),
            swc_tac::Item::Obj { members } => Box::new(members.iter_mut().flat_map(|m| {
                let v = once(&mut m.1);
                let w: Box<dyn Iterator<Item = &mut I> + '_> = match &mut m.0 {
                    swc_tac::PropKey::Lit(_) => Box::new(empty()),
                    swc_tac::PropKey::Computed(c) => Box::new(once(c)),
                };
                v.chain(w)
            })),
            swc_tac::Item::Arr { members } => Box::new(members.iter_mut()),
            swc_tac::Item::Yield { value, delegate } => Box::new(value.iter_mut()),
            swc_tac::Item::Await { value } => Box::new(once(value)),
            swc_tac::Item::Undef | Item::This => Box::new(empty()),
            Item::Asm { value } => Box::new(value.refs_mut()),
            Item::Intrinsic { value } => {
                let mut v = Vec::default();
                value
                    .as_mut()
                    .map(&mut |a| Ok::<_, Infallible>(v.push(a)))
                    .unwrap();
                Box::new(v.into_iter())
            }
        }
    }
}

#[derive(Default)]
#[non_exhaustive]
pub struct Trans<'a> {
    pub map: BTreeMap<Id<Block>, Id<TBlock>>,
    pub ret_to: Option<(Ident, Id<TBlock>)>,
    pub recatch: TCatch,
    pub this: Option<Ident>,
    pub import_mapper: StaticMap<ImportMapperReq, Option<&'a (dyn ImportMapper + 'a)>>,
}
impl Trans<'_> {
    pub fn trans(&mut self, i: &Cfg, o: &mut TCfg, b: Id<Block>) -> anyhow::Result<Id<TBlock>> {
        loop {
            if let Some(a) = self.map.get(&b) {
                return Ok(*a);
            }
            let t = o.blocks.alloc(TBlock {
                stmts: vec![],
                catch: self.recatch.clone(),
                term: Default::default(),
                orig_span: i.blocks[b].end.orig_span.clone(),
            });
            self.map.insert(b, t);
            if let Catch::Jump { pat, k } = &i.blocks[b].end.catch {
                match pat {
                    Pat::Ident(id) => {
                        let k = self.trans(i, o, *k)?;
                        o.blocks[t].catch = TCatch::Jump {
                            pat: id.id.clone().into(),
                            k,
                        };
                    }
                    _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
                }
            }
            let mut t = t;
            for s in i.blocks[b].stmts.iter() {
                t = self.stmt(i, o, b, t, s)?;
            }
            let term = match &i.blocks[b].end.term {
                swc_cfg::Term::Return(expr) => match self.ret_to.clone() {
                    None => match expr {
                        None => TTerm::Return(None),
                        Some(a) => {
                            let c;
                            (c, t) = self.expr(i, o, b, t, a)?;
                            TTerm::Return(Some(c))
                        }
                    },
                    Some((i2, b2)) => {
                        if let Some(a) = expr {
                            let c;
                            (c, t) = self.expr(i, o, b, t, a)?;
                            o.blocks[t].stmts.push(TStmt {
                                left: LId::Id { id: i2.clone() },
                                flags: Default::default(),
                                right: Item::Just { id: c },
                                span: i.blocks[b]
                                    .end
                                    .orig_span
                                    .clone()
                                    .unwrap_or_else(|| Span::dummy_with_cmt()),
                            });
                        }
                        TTerm::Jmp(b2)
                    }
                },
                swc_cfg::Term::Throw(expr) => {
                    let c;
                    (c, t) = self.expr(i, o, b, t, expr)?;
                    TTerm::Throw(c)
                }
                swc_cfg::Term::Jmp(id) => TTerm::Jmp(self.trans(i, o, *id)?),
                swc_cfg::Term::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => {
                    let c;
                    (c, t) = self.expr(i, o, b, t, cond)?;
                    TTerm::CondJmp {
                        cond: c,
                        if_true: self.trans(i, o, *if_true)?,
                        if_false: self.trans(i, o, *if_false)?,
                    }
                }
                swc_cfg::Term::Switch { x, blocks, default } => {
                    let y;
                    (y, t) = self.expr(i, o, b, t, x)?;
                    let mut m2 = HashMap::new();
                    for (a, b2) in blocks.iter() {
                        let b2 = self.trans(i, o, *b2)?;
                        let c;
                        (c, t) = self.expr(i, o, b, t, a)?;
                        m2.insert(c, b2);
                    }
                    TTerm::Switch {
                        x: y,
                        blocks: m2,
                        default: self.trans(i, o, *default)?,
                    }
                }
                swc_cfg::Term::Default => TTerm::Default,
            };
            o.blocks[t].term = term;
        }
    }
    pub fn stmt(
        &mut self,
        i: &Cfg,
        o: &mut TCfg,
        b: Id<Block>,
        mut t: Id<TBlock>,
        s: &Stmt,
    ) -> anyhow::Result<Id<TBlock>> {
        match s {
            Stmt::Expr(e) => {
                (_, t) = self.expr(i, o, b, t, &e.expr)?;
                return Ok(t);
            }
            Stmt::Empty(_) => return Ok(t),
            Stmt::Decl(d) => match d {
                swc_ecma_ast::Decl::Class(class_decl) => todo!(),
                swc_ecma_ast::Decl::Fn(f) => {
                    o.blocks[t].stmts.push(TStmt {
                        left: LId::Id {
                            id: f.ident.clone().into(),
                        },
                        flags: Default::default(),
                        right: Item::Func {
                            func: f.function.as_ref().clone().try_into()?,
                        },
                        span: f.span(),
                    });
                    o.decls.insert(f.ident.clone().into());
                    return Ok(t);
                }
                swc_ecma_ast::Decl::Var(var_decl) => {
                    for var_decl in var_decl.decls.iter() {
                        if let Some(e) = &var_decl.init {
                            match &var_decl.name {
                                Pat::Ident(i2) => {
                                    let f;
                                    (f, t) = self.expr(i, o, b, t, e)?;
                                    o.blocks[t].stmts.push(TStmt {
                                        left: LId::Id {
                                            id: i2.id.clone().into(),
                                        },
                                        flags: Default::default(),
                                        right: Item::Just { id: f },
                                        span: e.span(),
                                    });
                                    o.decls.insert(i2.id.clone().into());
                                    if let Some(a) = i2.type_ann.as_ref().cloned() {
                                        o.type_annotations
                                            .insert(i2.id.clone().into(), *a.type_ann);
                                    }
                                }
                                _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
                            }
                        }
                    }
                    return Ok(t);
                }
                swc_ecma_ast::Decl::Using(using_decl) => todo!(),
                swc_ecma_ast::Decl::TsInterface(ts_interface_decl) => todo!(),
                swc_ecma_ast::Decl::TsTypeAlias(ts_type_alias_decl) => todo!(),
                swc_ecma_ast::Decl::TsEnum(ts_enum_decl) => todo!(),
                swc_ecma_ast::Decl::TsModule(ts_module_decl) => todo!(),
            },
            _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
        }
    }
    pub fn member_expr(
        &mut self,
        i: &Cfg,
        o: &mut TCfg,
        b: Id<Block>,
        mut t: Id<TBlock>,
        s: &MemberExpr,
    ) -> anyhow::Result<(Ident, Id<TBlock>)> {
        let obj;
        (obj, t) = self.expr(i, o, b, t, &s.obj)?;
        let mem;
        // let e;
        (mem, t) = self.expr(i, o, b, t, &imp(s.prop.clone()))?;
        let v = o.regs.alloc(());
        o.blocks[t].stmts.push(TStmt {
            left: LId::Id { id: v.clone() },
            flags: ValFlags::SSA_LIKE,
            right: Item::Mem { obj, mem },
            span: s.span(),
        });
        o.decls.insert(v.clone());
        Ok((v, t))
    }
    pub fn expr(
        &mut self,
        i: &Cfg,
        o: &mut TCfg,
        b: Id<Block>,
        mut t: Id<TBlock>,
        s: &Expr,
    ) -> anyhow::Result<(Ident, Id<TBlock>)> {
        if let Some(i2) = self.import_mapper[ImportMapperReq::Native].as_deref() {
            if let Some(n) = s.resolve_natives(i2) {
                let arg = n.map(&mut |e| {
                    let x;
                    (x, t) = self.expr(i, o, b, t, e)?;
                    anyhow::Ok(x)
                })?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Intrinsic { value: arg },
                    span: s.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
        }
        match s {
            Expr::Cond(c) => {
                let v;
                (v, t) = self.expr(i, o, b, t, &c.test)?;
                let then = o.blocks.alloc(TBlock {
                    stmts: vec![],
                    catch: o.blocks[t].catch.clone(),
                    term: Default::default(),
                    orig_span: Some(c.span),
                });
                let els = o.blocks.alloc(TBlock {
                    stmts: vec![],
                    catch: o.blocks[t].catch.clone(),
                    term: Default::default(),
                    orig_span: Some(c.span),
                });
                let done = o.blocks.alloc(TBlock {
                    stmts: vec![],
                    catch: o.blocks[t].catch.clone(),
                    term: Default::default(),
                    orig_span: Some(c.span),
                });
                o.blocks[t].term = TTerm::CondJmp {
                    cond: v,
                    if_true: then,
                    if_false: els,
                };
                let tmp = o.regs.alloc(());
                o.decls.insert(tmp.clone());
                let (a, then) = self.expr(i, o, b, then, &c.cons)?;
                o.blocks[then].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Just { id: a },
                    span: s.span(),
                });
                o.blocks[then].term = TTerm::Jmp(done);
                let (a, els) = self.expr(i, o, b, els, &c.alt)?;
                o.blocks[els].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Just { id: a },
                    span: s.span(),
                });
                o.blocks[els].term = TTerm::Jmp(done);
                return Ok((tmp, done));
            }
            Expr::This(this) => {
                let id = match self.this.clone() {
                    Some(a) => a,
                    None => {
                        let tmp = o.regs.alloc(());
                        o.blocks[t].stmts.push(TStmt {
                            left: LId::Id { id: tmp.clone() },
                            flags: ValFlags::SSA_LIKE,
                            right: Item::This,
                            span: this.span(),
                        });
                        o.decls.insert(tmp.clone());
                        tmp
                    }
                };
                return Ok((id, t));
            }
            Expr::Ident(i) => Ok((i.clone().into(), t)),
            Expr::Assign(a) => {
                let mut right;
                (right, t) = self.expr(i, o, b, t, &a.right)?;
                match &a.left {
                    swc_ecma_ast::AssignTarget::Simple(simple_assign_target) => {
                        match &simple_assign_target {
                            SimpleAssignTarget::Ident(i) => {
                                let item = match a.op.clone().to_update() {
                                    None => Item::Just { id: right },
                                    Some(a) => Item::Bin {
                                        left: right,
                                        right: i.id.clone().into(),
                                        op: a,
                                    },
                                };
                                o.blocks[t].stmts.push(TStmt {
                                    left: LId::Id {
                                        id: i.id.clone().into(),
                                    },
                                    flags: Default::default(),
                                    right: item,
                                    span: i.span(),
                                });
                                right = i.id.clone().into();
                            }
                            SimpleAssignTarget::Member(m) => {
                                let obj;
                                let mem;
                                let e;
                                (obj, t) = self.expr(i, o, b, t, &m.obj)?;
                                (mem, t) = self.expr(
                                    i,
                                    o,
                                    b,
                                    t,
                                    match &m.prop {
                                        swc_ecma_ast::MemberProp::Ident(ident_name) => {
                                            e = Expr::Ident(swc_ecma_ast::Ident::new(
                                                ident_name.sym.clone(),
                                                ident_name.span,
                                                Default::default(),
                                            ));
                                            &e
                                        }
                                        swc_ecma_ast::MemberProp::PrivateName(private_name) => {
                                            todo!()
                                        }
                                        swc_ecma_ast::MemberProp::Computed(computed_prop_name) => {
                                            &computed_prop_name.expr
                                        }
                                    },
                                )?;
                                let item = match a.op.clone().to_update() {
                                    None => Item::Just { id: right },
                                    Some(a) => {
                                        let id = o.regs.alloc(());
                                        o.blocks[t].stmts.push(TStmt {
                                            left: LId::Id { id: id.clone() },
                                            flags: ValFlags::SSA_LIKE,
                                            right: Item::Mem {
                                                obj: obj.clone(),
                                                mem: mem.clone(),
                                            },
                                            span: m.span(),
                                        });
                                        Item::Bin {
                                            left: right,
                                            right: id,
                                            op: a,
                                        }
                                    }
                                };
                                o.blocks[t].stmts.push(TStmt {
                                    left: LId::Member {
                                        obj: obj.clone(),
                                        mem: [mem.clone()],
                                    },
                                    flags: Default::default(),
                                    right: item,
                                    span: m.span(),
                                });
                                right = o.regs.alloc(());
                                o.blocks[t].stmts.push(TStmt {
                                    left: LId::Id { id: right.clone() },
                                    flags: ValFlags::SSA_LIKE,
                                    right: Item::Mem { obj, mem },
                                    span: m.span(),
                                });
                                o.decls.insert(right.clone());
                            }
                            _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
                        }
                    }
                    swc_ecma_ast::AssignTarget::Pat(assign_target_pat) => {
                        match &assign_target_pat {
                            _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
                        }
                    }
                };
                return Ok((right, t));
            }
            Expr::Call(call) => {
                let c = match &call.callee {
                    Callee::Expr(e) => match e.as_ref() {
                        Expr::Member(m) => {
                            let r#fn;
                            (r#fn, t) = self.expr(i, o, b, t, &m.obj)?;
                            let member;
                            (member, t) = self.expr(i, o, b, t, &imp(m.prop.clone()))?;
                            TCallee::Member { r#fn, member }
                        }
                        Expr::Fn(f) if f.function.params.len() == call.args.len() => {
                            for (p, a) in f.function.params.iter().zip(call.args.iter()) {
                                let Pat::Ident(id) = &p.pat else {
                                    anyhow::bail!("non-simple pattern")
                                };
                                let arg;
                                (arg, t) = self.expr(i, o, b, t, &a.expr)?;
                                o.blocks[t].stmts.push(TStmt {
                                    left: LId::Id { id: id.to_id() },
                                    flags: Default::default(),
                                    right: Item::Just { id: arg },
                                    span: a.span(),
                                });
                            }
                            let tmp = o.regs.alloc(());
                            let t2 = o.blocks.alloc(TBlock {
                                stmts: vec![],
                                catch: o.blocks[t].catch.clone(),
                                term: Default::default(),
                                orig_span: Some(f.span()),
                            });
                            let cfg: swc_cfg::Func = f.function.as_ref().clone().try_into()?;
                            let mut t4 = Trans {
                                map: Default::default(),
                                ret_to: Some((tmp.clone(), t2)),
                                recatch: o.blocks[t].catch.clone(),
                                this: Some((Atom::new("globalThis"), Default::default())),
                                import_mapper: static_map! {a => self.import_mapper[a].as_deref()},
                            };
                            let t3 = t4.trans(&cfg.cfg, o, cfg.entry)?;
                            o.blocks[t].term = TTerm::Jmp(t3);
                            return Ok((tmp, t2));
                        }
                        _ => {
                            let r#fn;
                            (r#fn, t) = self.expr(i, o, b, t, e.as_ref())?;
                            match o
                                .def(portal_jsc_common::LId::Id { id: r#fn.clone() })
                                .cloned()
                            {
                                Some(Item::Func { func })
                                    if func.params.len() == call.args.len() =>
                                {
                                    for (p, a) in func.params.iter().zip(call.args.iter()) {
                                        // let Pat::Ident(id) = &p.pat else {
                                        //     anyhow::bail!("non-simple pattern")
                                        // };
                                        let arg;
                                        (arg, t) = self.expr(i, o, b, t, &a.expr)?;
                                        o.blocks[t].stmts.push(TStmt {
                                            left: LId::Id { id: p.clone() },
                                            flags: Default::default(),
                                            right: Item::Just { id: arg },
                                            span: a.span(),
                                        });
                                    }
                                    let tmp = o.regs.alloc(());
                                    let t2 = o.blocks.alloc(TBlock {
                                        stmts: vec![],
                                        catch: o.blocks[t].catch.clone(),
                                        term: Default::default(),
                                        orig_span: Some(e.span()),
                                    });
                                    let cfg: swc_cfg::Func = func.clone().try_into()?;
                                    let mut t4 = Trans {
                                        map: Default::default(),
                                        ret_to: Some((tmp.clone(), t2)),
                                        recatch: o.blocks[t].catch.clone(),
                                        this: Some((Atom::new("globalThis"), Default::default())),
                                        import_mapper: static_map! {a => self.import_mapper[a].as_deref()},
                                    };
                                    let t3 = t4.trans(&cfg.cfg, o, cfg.entry)?;
                                    o.blocks[t].term = TTerm::Jmp(t3);
                                    return Ok((tmp, t2));
                                }
                                _ => TCallee::Val(r#fn),
                            }
                        }
                    },
                    _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
                };
                let args = call
                    .args
                    .iter()
                    .map(|a| {
                        let arg;
                        (arg, t) = self.expr(i, o, b, t, &a.expr)?;
                        anyhow::Ok(arg)
                    })
                    .collect::<anyhow::Result<_>>()?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Call { callee: c, args },
                    span: call.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Bin(bin) => match (&*bin.left, &*bin.right, bin.op.clone()) {
                (l, Expr::Lit(Lit::Num(Number { value: 0.0, .. })), BinaryOp::BitOr)
                | (Expr::Lit(Lit::Num(Number { value: 0.0, .. })), l, BinaryOp::BitOr) => {
                    let left;
                    // let right;
                    (left, t) = self.expr(i, o, b, t, l)?;
                    // (right, t) = self.expr(i, o, b, t, r)?;
                    let tmp = o.regs.alloc(());
                    o.blocks[t].stmts.push(TStmt {
                        left: LId::Id { id: tmp.clone() },
                        flags: ValFlags::SSA_LIKE,
                        right: Item::Asm {
                            value: Asm::OrZero(left),
                        },
                        span: bin.span(),
                    });
                    o.decls.insert(tmp.clone());
                    return Ok((tmp, t));
                }
                (l, r, op) => {
                    let left;
                    let right;
                    (left, t) = self.expr(i, o, b, t, l)?;
                    (right, t) = self.expr(i, o, b, t, r)?;
                    let tmp = o.regs.alloc(());
                    o.blocks[t].stmts.push(TStmt {
                        left: LId::Id { id: tmp.clone() },
                        flags: ValFlags::SSA_LIKE,
                        right: Item::Bin { left, right, op },
                        span: bin.span(),
                    });
                    o.decls.insert(tmp.clone());
                    return Ok((tmp, t));
                }
            },
            Expr::Unary(un) => {
                if un.op == UnaryOp::Void {
                    let tmp = o.regs.alloc(());
                    o.blocks[t].stmts.push(TStmt {
                        left: LId::Id { id: tmp.clone() },
                        flags: ValFlags::SSA_LIKE,
                        right: Item::Undef,
                        span: un.span(),
                    });
                    o.decls.insert(tmp.clone());
                    return Ok((tmp, t));
                }
                let arg;
                // let right;
                (arg, t) = self.expr(i, o, b, t, &un.arg)?;
                // (right, t) = self.expr(i, o, b, t, &bin.right)?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Un {
                        arg,
                        op: un.op.clone(),
                    },
                    span: un.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Member(m) => return self.member_expr(i, o, b, t, m),
            Expr::Lit(l) => {
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Lit { lit: l.clone() },
                    span: l.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Fn(f) => {
                let tmp = match &f.ident {
                    Some(a) => a.clone().into(),
                    None => o.regs.alloc(()),
                };
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: Default::default(),
                    right: Item::Func {
                        func: f.function.as_ref().clone().try_into()?,
                    },
                    span: f.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Array(arr) => {
                let members = arr
                    .elems
                    .iter()
                    .flat_map(|a| a.as_ref())
                    .map(|x| {
                        anyhow::Ok({
                            let y;
                            (y, t) = self.expr(i, o, b, t, &x.expr)?;
                            y
                        })
                    })
                    .collect::<anyhow::Result<_>>()?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Arr { members },
                    span: arr.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Object(obj) => {
                let members = obj
                    .props
                    .iter()
                    .map(|a| {
                        anyhow::Ok(match a {
                            swc_ecma_ast::PropOrSpread::Spread(spread_element) => {
                                anyhow::bail!("todo: {}:{}", file!(), line!())
                            }
                            swc_ecma_ast::PropOrSpread::Prop(prop) => match prop.as_ref() {
                                swc_ecma_ast::Prop::Shorthand(ident) => {
                                    Some((PropKey::Lit(ident.clone().into()), ident.clone().into()))
                                }
                                swc_ecma_ast::Prop::KeyValue(key_value_prop) => {
                                    let v;
                                    (v, t) = self.expr(i, o, b, t, &key_value_prop.value)?;
                                    match &key_value_prop.key {
                                        swc_ecma_ast::PropName::Ident(ident_name) => Some((
                                            PropKey::Lit((
                                                ident_name.sym.clone(),
                                                Default::default(),
                                            )),
                                            v,
                                        )),
                                        swc_ecma_ast::PropName::Str(s) => Some((
                                            PropKey::Lit((s.value.clone(), Default::default())),
                                            v,
                                        )),
                                        swc_ecma_ast::PropName::Num(number) => {
                                            anyhow::bail!("todo: {}:{}", file!(), line!())
                                        }
                                        swc_ecma_ast::PropName::Computed(computed_prop_name) => {
                                            let w;
                                            (w, t) = self.expr(i, o, b, t, s)?;
                                            Some((PropKey::Computed(w), v))
                                        }
                                        swc_ecma_ast::PropName::BigInt(big_int) => {
                                            anyhow::bail!("todo: {}:{}", file!(), line!())
                                        }
                                    }
                                }
                                swc_ecma_ast::Prop::Assign(assign_prop) => {
                                    anyhow::bail!("todo: {}:{}", file!(), line!())
                                }
                                swc_ecma_ast::Prop::Getter(getter_prop) => {
                                    anyhow::bail!("todo: {}:{}", file!(), line!())
                                }
                                swc_ecma_ast::Prop::Setter(setter_prop) => {
                                    anyhow::bail!("todo: {}:{}", file!(), line!())
                                }
                                swc_ecma_ast::Prop::Method(method_prop) => {
                                    anyhow::bail!("todo: {}:{}", file!(), line!())
                                }
                            },
                        })
                    })
                    .filter_map(|a| match a {
                        Ok(Some(a)) => Some(Ok(a)),
                        Ok(None) => None,
                        Err(e) => Some(Err(e)),
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Obj { members },
                    span: obj.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Await(x) => {
                let (a, t) = self.expr(i, o, b, t, &x.arg)?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Await { value: a.clone() },
                    span: x.span(),
                });
                return Ok((tmp, t));
            }
            Expr::Yield(y) => {
                let y2 = match &y.arg {
                    None => None,
                    Some(a) => {
                        let b2;
                        (b2, t) = self.expr(i, o, b, t, a.as_ref())?;
                        Some(b2)
                    }
                };
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push(TStmt {
                    left: LId::Id { id: tmp.clone() },
                    flags: ValFlags::SSA_LIKE,
                    right: Item::Yield {
                        value: y2,
                        delegate: y.delegate,
                    },
                    span: y.span(),
                });
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Seq(s) => {
                let mut r = None;
                for a in s.exprs.iter() {
                    let c;
                    (c, t) = self.expr(i, o, b, t, a)?;
                    r = Some(c)
                }
                return Ok((r.context("in getting the last one")?, t));
            }
            _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
        }
    }
}
