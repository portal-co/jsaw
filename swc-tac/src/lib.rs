use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::Infallible;
use std::default;
use std::iter::{empty, once};

use arena_traits::Arena as TArena;
use id_arena::{Arena, Id};
use lam::LAM;
use swc_cfg::{Block, Catch, Cfg, Func};
use swc_common::pass::Either;
use swc_ecma_ast::{
    AssignOp, BinaryOp, Expr, Function, Lit, MemberExpr, Pat, SimpleAssignTarget, Stmt, UnaryOp,
};

use swc_ecma_ast::Id as Ident;

pub mod lam;
pub mod rew;
#[derive(Clone)]
pub struct TFunc {
    pub cfg: TCfg,
    pub entry: Id<TBlock>,
    pub params: Vec<Ident>,
    pub is_generator: bool,
    pub is_async: bool,
}

impl TryFrom<Func> for TFunc {
    type Error = anyhow::Error;

    fn try_from(value: Func) -> Result<Self, Self::Error> {
        let mut cfg = TCfg::default();
        let entry = Trans {
            map: BTreeMap::new(),
        }
        .trans(&value.cfg, &mut cfg, value.entry)?;
        let params = value
            .params
            .iter()
            .filter_map(|x| match &x.pat {
                Pat::Ident(i) => Some(i.id.clone().into()),
                _ => None,
            })
            .collect::<Vec<Ident>>();
        Ok(Self {
            cfg,
            entry,
            params,
            is_generator: value.is_generator,
            is_async: value.is_async,
        })
    }
}
impl TryFrom<Function> for TFunc {
    type Error = anyhow::Error;

    fn try_from(value: Function) -> Result<Self, Self::Error> {
        let a: Func = value.try_into()?;
        return a.try_into();
    }
}
#[derive(Default, Clone)]
pub struct TCfg {
    pub blocks: Arena<TBlock>,
    pub regs: LAM<()>,
    pub decls: BTreeSet<Ident>,
}
impl TCfg {
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
            i.chain(k.1.stmts.iter().flat_map(|(a, b)| {
                let a: Box<dyn Iterator<Item = Ident> + '_> = match a {
                    LId::Id { id } => Box::new(once(id.clone())),
                    LId::Member { obj, mem } => {
                        Box::new(once(obj.clone()).chain(once(mem.clone())))
                    }
                };
                let b: Box<dyn Iterator<Item = Ident> + '_> = match b {
                    Item::Just { id } => Box::new(once(id.clone())),
                    Item::Bin { left, right, op } => {
                        Box::new(vec![left.clone(), right.clone()].into_iter())
                    }
                    Item::Un { arg, op } => Box::new(once(arg.clone())),
                    Item::Mem { obj, mem } => Box::new(once(obj.clone()).chain(once(mem.clone()))),
                    Item::Func { func } => Box::new(func.cfg.externs()),
                    Item::Lit { lit } => Box::new(empty()),
                    Item::Call { r#fn, args } => {
                        Box::new(once(r#fn.clone()).chain(args.iter().cloned()))
                    }
                    Item::Obj { members } => Box::new(members.iter().flat_map(|(a, b)| {
                        once(b.clone()).chain(
                            match a {
                                PropKey::Lit(i) => None,
                                PropKey::Computed(i) => Some(i.clone()),
                            }
                            .into_iter(),
                        )
                    })),
                    Item::Arr { members } => Box::new(members.iter().cloned()),
                    Item::Yield { value, delegate } => Box::new(value.iter().cloned()),
                    Item::Await { value } => Box::new(once(value.clone())),
                    Item::Undef => Box::new(empty()),
                };
                a.chain(b)
            }))
        });
        return a;
    }
    pub fn externs<'a>(&'a self) -> impl Iterator<Item = Ident> + 'a {
        self.refs().filter(|a| !self.decls.contains(a))
    }
    pub fn update(&mut self) {
        for x in self.blocks.iter() {
            for k in x.1.stmts.iter() {
                k.0.clone()
                    .map(&mut |a| {
                        self.regs[a] = ();
                        Ok::<(), Infallible>(())
                    })
                    .unwrap();
            }
        }
    }
}
#[derive(Clone, Default)]
pub struct TBlock {
    pub stmts: Vec<(LId, Item)>,
    pub catch: TCatch,
    pub term: TTerm,
}
#[derive(Clone, Default)]
pub enum TCatch {
    #[default]
    Throw,
    Jump {
        pat: Ident,
        k: Id<TBlock>,
    },
}
#[derive(Clone, Default)]
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
#[derive(Clone, Ord, PartialEq, PartialOrd, Eq)]
pub enum PropKey<I = Ident> {
    Lit(Ident),
    Computed(I),
}
impl<I> PropKey<I> {
    pub fn map<J: Ord, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<PropKey<J>, E> {
        Ok(match self {
            PropKey::Lit(l) => PropKey::Lit(l),
            PropKey::Computed(x) => PropKey::Computed(f(x)?),
        })
    }
}
#[derive(Clone)]
pub enum Item<I = Ident> {
    Just { id: I },
    Bin { left: I, right: I, op: BinaryOp },
    Un { arg: I, op: UnaryOp },
    Mem { obj: I, mem: I },
    Func { func: TFunc },
    Lit { lit: Lit },
    Call { r#fn: I, args: Vec<I> },
    Obj { members: BTreeMap<PropKey<I>, I> },
    Arr { members: Vec<I> },
    Yield { value: Option<I>, delegate: bool },
    Await { value: I },
    Undef,
}
impl<I> Item<I> {
    pub fn map<J: Ord, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<Item<J>, E> {
        Ok(match self {
            Item::Just { id } => Item::Just { id: f(id)? },
            Item::Bin { left, right, op } => Item::Bin {
                left: f(left)?,
                right: f(right)?,
                op,
            },
            Item::Un { arg, op } => Item::Un { arg: f(arg)?, op },
            Item::Mem { obj, mem } => Item::Mem {
                obj: f(obj)?,
                mem: f(mem)?,
            },
            Item::Func { func } => Item::Func { func },
            Item::Lit { lit } => Item::Lit { lit },
            Item::Call { r#fn, args } => Item::Call {
                r#fn: f(r#fn)?,
                args: args.into_iter().map(f).collect::<Result<Vec<J>, E>>()?,
            },
            Item::Obj { members } => Item::Obj {
                members: members
                    .into_iter()
                    .map(|(a, b)| Ok((a.map(f)?, f(b)?)))
                    .collect::<Result<_, E>>()?,
            },
            Item::Arr { members } => Item::Arr {
                members: members.into_iter().map(f).collect::<Result<_, E>>()?,
            },
            Item::Yield { value, delegate } => Item::Yield {
                value: match value {
                    None => None,
                    Some(a) => Some(f(a)?),
                },
                delegate: delegate,
            },
            Item::Await { value } => Item::Await { value: f(value)? },
            Item::Undef => Item::Undef,
        })
    }
}
#[derive(Clone)]
pub enum LId<I = Ident> {
    Id { id: I },
    Member { obj: I, mem: I },
}
impl<I> LId<I> {
    pub fn map<J, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<LId<J>, E> {
        Ok(match self {
            LId::Id { id } => LId::Id { id: f(id)? },
            LId::Member { obj, mem } => LId::Member {
                obj: f(obj)?,
                mem: f(mem)?,
            },
        })
    }
}
pub struct Trans {
    pub map: BTreeMap<Id<Block>, Id<TBlock>>,
}
impl Trans {
    pub fn trans(&mut self, i: &Cfg, o: &mut TCfg, b: Id<Block>) -> anyhow::Result<Id<TBlock>> {
        loop {
            if let Some(a) = self.map.get(&b) {
                return Ok(*a);
            }
            let t = o.blocks.alloc(TBlock {
                stmts: vec![],
                catch: TCatch::Throw,
                term: Default::default(),
            });
            self.map.insert(b, t);
            if let Catch::Jump { pat, k } = &i.blocks[b].catch {
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
            let term = match &i.blocks[b].term {
                swc_cfg::Term::Return(expr) => match expr {
                    None => TTerm::Return(None),
                    Some(a) => {
                        let c;
                        (c, t) = self.expr(i, o, b, t, a)?;
                        TTerm::Return(Some(c))
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
                    o.blocks[t].stmts.push((
                        LId::Id {
                            id: f.ident.clone().into(),
                        },
                        Item::Func {
                            func: f.function.as_ref().clone().try_into()?,
                        },
                    ));
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
                                    o.blocks[t].stmts.push((
                                        LId::Id {
                                            id: i2.id.clone().into(),
                                        },
                                        Item::Just { id: f },
                                    ));
                                    o.decls.insert(i2.id.clone().into());
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
        let e;
        (mem, t) = self.expr(
            i,
            o,
            b,
            t,
            match &s.prop {
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
                swc_ecma_ast::MemberProp::Computed(computed_prop_name) => &computed_prop_name.expr,
            },
        )?;
        let v = o.regs.alloc(());
        o.blocks[t]
            .stmts
            .push((LId::Id { id: v.clone() }, Item::Mem { obj, mem }));
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
        match s {
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
                                o.blocks[t].stmts.push((
                                    LId::Id {
                                        id: i.id.clone().into(),
                                    },
                                    item,
                                ));
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
                                        o.blocks[t].stmts.push((
                                            LId::Id { id: id.clone() },
                                            Item::Mem {
                                                obj: obj.clone(),
                                                mem: mem.clone(),
                                            },
                                        ));
                                        Item::Bin {
                                            left: right,
                                            right: id,
                                            op: a,
                                        }
                                    }
                                };
                                o.blocks[t].stmts.push((
                                    LId::Member {
                                        obj: obj.clone(),
                                        mem: mem.clone(),
                                    },
                                    item,
                                ));
                                right = o.regs.alloc(());
                                o.blocks[t]
                                    .stmts
                                    .push((LId::Id { id: right.clone() }, Item::Mem { obj, mem }));
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
            Expr::Bin(bin) => {
                let left;
                let right;
                (left, t) = self.expr(i, o, b, t, &bin.left)?;
                (right, t) = self.expr(i, o, b, t, &bin.right)?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push((
                    LId::Id { id: tmp.clone() },
                    Item::Bin {
                        left,
                        right,
                        op: bin.op.clone(),
                    },
                ));
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Unary(un) => {
                if un.op == UnaryOp::Void {
                    let tmp = o.regs.alloc(());
                    o.blocks[t]
                        .stmts
                        .push((LId::Id { id: tmp.clone() }, Item::Undef));
                    o.decls.insert(tmp.clone());
                    return Ok((tmp, t));
                }
                let arg;
                // let right;
                (arg, t) = self.expr(i, o, b, t, &un.arg)?;
                // (right, t) = self.expr(i, o, b, t, &bin.right)?;
                let tmp = o.regs.alloc(());
                o.blocks[t].stmts.push((
                    LId::Id { id: tmp.clone() },
                    Item::Un {
                        arg,
                        op: un.op.clone(),
                    },
                ));
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Member(m) => return self.member_expr(i, o, b, t, m),
            Expr::Lit(l) => {
                let tmp = o.regs.alloc(());
                o.blocks[t]
                    .stmts
                    .push((LId::Id { id: tmp.clone() }, Item::Lit { lit: l.clone() }));
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Fn(f) => {
                let tmp = match &f.ident {
                    Some(a) => a.clone().into(),
                    None => o.regs.alloc(()),
                };
                o.blocks[t].stmts.push((
                    LId::Id { id: tmp.clone() },
                    Item::Func {
                        func: f.function.as_ref().clone().try_into()?,
                    },
                ));
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
                o.blocks[t]
                    .stmts
                    .push((LId::Id { id: tmp.clone() }, Item::Arr { members }));
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
                    .collect::<anyhow::Result<BTreeMap<_, _>>>()?;
                let tmp = o.regs.alloc(());
                o.blocks[t]
                    .stmts
                    .push((LId::Id { id: tmp.clone() }, Item::Obj { members }));
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            Expr::Await(x) => {
                let (a, t) = self.expr(i, o, b, t, &x.arg)?;
                o.blocks[t]
                    .stmts
                    .push((LId::Id { id: a.clone() }, Item::Await { value: a.clone() }));
                return Ok((a, t));
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
                o.blocks[t].stmts.push((
                    LId::Id { id: tmp.clone() },
                    Item::Yield {
                        value: y2,
                        delegate: y.delegate,
                    },
                ));
                o.decls.insert(tmp.clone());
                return Ok((tmp, t));
            }
            _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
        }
    }
}
