use std::collections::{BTreeMap, HashMap};

use arena_traits::Arena as TArena;
use id_arena::{Arena, Id};
use lam::LAM;
use swc_cfg::{Block, Catch, Cfg};
use swc_ecma_ast::{
    AssignOp, BinaryOp, Expr, Function, Ident, Lit, MemberExpr, Pat, SimpleAssignTarget, Stmt,
    UnaryOp,
};

pub mod lam;

pub struct TCfg {
    pub blocks: Arena<TBlock>,
    pub regs: LAM<()>,
}
pub struct TBlock {
    pub stmts: Vec<(LId, Item)>,
    pub catch: TCatch,
    pub term: TTerm,
}
#[derive(Clone)]
pub enum TCatch {
    Throw,
    Jump { pat: Ident, k: Id<TBlock> },
}
#[derive(Default)]
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
#[derive(Clone)]
pub enum Item<I = Ident> {
    Just { id: I },
    Bin { left: I, right: I, op: BinaryOp },
    Un { arg: I, op: UnaryOp },
    Mem { obj: I, mem: I },
    Func { func: Function },
    Lit { lit: Lit },
    Call { r#fn: I, args: Vec<I> },
}
impl<I> Item<I> {
    pub fn map<J, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<Item<J>, E> {
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
                            pat: id.id.clone(),
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
                            id: f.ident.clone(),
                        },
                        Item::Func {
                            func: f.function.as_ref().clone(),
                        },
                    ));
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
                                        LId::Id { id: i2.id.clone() },
                                        Item::Just { id: f },
                                    ));
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
                    e = Expr::Ident(Ident::new(
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
            Expr::Ident(i) => Ok((i.clone(), t)),
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
                                        right: i.id.clone(),
                                        op: a,
                                    },
                                };
                                o.blocks[t].stmts.push((LId::Id { id: i.id.clone() }, item));
                                right = i.id.clone();
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
                                            e = Expr::Ident(Ident::new(
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
                return Ok((tmp, t));
            }
            Expr::Unary(un) => {
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
                return Ok((tmp, t));
            }
            Expr::Member(m) => return self.member_expr(i, o, b, t, m),
            Expr::Lit(l) => {
                let tmp = o.regs.alloc(());
                o.blocks[t]
                    .stmts
                    .push((LId::Id { id: tmp.clone() }, Item::Lit { lit: l.clone() }));
                return Ok((tmp, t));
            }
            Expr::Fn(f) => {
                let tmp = match &f.ident {
                    Some(a) => a.clone(),
                    None => o.regs.alloc(()),
                };
                o.blocks[t].stmts.push((
                    LId::Id { id: tmp.clone() },
                    Item::Func {
                        func: f.function.as_ref().clone(),
                    },
                ));
                return Ok((tmp, t));
            }
            _ => anyhow::bail!("todo: {}:{}", file!(), line!()),
        }
    }
}
