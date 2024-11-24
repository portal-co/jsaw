use std::collections::BTreeMap;

use id_arena::Id;
use swc_cfg::{Block, Cfg};
use swc_cfg::{Func, Term};
use swc_common::Span;
use swc_ecma_ast::BinExpr;
use swc_ecma_ast::CallExpr;
use swc_ecma_ast::ComputedPropName;
use swc_ecma_ast::Expr;
use swc_ecma_ast::ExprOrSpread;
use swc_ecma_ast::ExprStmt;
use swc_ecma_ast::FnExpr;
use swc_ecma_ast::Id as Ident;
use swc_ecma_ast::KeyValueProp;
use swc_ecma_ast::MemberExpr;
use swc_ecma_ast::ObjectLit;
use swc_ecma_ast::Pat;
use swc_ecma_ast::Prop;
use swc_ecma_ast::PropOrSpread;
use swc_ecma_ast::Stmt;
use swc_ecma_ast::UnaryExpr;
use swc_ecma_ast::{ArrayLit, Param};
use swc_ecma_ast::{AssignExpr, Decl, VarDecl, VarDeclarator};
use swc_ecma_ast::{AssignTarget, Function};

use crate::{TBlock, TCfg, TFunc};

impl TryFrom<TFunc> for Func {
    type Error = anyhow::Error;

    fn try_from(value: TFunc) -> Result<Self, Self::Error> {
        let mut cfg = Cfg::default();
        let entry = Rew {
            all: BTreeMap::new(),
        }
        .trans(&mut cfg, &value.cfg, value.entry)?;
        let params = value
            .params
            .iter()
            .map(|a| Param {
                span: Span::dummy_with_cmt(),
                decorators: vec![],
                pat: Pat::Ident(i(a).into()),
            })
            .collect::<Vec<_>>();
        let entry2 = cfg.blocks.alloc(Default::default());
        for d in value.cfg.decls.iter() {
            cfg.blocks[entry2]
                .stmts
                .push(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: Span::dummy_with_cmt(),
                    ctxt: d.1.clone(),
                    declare: false,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    decls: vec![VarDeclarator {
                        span: Span::dummy_with_cmt(),
                        name: Pat::Ident(i(d).into()),
                        init: None,
                        definite: false,
                    }],
                }))));
        }
        cfg.blocks[entry2].end.term = Term::Jmp(entry);
        Ok(Func {
            cfg,
            entry: entry2,
            params,
            is_generator: value.is_generator,
            is_async: value.is_async,
        })
    }
}
impl TryFrom<TFunc> for Function {
    type Error = anyhow::Error;

    fn try_from(value: TFunc) -> Result<Self, Self::Error> {
        let a: Func = value.try_into()?;
        return Ok(a.into());
    }
}

pub struct Rew {
    pub all: BTreeMap<Id<TBlock>, Id<Block>>,
}
impl Rew {
    pub fn trans(
        &mut self,
        cfg: &mut Cfg,
        tcfg: &TCfg,
        k: Id<TBlock>,
    ) -> anyhow::Result<Id<Block>> {
        loop {
            if let Some(x) = self.all.get(&k) {
                return Ok(*x);
            }
            let l = cfg.blocks.alloc(Default::default());
            self.all.insert(k, l);
            let catch = match &tcfg.blocks[k].catch {
                crate::TCatch::Throw => swc_cfg::Catch::Throw,
                crate::TCatch::Jump { pat, k } => swc_cfg::Catch::Jump {
                    pat: Pat::Ident(swc_ecma_ast::BindingIdent {
                        id: i(pat),
                        type_ann: None,
                    }),
                    k: self.trans(cfg, tcfg, *k)?,
                },
            };
            cfg.blocks[l].end.catch = catch;
            for i2 in tcfg.blocks[k].stmts.iter() {
                let left = match &i2.0 {
                    crate::LId::Id { id } => swc_ecma_ast::AssignTarget::Simple(
                        swc_ecma_ast::SimpleAssignTarget::Ident(swc_ecma_ast::BindingIdent {
                            id: i(id),
                            type_ann: None,
                        }),
                    ),
                    crate::LId::Member { obj, mem } => {
                        let obj = i(obj);
                        let mem = i(mem);
                        AssignTarget::Simple(swc_ecma_ast::SimpleAssignTarget::Member(MemberExpr {
                            span: Span::dummy_with_cmt(),
                            obj: Box::new(Expr::Ident(obj)),
                            prop: swc_ecma_ast::MemberProp::Computed(
                                swc_ecma_ast::ComputedPropName {
                                    span: Span::dummy_with_cmt(),
                                    expr: Box::new(Expr::Ident(mem)),
                                },
                            ),
                        }))
                    }
                };
                let right = Box::new(match &i2.1 {
                    crate::Item::Just { id } => Expr::Ident(i(id)),
                    crate::Item::Bin { left, right, op } => Expr::Bin(BinExpr {
                        span: Span::dummy_with_cmt(),
                        op: *op,
                        left: Box::new(Expr::Ident(i(left))),
                        right: Box::new(Expr::Ident(i(right))),
                    }),
                    crate::Item::Un { arg, op } => Expr::Unary(UnaryExpr {
                        span: Span::dummy_with_cmt(),
                        op: *op,
                        arg: Box::new(Expr::Ident(i(arg))),
                    }),
                    crate::Item::Mem { obj, mem } => {
                        let obj = i(obj);
                        let mem = i(mem);
                        Expr::Member(MemberExpr {
                            span: Span::dummy_with_cmt(),
                            obj: Box::new(Expr::Ident(obj)),
                            prop: swc_ecma_ast::MemberProp::Computed(
                                swc_ecma_ast::ComputedPropName {
                                    span: Span::dummy_with_cmt(),
                                    expr: Box::new(Expr::Ident(mem)),
                                },
                            ),
                        })
                    }
                    crate::Item::Func { func } => Expr::Fn(FnExpr {
                        ident: None,
                        function: Box::new(func.clone().try_into()?),
                    }),
                    crate::Item::Lit { lit } => Expr::Lit(lit.clone()),
                    crate::Item::Call { r#fn, member, args } => {
                        let f = Box::new(Expr::Ident(i(r#fn)));
                        Expr::Call(CallExpr {
                            span: Span::dummy_with_cmt(),
                            ctxt: r#fn.1.clone(),
                            callee: swc_ecma_ast::Callee::Expr(match member {
                                Some(m) => Box::new(Expr::Member(MemberExpr {
                                    span: Span::dummy_with_cmt(),
                                    obj: f,
                                    prop: swc_ecma_ast::MemberProp::Computed(ComputedPropName {
                                        span: Span::dummy_with_cmt(),
                                        expr: Box::new(Expr::Ident(i(m))),
                                    }),
                                })),
                                None => f,
                            }),
                            args: args
                                .iter()
                                .map(|a| swc_ecma_ast::ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(Expr::Ident(i(a))),
                                })
                                .collect(),
                            type_args: None,
                        })
                    }
                    crate::Item::Obj { members } => Expr::Object(ObjectLit {
                        span: Span::dummy_with_cmt(),
                        props: members
                            .iter()
                            .map(|a| {
                                PropOrSpread::Prop({
                                    let b = i(&a.1);
                                    let b = Box::new(Expr::Ident(b));
                                    Box::new(match &a.0 {
                                        crate::PropKey::Lit(l) => Prop::KeyValue(KeyValueProp {
                                            key: swc_ecma_ast::PropName::Ident(
                                                swc_ecma_ast::IdentName {
                                                    span: Span::dummy_with_cmt(),
                                                    sym: l.0.clone(),
                                                },
                                            ),
                                            value: b,
                                        }),
                                        crate::PropKey::Computed(c) => {
                                            Prop::KeyValue(KeyValueProp {
                                                key: swc_ecma_ast::PropName::Computed(
                                                    ComputedPropName {
                                                        span: Span::dummy_with_cmt(),
                                                        expr: Box::new(Expr::Ident(i(c))),
                                                    },
                                                ),
                                                value: b,
                                            })
                                        }
                                    })
                                })
                            })
                            .collect(),
                    }),
                    crate::Item::Arr { members } => Expr::Array(ArrayLit {
                        span: Span::dummy_with_cmt(),
                        elems: members
                            .iter()
                            .map(|a| {
                                Some(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(Expr::Ident(i(a))),
                                })
                            })
                            .collect(),
                    }),
                    crate::Item::Yield { value, delegate } => {
                        Expr::Yield(swc_ecma_ast::YieldExpr {
                            span: Span::dummy_with_cmt(),
                            arg: value.as_ref().map(|x| Box::new(Expr::Ident(i(x)))),
                            delegate: *delegate,
                        })
                    }
                    crate::Item::Await { value } => Expr::Await(swc_ecma_ast::AwaitExpr {
                        span: Span::dummy_with_cmt(),
                        arg: Box::new(Expr::Ident(i(value))),
                    }),
                    crate::Item::Undef => *Expr::undefined(Span::dummy_with_cmt()),
                });
                cfg.blocks[l].stmts.push(Stmt::Expr(ExprStmt {
                    span: Span::dummy_with_cmt(),
                    expr: Box::new(Expr::Assign(AssignExpr {
                        span: Span::dummy_with_cmt(),
                        op: swc_ecma_ast::AssignOp::Assign,
                        left,
                        right,
                    })),
                }));
            }
            let term = match &tcfg.blocks[k].term {
                crate::TTerm::Return(r) => Term::Return(r.as_ref().map(i).map(Expr::Ident)),
                crate::TTerm::Throw(x) => Term::Throw(Expr::Ident(i(x))),
                crate::TTerm::Jmp(id) => Term::Jmp(self.trans(cfg, tcfg, *id)?),
                crate::TTerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => Term::CondJmp {
                    cond: Expr::Ident(i(cond)),
                    if_true: self.trans(cfg, tcfg, *if_true)?,
                    if_false: self.trans(cfg, tcfg, *if_false)?,
                },
                crate::TTerm::Switch { x, blocks, default } => Term::Switch {
                    x: Expr::Ident(i(x)),
                    blocks: blocks
                        .iter()
                        .map(|a| anyhow::Ok((Expr::Ident(i(a.0)), self.trans(cfg, tcfg, *a.1)?)))
                        .collect::<anyhow::Result<_>>()?,
                    default: self.trans(cfg, tcfg, *default)?,
                },
                crate::TTerm::Default => Term::Default,
            };
            cfg.blocks[l].end.term = term;
        }
    }
}
pub fn i(a: &Ident) -> swc_ecma_ast::Ident {
    swc_ecma_ast::Ident {
        span: Span::dummy_with_cmt(),
        ctxt: a.1.clone(),
        sym: a.0.clone(),
        optional: false,
    }
}
