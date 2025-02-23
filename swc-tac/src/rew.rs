use std::collections::BTreeMap;

use id_arena::Id;
use swc_cfg::{Block, Cfg};
use swc_cfg::{Func, Term};
use swc_common::{Span, SyntaxContext};
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
use swc_ecma_ast::{BinExpr, BindingIdent, TsTypeAnn};

use crate::{TBlock, TCallee, TCfg, TFunc};

impl TryFrom<TFunc> for Func {
    type Error = anyhow::Error;

    fn try_from(value: TFunc) -> Result<Self, Self::Error> {
        let mut cfg = Cfg::default();
        let entry = Rew {
            all: BTreeMap::new(),
        }
        .trans(&mut cfg, &value.cfg, value.entry)?;
        let span = Span::dummy_with_cmt();
        let params = value
            .params
            .iter()
            .zip(value.ts_params.iter())
            .map(|(a, t)| Param {
                span: span,
                decorators: vec![],
                pat: Pat::Ident(BindingIdent {
                    id: i(a, span),
                    type_ann: t.as_ref().map(|a| {
                        Box::new(TsTypeAnn {
                            span: span,
                            type_ann: Box::new(a.clone()),
                        })
                    }),
                }),
            })
            .collect::<Vec<_>>();
        let entry2 = cfg.blocks.alloc(Default::default());
        for d in value.cfg.decls.iter() {
            cfg.blocks[entry2]
                .stmts
                .push(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: span,
                    ctxt: d.1.clone(),
                    declare: false,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    decls: vec![VarDeclarator {
                        span: span,
                        name: Pat::Ident(BindingIdent {
                            id: i(d, span),
                            type_ann: value.cfg.type_annotations.get(d).map(|a| {
                                Box::new(TsTypeAnn {
                                    span: span,
                                    type_ann: Box::new(a.clone()),
                                })
                            }),
                        }),
                        init: None,
                        definite: false,
                    }],
                }))));
        }
        cfg.blocks[entry2].end.term = Term::Jmp(entry);
        cfg.ts_retty = value.cfg.ts_retty;
        cfg.generics = value.cfg.generics;
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
            cfg.blocks[l].end.orig_span = tcfg.blocks[k].orig_span.clone();
            self.all.insert(k, l);
            let catch = match &tcfg.blocks[k].catch {
                crate::TCatch::Throw => swc_cfg::Catch::Throw,
                crate::TCatch::Jump { pat, k } => swc_cfg::Catch::Jump {
                    pat: Pat::Ident(swc_ecma_ast::BindingIdent {
                        id: i(pat, Span::dummy_with_cmt()),
                        type_ann: None,
                    }),
                    k: self.trans(cfg, tcfg, *k)?,
                },
            };
            cfg.blocks[l].end.catch = catch;
            for i2 in tcfg.blocks[k].stmts.iter() {
                let span = i2.3;
                let left = match &i2.0 {
                    crate::LId::Id { id } => swc_ecma_ast::AssignTarget::Simple(
                        swc_ecma_ast::SimpleAssignTarget::Ident(swc_ecma_ast::BindingIdent {
                            id: i(id, span),
                            type_ann: None,
                        }),
                    ),
                    crate::LId::Member { obj, mem } => {
                        let obj = i(obj, span);
                        let mem = i(mem, span);
                        AssignTarget::Simple(swc_ecma_ast::SimpleAssignTarget::Member(MemberExpr {
                            span: span,
                            obj: Box::new(Expr::Ident(obj)),
                            prop: swc_ecma_ast::MemberProp::Computed(
                                swc_ecma_ast::ComputedPropName {
                                    span: span,
                                    expr: Box::new(Expr::Ident(mem)),
                                },
                            ),
                        }))
                    }
                };
                let right = Box::new(match &i2.2 {
                    crate::Item::Just { id } => Expr::Ident(i(id, span)),
                    crate::Item::Bin { left, right, op } => Expr::Bin(BinExpr {
                        span: span,
                        op: *op,
                        left: Box::new(Expr::Ident(i(left, span))),
                        right: Box::new(Expr::Ident(i(right, span))),
                    }),
                    crate::Item::Un { arg, op } => Expr::Unary(UnaryExpr {
                        span: span,
                        op: *op,
                        arg: Box::new(Expr::Ident(i(arg, span))),
                    }),
                    crate::Item::Mem { obj, mem } => {
                        let obj = i(obj, span);
                        let mem = i(mem, span);
                        Expr::Member(MemberExpr {
                            span: span,
                            obj: Box::new(Expr::Ident(obj)),
                            prop: swc_ecma_ast::MemberProp::Computed(
                                swc_ecma_ast::ComputedPropName {
                                    span: span,
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
                    crate::Item::Call { callee, args } => Expr::Call(CallExpr {
                        span: span,
                        ctxt: SyntaxContext::empty(),
                        callee: swc_ecma_ast::Callee::Expr(match callee {
                            crate::TCallee::Member { r#fn, member } => {
                                let f = Box::new(Expr::Ident(i(r#fn, span)));
                                Box::new(Expr::Member(MemberExpr {
                                    span: span,
                                    obj: f,
                                    prop: swc_ecma_ast::MemberProp::Computed(ComputedPropName {
                                        span: span,
                                        expr: Box::new(Expr::Ident(i(member, span))),
                                    }),
                                }))
                            }
                            crate::TCallee::Static(r#fn) | TCallee::Val(r#fn) => {
                                let f = Box::new(Expr::Ident(i(r#fn, span)));
                                f
                            }
                        }),
                        args: args
                            .iter()
                            .map(|a| swc_ecma_ast::ExprOrSpread {
                                spread: None,
                                expr: Box::new(Expr::Ident(i(a, span))),
                            })
                            .collect(),
                        type_args: None,
                    }),
                    crate::Item::Obj { members } => Expr::Object(ObjectLit {
                        span: span,
                        props: members
                            .iter()
                            .map(|a| {
                                PropOrSpread::Prop({
                                    let b = i(&a.1, span);
                                    let b = Box::new(Expr::Ident(b));
                                    Box::new(match &a.0 {
                                        crate::PropKey::Lit(l) => Prop::KeyValue(KeyValueProp {
                                            key: swc_ecma_ast::PropName::Ident(
                                                swc_ecma_ast::IdentName {
                                                    span: span,
                                                    sym: l.0.clone(),
                                                },
                                            ),
                                            value: b,
                                        }),
                                        crate::PropKey::Computed(c) => {
                                            Prop::KeyValue(KeyValueProp {
                                                key: swc_ecma_ast::PropName::Computed(
                                                    ComputedPropName {
                                                        span: span,
                                                        expr: Box::new(Expr::Ident(i(c, span))),
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
                        span: span,
                        elems: members
                            .iter()
                            .map(|a| {
                                Some(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(Expr::Ident(i(a, span))),
                                })
                            })
                            .collect(),
                    }),
                    crate::Item::Yield { value, delegate } => {
                        Expr::Yield(swc_ecma_ast::YieldExpr {
                            span: span,
                            arg: value.as_ref().map(|x| Box::new(Expr::Ident(i(x, span)))),
                            delegate: *delegate,
                        })
                    }
                    crate::Item::Await { value } => Expr::Await(swc_ecma_ast::AwaitExpr {
                        span: span,
                        arg: Box::new(Expr::Ident(i(value, span))),
                    }),
                    crate::Item::Undef => *Expr::undefined(span),
                });
                cfg.blocks[l].stmts.push(Stmt::Expr(ExprStmt {
                    span: span,
                    expr: Box::new(Expr::Assign(AssignExpr {
                        span: span,
                        op: swc_ecma_ast::AssignOp::Assign,
                        left,
                        right,
                    })),
                }));
            }
            let term = match &tcfg.blocks[k].term {
                crate::TTerm::Return(r) => Term::Return(
                    r.as_ref()
                        .map(|v| {
                            i(
                                v,
                                tcfg.blocks[k]
                                    .orig_span
                                    .clone()
                                    .unwrap_or(Span::dummy_with_cmt()),
                            )
                        })
                        .map(Expr::Ident),
                ),
                crate::TTerm::Throw(x) => Term::Throw(Expr::Ident(i(
                    x,
                    tcfg.blocks[k]
                        .orig_span
                        .clone()
                        .unwrap_or(Span::dummy_with_cmt()),
                ))),
                crate::TTerm::Jmp(id) => Term::Jmp(self.trans(cfg, tcfg, *id)?),
                crate::TTerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => Term::CondJmp {
                    cond: Expr::Ident(i(
                        cond,
                        tcfg.blocks[k]
                            .orig_span
                            .clone()
                            .unwrap_or(Span::dummy_with_cmt()),
                    )),
                    if_true: self.trans(cfg, tcfg, *if_true)?,
                    if_false: self.trans(cfg, tcfg, *if_false)?,
                },
                crate::TTerm::Switch { x, blocks, default } => Term::Switch {
                    x: Expr::Ident(i(
                        x,
                        tcfg.blocks[k]
                            .orig_span
                            .clone()
                            .unwrap_or(Span::dummy_with_cmt()),
                    )),
                    blocks: blocks
                        .iter()
                        .map(|a| {
                            anyhow::Ok((
                                Expr::Ident(i(
                                    a.0,
                                    tcfg.blocks[k]
                                        .orig_span
                                        .clone()
                                        .unwrap_or(Span::dummy_with_cmt()),
                                )),
                                self.trans(cfg, tcfg, *a.1)?,
                            ))
                        })
                        .collect::<anyhow::Result<_>>()?,
                    default: self.trans(cfg, tcfg, *default)?,
                },
                crate::TTerm::Default => Term::Default,
            };
            cfg.blocks[l].end.term = term;
        }
    }
}
pub fn i(a: &Ident, span: Span) -> swc_ecma_ast::Ident {
    swc_ecma_ast::Ident {
        span: span,
        ctxt: a.1.clone(),
        sym: a.0.clone(),
        optional: false,
    }
}
