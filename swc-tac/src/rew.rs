use std::collections::BTreeMap;

use id_arena::Id;
use swc_cfg::{Block, Cfg};
use swc_cfg::{Func, Term};
use swc_common::{Span, SyntaxContext};
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
use swc_ecma_ast::{BinaryOp, CallExpr, Lit, Number};
use swc_ecma_ast::{ComputedPropName, ThisExpr};

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
                    id: ident(a, span),
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
                            id: ident(d, span),
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
#[derive(Default)]
#[non_exhaustive]
pub struct Rew {
    pub all: BTreeMap<Id<TBlock>, Id<Block>>,
}
impl Rew {
    pub fn trans(
        &mut self,
        cfg: &mut Cfg,
        tcfg: &TCfg,
        block_id: Id<TBlock>,
    ) -> anyhow::Result<Id<Block>> {
        loop {
            if let Some(existing_block_id) = self.all.get(&block_id) {
                return Ok(*existing_block_id);
            }
            let new_block_id = cfg.blocks.alloc(Default::default());
            cfg.blocks[new_block_id].end.orig_span = tcfg.blocks[block_id].orig_span.clone();
            self.all.insert(block_id, new_block_id);
            let catch = match &tcfg.blocks[block_id].catch {
                crate::TCatch::Throw => swc_cfg::Catch::Throw,
                crate::TCatch::Jump { pat, k } => swc_cfg::Catch::Jump {
                    pat: Pat::Ident(swc_ecma_ast::BindingIdent {
                        id: ident(pat, Span::dummy_with_cmt()),
                        type_ann: None,
                    }),
                    k: self.trans(cfg, tcfg, *k)?,
                },
            };
            cfg.blocks[new_block_id].end.catch = catch;
            for statement_data in tcfg.blocks[block_id].stmts.iter() {
                let span = statement_data.span;
                let left = match &statement_data.left {
                    crate::LId::Id { id } => swc_ecma_ast::AssignTarget::Simple(
                        swc_ecma_ast::SimpleAssignTarget::Ident(swc_ecma_ast::BindingIdent {
                            id: ident(id, span),
                            type_ann: None,
                        }),
                    ),
                    crate::LId::Member { obj, mem } => {
                        let obj = ident(obj, span);
                        let mem = ident(&mem[0], span);
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
                    _ => todo!(),
                };
                let right = Box::new(match &statement_data.right {
                    crate::Item::Just { id } => Expr::Ident(ident(id, span)),
                    crate::Item::Bin { left, right, op } => Expr::Bin(BinExpr {
                        span: span,
                        op: *op,
                        left: Box::new(Expr::Ident(ident(left, span))),
                        right: Box::new(Expr::Ident(ident(right, span))),
                    }),
                    crate::Item::Un { arg, op } => Expr::Unary(UnaryExpr {
                        span: span,
                        op: *op,
                        arg: Box::new(Expr::Ident(ident(arg, span))),
                    }),
                    crate::Item::Mem { obj, mem } => {
                        let obj = ident(obj, span);
                        let mem = ident(mem, span);
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
                                let f = Box::new(Expr::Ident(ident(r#fn, span)));
                                Box::new(Expr::Member(MemberExpr {
                                    span: span,
                                    obj: f,
                                    prop: swc_ecma_ast::MemberProp::Computed(ComputedPropName {
                                        span: span,
                                        expr: Box::new(Expr::Ident(ident(member, span))),
                                    }),
                                }))
                            }
                            crate::TCallee::Static(r#fn) | TCallee::Val(r#fn) => {
                                let f = Box::new(Expr::Ident(ident(r#fn, span)));
                                f
                            }
                        }),
                        args: args
                            .iter()
                            .map(|a| swc_ecma_ast::ExprOrSpread {
                                spread: None,
                                expr: Box::new(Expr::Ident(ident(a, span))),
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
                                    let b = ident(&a.1, span);
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
                                                        expr: Box::new(Expr::Ident(ident(c, span))),
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
                                    expr: Box::new(Expr::Ident(ident(a, span))),
                                })
                            })
                            .collect(),
                    }),
                    crate::Item::Yield { value, delegate } => {
                        Expr::Yield(swc_ecma_ast::YieldExpr {
                            span: span,
                            arg: value
                                .as_ref()
                                .map(|yielded_value| Box::new(Expr::Ident(ident(yielded_value, span)))),
                            delegate: *delegate,
                        })
                    }
                    crate::Item::Await { value } => Expr::Await(swc_ecma_ast::AwaitExpr {
                        span: span,
                        arg: Box::new(Expr::Ident(ident(value, span))),
                    }),
                    crate::Item::Undef => *Expr::undefined(span),
                    crate::Item::Asm { value } => match value {
                        portal_jsc_common::Asm::OrZero(a) => Expr::Bin(BinExpr {
                            span,
                            op: BinaryOp::BitOr,
                            left: Box::new(Expr::Ident(ident(a, span))),
                            right: Box::new(Expr::Lit(Lit::Num(Number {
                                span,
                                value: 0.0,
                                raw: None,
                            }))),
                        }),
                        _ => todo!(),
                    },
                    crate::Item::This => Expr::This(ThisExpr { span }),
                });
                cfg.blocks[new_block_id].stmts.push(Stmt::Expr(ExprStmt {
                    span: span,
                    expr: Box::new(Expr::Assign(AssignExpr {
                        span: span,
                        op: swc_ecma_ast::AssignOp::Assign,
                        left,
                        right,
                    })),
                }));
            }
            let term = match &tcfg.blocks[block_id].term {
                crate::TTerm::Return(r) => Term::Return(
                    r.as_ref()
                        .map(|returned_value| {
                            ident(
                                returned_value,
                                tcfg.blocks[block_id]
                                    .orig_span
                                    .clone()
                                    .unwrap_or(Span::dummy_with_cmt()),
                            )
                        })
                        .map(Expr::Ident),
                ),
                crate::TTerm::Throw(x) => Term::Throw(Expr::Ident(ident(
                    x,
                    tcfg.blocks[block_id]
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
                    cond: Expr::Ident(ident(
                        cond,
                        tcfg.blocks[block_id]
                            .orig_span
                            .clone()
                            .unwrap_or(Span::dummy_with_cmt()),
                    )),
                    if_true: self.trans(cfg, tcfg, *if_true)?,
                    if_false: self.trans(cfg, tcfg, *if_false)?,
                },
                crate::TTerm::Switch { x, blocks, default } => Term::Switch {
                    x: Expr::Ident(ident(
                        x,
                        tcfg.blocks[block_id]
                            .orig_span
                            .clone()
                            .unwrap_or(Span::dummy_with_cmt()),
                    )),
                    blocks: blocks
                        .iter()
                        .map(|a| {
                            anyhow::Ok((
                                Expr::Ident(ident(
                                    a.0,
                                    tcfg.blocks[block_id]
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
            cfg.blocks[new_block_id].end.term = term;
        }
    }
}
fn ident(a: &Ident, span: Span) -> swc_ecma_ast::Ident {
    swc_ecma_ast::Ident {
        span: span,
        ctxt: a.1.clone(),
        sym: a.0.clone(),
        optional: false,
    }
}
