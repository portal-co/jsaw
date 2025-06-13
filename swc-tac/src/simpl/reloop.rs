use std::{convert::Infallible, iter::once};

use portal_jsc_simpl_js::{
    SimplAssignment, SimplBinOp, SimplCallExpr, SimplIf, SimplIfKind, SimplSelectExpr,
    SimplSwitchStmt,
};
use portal_jsc_swc_util::{BreakKind, MakeSpanned};
use relooper::ShapedBlock;
use swc_atoms::Atom;
use swc_ecma_ast::{AssignOp, Bool, Number};

use super::*;

pub fn reloop<D: TacDialect>(
    cfg: &TSimplCfg<D>,
    shaped_block: &ShapedBlock<Id<TSimplBlock<D>>>,
    span: Span,
    cff: &swc_ecma_ast::Ident,
) -> Vec<SimplStmt<D>> {
    match shaped_block {
        ShapedBlock::Simple(simple_block) => {
            let block = &cfg.blocks[simple_block.label];
            let span = match block.orig_span.clone() {
                None => span,
                Some(s) => s,
            };
            let generate_jump = |target_block_id: Id<TSimplBlock<D>>| {
                vec![SimplStmt::<D>::Expr(MakeSpanned {
                    value: Box::new(SimplExpr::Assign(MakeSpanned {
                        span,
                        value: SimplAssignment {
                            target: D::span(
                                Default::default(),
                                SimplPath {
                                    root: cff.clone(),
                                    keys: vec![],
                                },
                            ),
                            assign: AssignOp::Assign,
                            body: Box::new(SimplExpr::Lit(Lit::Num(Number {
                                span: span,
                                value: target_block_id.index() as u32 as f64,
                                raw: None,
                            }))),
                        },
                    })),
                    span: span,
                })]
                .into_iter()
                .chain(
                    match simple_block.branches.get(&target_block_id) {
                        None => vec![],
                        Some(branch_mode) => match branch_mode {
                            relooper::BranchMode::LoopBreak(l)
                            | relooper::BranchMode::LoopBreakIntoMulti(l) => {
                                vec![SimplStmt::Break(swc_ecma_ast::Ident::new(
                                    Atom::new(format!("${l}")),
                                    span,
                                    cff.ctxt,
                                ))]
                            }
                            relooper::BranchMode::LoopContinue(l)
                            | relooper::BranchMode::LoopContinueIntoMulti(l) => {
                                vec![SimplStmt::Continue(swc_ecma_ast::Ident::new(
                                    Atom::new(format!("${l}")),
                                    span,
                                    cff.ctxt,
                                ))]
                            }
                            relooper::BranchMode::MergedBranch => vec![],
                            relooper::BranchMode::MergedBranchIntoMulti => vec![],
                            relooper::BranchMode::SetLabelAndBreak => {
                                vec![SimplStmt::Break(cff.clone())]
                            }
                        },
                    }
                    .into_iter(),
                )
                .collect::<Vec<_>>()
            };
            let mut body = block
                .stmts
                .iter()
                .map(|(b, m, f, i, e)| {
                    let e = *e;
                    let loaded: SimplExpr<D> = match i {
                        SimplItem::Just { id } => {
                            SimplExpr::Ident(D::span(id.1.clone(), id.0.clone().span(e)))
                        }
                        SimplItem::Bin { left, right, op } => SimplExpr::Bin(MakeSpanned {
                            span,
                            value: SimplBinOp {
                                lhs: Box::new(SimplExpr::Ident(D::span(
                                    left.1.clone(),
                                    left.0.clone().span(e),
                                ))),
                                rhs: Box::new(SimplExpr::Ident(D::span(
                                    right.1.clone(),
                                    right.0.clone().span(e),
                                ))),
                                op: op.clone(),
                            },
                        }),
                        SimplItem::Lit { lit } => SimplExpr::Lit(lit.clone()),
                        SimplItem::CallStatic { r#fn, args } => SimplExpr::Call(MakeSpanned {
                            value: Box::new(SimplCallExpr::Path {
                                path: FuncId {
                                    path: D::span(r#fn.path.1.clone(), r#fn.path.0.clone().span(e)),
                                    template_args: r#fn.template_args.clone(),
                                },
                                args: args
                                    .iter()
                                    .map(|id| {
                                        SimplExpr::Ident(D::span(
                                            id.1.clone(),
                                            id.0.clone().span(e),
                                        ))
                                    })
                                    .collect(),
                            }),
                            span: span,
                        }),
                        SimplItem::CallTag { tag, args } => SimplExpr::Call(MakeSpanned {
                            value: Box::new(SimplCallExpr::Tag {
                                tag: tag.clone(),
                                args: args
                                    .iter()
                                    .map(|id| {
                                        SimplExpr::Ident(D::span(
                                            id.1.clone(),
                                            id.0.clone().span(e),
                                        ))
                                    })
                                    .collect(),
                            }),
                            span: span,
                        }),
                        SimplItem::DiscriminantIn { value, ids } => {
                            SimplExpr::Select(MakeSpanned {
                                value: SimplSelectExpr {
                                    scrutinee: Box::new(SimplExpr::Ident(D::span(
                                        value.1.clone(),
                                        value.0.clone().span(e),
                                    ))),
                                    cases: ids
                                        .iter()
                                        .enumerate()
                                        .map(|(i, (a, b))| {
                                            (
                                                a.clone(),
                                                (
                                                    vec![SimplStmt::Return(MakeSpanned {
                                                        value: Box::new(SimplExpr::Lit(Lit::Num(
                                                            Number {
                                                                span: e,
                                                                raw: None,
                                                                value: i as u32 as f64,
                                                            },
                                                        ))),
                                                        span: e,
                                                    })],
                                                    b.iter()
                                                        .map(|c| c.0.root.clone())
                                                        .map(|(a, b)| swc_ecma_ast::Ident {
                                                            span: e,
                                                            ctxt: b,
                                                            sym: a,
                                                            optional: false,
                                                        })
                                                        .collect(),
                                                ),
                                            )
                                        })
                                        .collect(),
                                },
                                span: span,
                            })
                        }
                    };
                    SimplStmt::Expr(MakeSpanned {
                        value: Box::new(SimplExpr::Assign(MakeSpanned {
                            value: SimplAssignment {
                                target: D::span(m.clone(), b.clone().span(e)),
                                assign: AssignOp::Assign,
                                body: Box::new(loaded),
                            },
                            span: e,
                        })),
                        span: span,
                    })
                })
                .collect::<Vec<SimplStmt<D>>>();
            match &block.term {
                TSimplTerm::Return(r) => {
                    let r = match r {
                        id => SimplExpr::<D>::Ident(D::span(id.1.clone(), id.0.clone().span(span))),
                    };
                    body.push(SimplStmt::Return(MakeSpanned {
                        value: Box::new(r),
                        span: span,
                    }));
                }
                TSimplTerm::Jmp(id) => body.extend(generate_jump(*id)),
                TSimplTerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => {
                    let cond = match cond {
                        id => SimplExpr::<D>::Ident(D::span(id.1.clone(), id.0.clone().span(span))),
                    };
                    body.push(SimplStmt::If(MakeSpanned {
                        value: SimplIf {
                            kind: SimplIfKind::If {
                                r#else: generate_jump(*if_false),
                            },
                            cond: Box::new(cond),
                            body: generate_jump(*if_true),
                        },
                        span: span,
                    }))
                }
                TSimplTerm::Select { scrutinee, cases } => {
                    let val = SimplExpr::<D>::Select(MakeSpanned {
                        value: SimplSelectExpr {
                            scrutinee: Box::new(SimplExpr::Ident(D::span(
                                scrutinee.1.clone(),
                                scrutinee.0.clone().span(span),
                            ))),
                            cases: cases
                                .iter()
                                .enumerate()
                                .map(|(i, (a, (d, b)))| {
                                    (
                                        a.clone(),
                                        (
                                            generate_jump(*d),
                                            b.iter()
                                                .map(|c| c.0.root.clone())
                                                .map(|(a, b)| swc_ecma_ast::Ident {
                                                    span: span,
                                                    ctxt: b,
                                                    sym: a,
                                                    optional: false,
                                                })
                                                .collect(),
                                        ),
                                    )
                                })
                                .collect(),
                        },
                        span: span,
                    });
                    body.push(SimplStmt::Expr(MakeSpanned {
                        value: Box::new(val),
                        span: span,
                    }))
                }
                TSimplTerm::Default => {}
                TSimplTerm::Switch { scrutinee, cases } => {
                    let scrutinee = match scrutinee {
                        id => SimplExpr::<D>::Ident(D::span(id.1.clone(), id.0.clone().span(span))),
                    };
                    body.push(SimplStmt::Switch(MakeSpanned {
                        value: SimplSwitchStmt {
                            scrutinee: Box::new(scrutinee),
                            cases: cases
                                .iter()
                                .map(|(cond, k)| {
                                    let cond = match cond {
                                        id => SimplExpr::<D>::Ident(D::span(
                                            id.1.clone(),
                                            id.0.clone().span(span),
                                        )),
                                    };
                                    (Box::new(cond), generate_jump(*k), BreakKind::BreakAfter)
                                })
                                .collect(),
                            label: swc_ecma_ast::Ident::new_private(Atom::new("$"), span),
                        },
                        span: span,
                    }));
                }
            };
            for a in [&simple_block.immediate, &simple_block.next]
                .into_iter()
                .flat_map(|a| a.as_ref())
            {
                body.extend(reloop(cfg, &*a, span, cff));
            }

            body
        }
        ShapedBlock::Loop(loop_block) => {
            let mut body = vec![SimplStmt::If(MakeSpanned {
                value: SimplIf {
                    kind: SimplIfKind::While {
                        label: swc_ecma_ast::Ident::new(
                            Atom::new(format!("${}", loop_block.loop_id)),
                            cff.span,
                            cff.ctxt,
                        ),
                    },
                    cond: Box::new(SimplExpr::Lit(Lit::Bool(Bool {
                        span: cff.span,
                        value: true,
                    }))),
                    body: reloop(cfg, &*loop_block.inner, span, cff),
                },
                span: cff.span,
            })];
            for a in [&loop_block.next].into_iter().flat_map(|a| a.as_ref()) {
                body.extend(reloop(cfg, &*a, span, cff));
            }

            body
        }
        ShapedBlock::Multiple(multiple_block) => {
            let mut body = vec![SimplStmt::<D>::Switch(MakeSpanned {
                value: SimplSwitchStmt {
                    scrutinee: Box::new(SimplExpr::Ident(D::span(
                        Default::default(),
                        SimplPath {
                            root: cff.clone(),
                            keys: vec![],
                        },
                    ))),
                    label: cff.clone(),
                    cases: multiple_block
                        .handled
                        .iter()
                        .flat_map(|i| {
                            let j = || reloop(cfg, &i.inner, span, cff);
                            i.labels.iter().map(move |l| {
                                (
                                    Box::new(SimplExpr::Lit(Lit::Num(Number {
                                        span,
                                        raw: None,
                                        value: l.index() as u32 as f64,
                                    }))),
                                    j(),
                                    if i.break_after {
                                        BreakKind::BreakAfter
                                    } else {
                                        BreakKind::DoNotBreakAfter
                                    },
                                )
                            })
                        })
                        .collect(),
                },
                span: span,
            })];
            // .fold(vec![], |mut v, i| {
            //     let j = || reloop(cfg, &i.inner, span, cff);
            //     if !i.break_after {
            //         // v.extend(i.labels.iter().fold(vec![], |v, l| {
            //         //     vec![SimplStmt::If(MakeSpanned {
            //         //         span,
            //         //         value: SimplIf {
            //         //             kind: SimplIfKind::If { r#else: v },
            //         //             cond: Box::new(SimplExpr::Bin(MakeSpanned {
            //         //                 span,
            //         //                 value: SimplBinOp {
            //         //                     lhs: Box::new(SimplExpr::Lit(Lit::Num(
            //         //                         Number {
            //         //                             span,
            //         //                             raw: None,
            //         //                             value: l.index() as u32 as f64,
            //         //                         },
            //         //                     ))),
            //         //                     op: BinaryOp::EqEqEq,
            //         //                     rhs: Box::new(SimplExpr::Ident(D::span(
            //         //                         Default::default(),
            //         //                         SimplPath {
            //         //                             root: cff.clone(),
            //         //                             keys: vec![],
            //         //                         },
            //         //                     ))),
            //         //                 },
            //         //             })),
            //         //             body: j(),
            //         //         },
            //         //     })]
            //         // }));
            //         v.push(SimplStmt::Switch(Simple))
            //         v
            //     } else {
            //         i.labels.iter().fold(v, |v, l| {
            //             vec![SimplStmt::If(MakeSpanned {
            //                 span,
            //                 value: SimplIf {
            //                     kind: SimplIfKind::If { r#else: v },
            //                     cond: Box::new(SimplExpr::Bin(MakeSpanned {
            //                         span,
            //                         value: SimplBinOp {
            //                             lhs: Box::new(SimplExpr::Lit(Lit::Num(
            //                                 Number {
            //                                     span,
            //                                     raw: None,
            //                                     value: l.index() as u32 as f64,
            //                                 },
            //                             ))),
            //                             op: BinaryOp::EqEqEq,
            //                             rhs: Box::new(SimplExpr::Ident(D::span(
            //                                 Default::default(),
            //                                 SimplPath {
            //                                     root: cff.clone(),
            //                                     keys: vec![],
            //                                 },
            //                             ))),
            //                         },
            //                     })),
            //                     body: j(),
            //                 },
            //             })]
            //         })
            //     }
            // })

            body
        }
    }
}
