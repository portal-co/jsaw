use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    mem::{replace, take},
};

use base64::Engine;
use swc_atoms::Atom;
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceFile, SourceMap, Span, Spanned,
};
use swc_common::{Mark, SyntaxContext};
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignOp, AssignPat, AssignTarget, AssignTargetPat, BinExpr, BinaryOp,
    BindingIdent, BlockStmt, BlockStmtOrExpr, CallExpr, Callee, ComputedPropName, CondExpr, Decl,
    Expr, ExprStmt, Id, Ident, IfStmt, Lit, MemberExpr, MemberProp, Pat, ReturnStmt, SeqExpr,
    SimpleAssignTarget, Stmt, Str, ThrowStmt, UnaryExpr, UnaryOp, VarDecl, VarDeclKind,
    VarDeclarator,
};
use swc_ecma_parser::{Lexer, Parser, Syntax};
use swc_ecma_transforms_optimization::simplify::const_propagation::constant_propagation;
use swc_ecma_visit::{VisitMut, VisitMutWith};
pub struct SyntaxContextToMark {
    root: Mark,
    map: HashMap<(Mark, Mark), Mark>,
}
impl SyntaxContextToMark {
    pub fn new(root: Mark) -> Self {
        Self {
            root,
            map: Default::default(),
        }
    }
    pub fn of(&mut self, mut a: SyntaxContext) -> Mark {
        if a == SyntaxContext::empty() {
            return self.root;
        }
        let last = a.remove_mark();
        let next = self.of(a);
        let last = self
            .map
            .entry((last, next))
            .or_insert_with(|| Mark::fresh(next));
        return *last;
    }
}
pub struct Brightener {
    pub key: Atom,
    pub global: Atom,
    pub val: bool,
    pub pack: Option<Ident>,
}
impl VisitMut for Brightener {
    fn visit_mut_expr(&mut self, node: &mut swc_ecma_ast::Expr) {
        let mut did = true;
        while take(&mut did) {
            node.visit_mut_children_with(self);
            *node = match take(node) {
                Expr::Member(MemberExpr { span, obj, prop }) if self.pack.is_some() && self.val => {
                    match prop {
                        MemberProp::Ident(i) => {
                            if i.sym == self.key {
                                did = true;
                                match (|obj: Expr| {
                                    Expr::Cond(CondExpr {
                                        span,
                                        test: Box::new(Expr::Bin(BinExpr {
                                            span,
                                            op: BinaryOp::EqEqEq,
                                            left: Box::new(obj.clone()),
                                            right: Box::new(Expr::Ident(Ident::new(
                                                self.global.clone(),
                                                span,
                                                Default::default(),
                                            ))),
                                        })),
                                        cons: Box::new(Expr::Ident(self.pack.clone().unwrap())),
                                        alt: Box::new(Expr::Member(MemberExpr {
                                            span,
                                            obj: Box::new(obj),
                                            prop: MemberProp::Ident(i),
                                        })),
                                    })
                                }) {
                                    go => {
                                        if obj.idempotent() {
                                            go(*obj)
                                        } else {
                                            match SyntaxContext::empty().apply_mark(Mark::new()) {
                                                ctxt => Expr::Call(CallExpr {
                                                    span,
                                                    ctxt,
                                                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                                                        Expr::Arrow(ArrowExpr {
                                                            span,
                                                            ctxt,
                                                            params: vec![Pat::Assign(AssignPat {
                                                                span,
                                                                left: Box::new(Pat::Ident(
                                                                    BindingIdent {
                                                                        id: Ident::new(
                                                                            Atom::new("obj"),
                                                                            span,
                                                                            ctxt,
                                                                        ),
                                                                        type_ann: None,
                                                                    },
                                                                )),
                                                                right: obj,
                                                            })],
                                                            body: Box::new(
                                                                swc_ecma_ast::BlockStmtOrExpr::Expr(
                                                                    Box::new(go(Expr::Ident(
                                                                        Ident::new(
                                                                            Atom::new("obj"),
                                                                            span,
                                                                            ctxt,
                                                                        ),
                                                                    ))),
                                                                ),
                                                            ),
                                                            is_async: false,
                                                            is_generator: false,
                                                            type_params: None,
                                                            return_type: None,
                                                        }),
                                                    )),
                                                    args: vec![],
                                                    type_args: None,
                                                }),
                                            }
                                        }
                                    }
                                }
                            } else {
                                Expr::Member(MemberExpr {
                                    span,
                                    obj,
                                    prop: MemberProp::Ident(i),
                                })
                            }
                        }
                        MemberProp::PrivateName(p) => Expr::Member(MemberExpr {
                            span,
                            obj,
                            prop: MemberProp::PrivateName(p),
                        }),
                        MemberProp::Computed(mem) => match (|obj: Expr, mem: Expr| {
                            did = true;
                            let cond = Expr::Bin(BinExpr {
                                span,
                                op: BinaryOp::EqEqEq,
                                left: Box::new(mem.clone()),
                                right: Box::new(Expr::Lit(Lit::Str(Str {
                                    span,
                                    value: self.key.clone(),
                                    raw: None,
                                }))),
                            });
                            let mut cond = if obj.as_ident().is_some_and(|a| {
                                a.to_id() == (self.global.clone(), Default::default())
                            }) {
                                cond
                            } else {
                                Expr::Bin(BinExpr {
                                    span,
                                    op: BinaryOp::LogicalAnd,
                                    left: Box::new(cond),
                                    right: Box::new(Expr::Bin(BinExpr {
                                        span,
                                        op: BinaryOp::EqEqEq,
                                        left: Box::new(obj.clone()),
                                        right: Box::new(Expr::Ident(Ident::new(
                                            self.global.clone(),
                                            span,
                                            Default::default(),
                                        ))),
                                    })),
                                })
                            };
                            Expr::Cond(CondExpr {
                                span,
                                test: Box::new(cond),
                                cons: Box::new(Expr::Ident(self.pack.clone().unwrap())),
                                alt: Box::new(Expr::Member(MemberExpr {
                                    span,
                                    obj: Box::new(obj),
                                    prop: MemberProp::Computed(ComputedPropName {
                                        span,
                                        expr: Box::new(mem),
                                    }),
                                })),
                            })
                        }) {
                            mut go => match (obj.idempotent(), mem.expr.idempotent()) {
                                (true, true) => go(*obj, *mem.expr),
                                _ => match SyntaxContext::empty().apply_mark(Mark::new()) {
                                    ctxt => Expr::Call(CallExpr {
                                        span,
                                        ctxt,
                                        callee: swc_ecma_ast::Callee::Expr(Box::new(Expr::Arrow(
                                            ArrowExpr {
                                                span,
                                                ctxt,
                                                params: vec![
                                                    Pat::Assign(AssignPat {
                                                        span,
                                                        left: Box::new(Pat::Ident(BindingIdent {
                                                            type_ann: None,
                                                            id: Ident::new(
                                                                Atom::new("obj"),
                                                                span,
                                                                ctxt,
                                                            ),
                                                        })),
                                                        right: obj,
                                                    }),
                                                    Pat::Assign(AssignPat {
                                                        span,
                                                        left: Box::new(Pat::Ident(BindingIdent {
                                                            type_ann: None,
                                                            id: Ident::new(
                                                                Atom::new("mem"),
                                                                span,
                                                                ctxt,
                                                            ),
                                                        })),
                                                        right: mem.expr,
                                                    }),
                                                ],
                                                is_async: false,
                                                is_generator: false,
                                                type_params: None,
                                                return_type: None,
                                                body: Box::new(
                                                    swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(
                                                        {
                                                            let right = Expr::Ident(Ident::new(
                                                                Atom::new("obj"),
                                                                span,
                                                                ctxt,
                                                            ));
                                                            let left = Expr::Ident(Ident::new(
                                                                Atom::new("mem"),
                                                                span,
                                                                ctxt,
                                                            ));
                                                            go(left, right)
                                                        },
                                                    )),
                                                ),
                                            },
                                        ))),
                                        args: vec![],
                                        type_args: None,
                                    }),
                                },
                            },
                        },
                    }
                }
                Expr::Bin(BinExpr {
                    left,
                    span,
                    right,
                    op: BinaryOp::In,
                }) => match (|left: Expr, right: Expr| {
                    let cond = Expr::Bin(BinExpr {
                        span,
                        op: BinaryOp::EqEqEq,
                        left: Box::new(left.clone()),
                        right: Box::new(Expr::Lit(Lit::Str(Str {
                            span,
                            value: self.key.clone(),
                            raw: None,
                        }))),
                    });
                    let mut cond = if right
                        .as_ident()
                        .is_some_and(|a| a.to_id() == (self.global.clone(), Default::default()))
                    {
                        cond
                    } else {
                        Expr::Bin(BinExpr {
                            span,
                            op: BinaryOp::LogicalAnd,
                            left: Box::new(cond),
                            right: Box::new(Expr::Bin(BinExpr {
                                span,
                                op: BinaryOp::EqEqEq,
                                left: Box::new(right.clone()),
                                right: Box::new(Expr::Ident(Ident::new(
                                    self.global.clone(),
                                    span,
                                    Default::default(),
                                ))),
                            })),
                        })
                    };
                    if !self.val {
                        cond = Expr::Unary(UnaryExpr {
                            span,
                            op: swc_ecma_ast::UnaryOp::Bang,
                            arg: Box::new(cond),
                        });
                    };
                    Expr::Bin(BinExpr {
                        span,
                        op: if self.val {
                            BinaryOp::LogicalOr
                        } else {
                            BinaryOp::LogicalAnd
                        },
                        left: Box::new(cond),
                        right: Box::new(Expr::Bin(BinExpr {
                            span,
                            op: BinaryOp::In,
                            left: Box::new(left),
                            right: Box::new(right),
                        })),
                    })
                }) {
                    go => match (left.idempotent(), right.idempotent()) {
                        (true, true) => go(*left, *right),
                        _ => match SyntaxContext::empty().apply_mark(Mark::new()) {
                            ctxt => Expr::Call(CallExpr {
                                span,
                                ctxt,
                                callee: swc_ecma_ast::Callee::Expr(Box::new(Expr::Arrow(
                                    ArrowExpr {
                                        span,
                                        ctxt,
                                        params: vec![
                                            Pat::Assign(AssignPat {
                                                span,
                                                left: Box::new(Pat::Ident(BindingIdent {
                                                    type_ann: None,
                                                    id: Ident::new(Atom::new("obj"), span, ctxt),
                                                })),
                                                right: right,
                                            }),
                                            Pat::Assign(AssignPat {
                                                span,
                                                left: Box::new(Pat::Ident(BindingIdent {
                                                    type_ann: None,
                                                    id: Ident::new(Atom::new("mem"), span, ctxt),
                                                })),
                                                right: left,
                                            }),
                                        ],
                                        is_async: false,
                                        is_generator: false,
                                        type_params: None,
                                        return_type: None,
                                        body: Box::new(swc_ecma_ast::BlockStmtOrExpr::Expr(
                                            Box::new({
                                                let right = Expr::Ident(Ident::new(
                                                    Atom::new("obj"),
                                                    span,
                                                    ctxt,
                                                ));
                                                let left = Expr::Ident(Ident::new(
                                                    Atom::new("mem"),
                                                    span,
                                                    ctxt,
                                                ));
                                                go(left, right)
                                            }),
                                        )),
                                    },
                                ))),
                                args: vec![],
                                type_args: None,
                            }),
                        },
                    },
                },
                n => n,
            }
        }
    }
}
pub trait Idempotency {
    fn idempotent(&self) -> bool;
}
impl<T: Idempotency> Idempotency for Box<T> {
    fn idempotent(&self) -> bool {
        (&**self).idempotent()
    }
}
impl Idempotency for Expr {
    fn idempotent(&self) -> bool {
        match self {
            Expr::Ident(_) | Expr::Lit(_) | Expr::This(_) => true,
            Expr::Assign(AssignExpr {
                span,
                op,
                left,
                right,
            }) => *op == AssignOp::Assign && left.idempotent() && right.idempotent(),
            _ => false,
        }
    }
}
impl Idempotency for AssignTarget {
    fn idempotent(&self) -> bool {
        match self {
            swc_ecma_ast::AssignTarget::Simple(simple_assign_target) => {
                match simple_assign_target {
                    SimpleAssignTarget::Ident(_) => true,
                    SimpleAssignTarget::Member(m) => match &m.prop {
                        MemberProp::PrivateName(_) => m.obj.idempotent(),
                        _ => false,
                    },
                    _ => false,
                }
            }
            swc_ecma_ast::AssignTarget::Pat(assign_target_pat) => match assign_target_pat {
                _ => false,
            },
        }
    }
}
struct CondWrapping{}
impl VisitMut for CondWrapping {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        *node = match take(node) {
            Expr::Cond(cond_expr) => {
                let mut call_expr = Expr::Call(CallExpr {
                    span: cond_expr.span,
                    ctxt: Default::default(),
                    callee: Callee::Expr(Box::new(Expr::Arrow(ArrowExpr {
                        span: cond_expr.span,
                        ctxt: Default::default(),
                        params: vec![],
                        body: Box::new(BlockStmtOrExpr::BlockStmt(BlockStmt {
                            span: cond_expr.span,
                            ctxt: Default::default(),
                            stmts: vec![Stmt::Return(ReturnStmt {
                                span: cond_expr.span,
                                arg: Some(Box::new(Expr::Cond(cond_expr))),
                            })],
                        })),
                        is_async: false,
                        is_generator: false,
                        type_params: None,
                        return_type: None,
                    }))),
                    args: vec![],
                    type_args: None,
                });
                match CondFolding::default() {
                    mut cond_folding => {
                        cond_folding.fold_stmts = true;
                        call_expr.visit_mut_with(&mut cond_folding);
                    }
                };
                call_expr
            }
            other => other,
        };
        node.visit_mut_children_with(self);
    }
}
#[derive(Default)]
#[non_exhaustive]
pub struct CondFolding {
    pub fold_stmts: bool,
}
impl VisitMut for CondFolding {
    fn visit_mut_stmt(&mut self, node: &mut Stmt) {
        let mut should_continue = true;
        while take(&mut should_continue) {
            node.visit_mut_children_with(self);
            if self.fold_stmts {
                *node = match take(node) {
                    Stmt::Expr(expr_stmt) if expr_stmt.expr.is_cond() => {
                        should_continue = true;
                        let Expr::Cond(cond_expr) = *expr_stmt.expr else {
                            unreachable!()
                        };
                        Stmt::If(IfStmt {
                            span: cond_expr.span,
                            test: cond_expr.test,
                            cons: Box::new(Stmt::Expr(ExprStmt {
                                span: expr_stmt.span,
                                expr: cond_expr.cons,
                            })),
                            alt: Some(Box::new(Stmt::Expr(ExprStmt {
                                span: expr_stmt.span,
                                expr: cond_expr.alt,
                            }))),
                        })
                    }
                    Stmt::Return(return_stmt) if return_stmt.arg.as_ref().is_some_and(|arg| arg.is_cond()) => {
                        should_continue = true;
                        let Expr::Cond(c) = *return_stmt.arg.unwrap() else {
                            unreachable!()
                        };
                        Stmt::If(IfStmt {
                            span: c.span,
                            test: c.test,
                            cons: Box::new(Stmt::Return(ReturnStmt {
                                span: return_stmt.span,
                                arg: Some(c.cons),
                            })),
                            alt: Some(Box::new(Stmt::Return(ReturnStmt {
                                span: return_stmt.span,
                                arg: Some(c.alt),
                            }))),
                        })
                    }
                    Stmt::Throw(e) if e.arg.is_cond() => {
                        should_continue = true;
                        let Expr::Cond(c) = *e.arg else {
                            unreachable!()
                        };
                        Stmt::If(IfStmt {
                            span: c.span,
                            test: c.test,
                            cons: Box::new(Stmt::Throw(ThrowStmt {
                                span: e.span,
                                arg: c.cons,
                            })),
                            alt: Some(Box::new(Stmt::Throw(ThrowStmt {
                                span: e.span,
                                arg: c.alt,
                            }))),
                        })
                    }
                    node => node,
                }
            }
        }
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        let mut cont = true;
        while take(&mut cont) {
            constant_propagation().visit_mut_expr(node);
            node.visit_mut_children_with(self);
            *node = match take(node) {
                Expr::Cond(CondExpr {
                    span,
                    test,
                    cons,
                    alt,
                }) => match *test {
                    Expr::Seq(mut sequence_expression) => {
                        let last = take(&mut **sequence_expression.exprs.last_mut().unwrap());
                        **sequence_expression.exprs.last_mut().unwrap() = Expr::Cond(CondExpr {
                            span,
                            test: Box::new(last),
                            cons,
                            alt,
                        });
                        cont = true;
                        Expr::Seq(sequence_expression)
                    }
                    Expr::Bin(binary_expression) if matches!(binary_expression.op, BinaryOp::LogicalAnd) => {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span,
                            test: binary_expression.left,
                            cons: Box::new(Expr::Cond(CondExpr {
                                span,
                                test: binary_expression.right,
                                cons,
                                alt: alt.clone(),
                            })),
                            alt,
                        })
                    }
                    Expr::Bin(binary_expression) if matches!(binary_expression.op, BinaryOp::LogicalOr) => {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span,
                            test: binary_expression.left,
                            cons: cons.clone(),
                            alt: Box::new(Expr::Cond(CondExpr {
                                span,
                                test: binary_expression.right,
                                cons,
                                alt,
                            })),
                        })
                    }
                    Expr::Unary(unary_expression) if matches!(unary_expression.op, UnaryOp::Bang) => {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span,
                            test: unary_expression.arg,
                            cons: alt,
                            alt: cons,
                        })
                    }
                    Expr::Lit(Lit::Bool(boolean_literal)) => {
                        // cont = true;
                        match boolean_literal.value {
                            true => *cons,
                            false => *alt,
                        }
                    }
                    test => match (*cons, *alt) {
                        (cons, alt) => Expr::Cond(CondExpr {
                            span,
                            test: Box::new(test),
                            cons: Box::new(cons),
                            alt: Box::new(alt),
                        }),
                    },
                },
                Expr::Bin(binary_expression) if binary_expression.left.is_cond() => {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *binary_expression.left else {
                        unreachable!()
                    };
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: conditional_expression.test,
                        cons: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            left: conditional_expression.cons,
                            right: binary_expression.right.clone(),
                        })),
                        alt: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            left: conditional_expression.alt,
                            right: binary_expression.right,
                        })),
                    })
                }
                Expr::Bin(binary_expression) if binary_expression.right.is_cond() => {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *binary_expression.right else {
                        unreachable!()
                    };
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: conditional_expression.test,
                        cons: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            right: conditional_expression.cons,
                            left: binary_expression.left.clone(),
                        })),
                        alt: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            right: conditional_expression.alt,
                            left: binary_expression.left,
                        })),
                    })
                }
                Expr::Assign(assign_expression)
                    if assign_expression.right.is_cond()
                        && match assign_expression.left {
                            AssignTarget::Simple(SimpleAssignTarget::Ident(_)) => true,
                            _ => false,
                        } =>
                {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *assign_expression.right else {
                        unreachable!()
                    };
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: conditional_expression.test,
                        cons: Box::new(Expr::Assign(AssignExpr {
                            span: assign_expression.span,
                            op: assign_expression.op,
                            left: assign_expression.left.clone(),
                            right: conditional_expression.cons,
                        })),
                        alt: Box::new(Expr::Assign(AssignExpr {
                            span: assign_expression.span,
                            op: assign_expression.op,
                            left: assign_expression.left,
                            right: conditional_expression.alt,
                        })),
                    })
                }
                a => a,
            }
        }
    }
}
#[derive(Default)]
pub struct ArrowCallPack {
    idents: BTreeSet<Id>,
}
impl VisitMut for ArrowCallPack {
    fn visit_mut_module(&mut self, node: &mut swc_ecma_ast::Module) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.body.insert(
                0,
                swc_ecma_ast::ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                })))),
            );
        }
    }
    fn visit_mut_script(&mut self, node: &mut swc_ecma_ast::Script) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.body.insert(
                0,
                Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                }))),
            );
        }
    }
    fn visit_mut_block_stmt(&mut self, node: &mut swc_ecma_ast::BlockStmt) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.stmts.insert(
                0,
                Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                }))),
            );
        }
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);
        *node = match take(node) {
            Expr::Call(CallExpr {
                span,
                ctxt,
                callee,
                args,
                type_args,
            }) if args.len() == 0
                && callee
                    .as_expr()
                    .and_then(|a| a.as_arrow())
                    .is_some_and(|a| {
                        a.body.is_expr()
                            && a.params
                                .iter()
                                .all(|a| a.as_assign().is_some_and(|x| x.left.is_ident()))
                    }) =>
            {
                let Callee::Expr(e) = callee else {
                    unreachable!()
                };
                let Expr::Arrow(a) = *e else { unreachable!() };
                let BlockStmtOrExpr::Expr(e) = *a.body else {
                    unreachable!()
                };
                let mut x = a
                    .params
                    .into_iter()
                    .map(|a| {
                        let Pat::Assign(a) = a else { unreachable!() };
                        let Pat::Ident(i) = *a.left else {
                            unreachable!()
                        };
                        self.idents.insert(i.id.to_id());
                        Expr::Assign(AssignExpr {
                            span: a.span,
                            op: AssignOp::Assign,
                            left: AssignTarget::Simple(SimpleAssignTarget::Ident(i)),
                            right: a.right,
                        })
                    })
                    .map(Box::new)
                    .chain([e]);
                Expr::Seq(SeqExpr {
                    span,
                    exprs: x.collect(),
                })
            }
            node => node,
        }
    }
}
#[derive(Default)]
#[non_exhaustive]
pub struct ConstCollector {
    pub map: BTreeMap<Id, Box<Expr>>,
}
impl VisitMut for ConstCollector {
    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        node.visit_mut_children_with(self);
        let VarDeclKind::Const = node.kind else {
            return;
        };
        for d in node.decls.iter() {
            if let Pat::Ident(i) = &d.name {
                if let Some(e) = d.init.as_ref() {
                    self.map.insert(i.to_id(), e.clone());
                }
            }
        }
    }
}
pub struct Inliner {
    inner: ConstCollector,
}
impl VisitMut for Inliner {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        fn go(a: &Expr) -> bool {
            match a {
                Expr::Ident(_) | Expr::Lit(_) => true,
                Expr::Member(m) => {
                    go(&m.obj)
                        && match &m.prop {
                            MemberProp::Ident(_) => true,
                            MemberProp::Computed(c) => go(&c.expr),
                            MemberProp::PrivateName(_) => false,
                        }
                }
                _ => false,
            }
        }
        loop {
            if let Expr::Ident(i) = node {
                if let Some(c) = self.inner.map.get(&i.to_id()) {
                    if go(&**c) {
                        *node = (&**c).clone();
                        continue;
                    }
                }
            }
            break;
        }
        node.visit_mut_children_with(self);
    }
}
pub struct Cleanse {}
impl VisitMut for Cleanse {
    fn visit_mut_stmts(&mut self, node: &mut Vec<Stmt>) {
        node.visit_mut_children_with(self);
        if node.iter().any(|a| {
            a.as_decl().and_then(|d| d.as_var()).is_some_and(|v| {
                v.decls.iter().all(|d| {
                    d.init
                        .as_ref()
                        .and_then(|x| x.as_call())
                        .and_then(|x| match &*x.args {
                            [a, b]
                                if a.expr
                                    .as_lit()
                                    .and_then(|l| l.as_num())
                                    .is_some_and(|n| n.value == 0.0)
                                    && b.expr
                                        .as_lit()
                                        .and_then(|l| l.as_num())
                                        .is_some_and(|n| n.value == 11.0) =>
                            {
                                match &x.callee {
                                    Callee::Expr(e) => Some(&*e),
                                    _ => None,
                                }
                            }
                            _ => None,
                        })
                        .and_then(|x| x.as_member())
                        .and_then(|x| match &x.prop {
                            MemberProp::Ident(i) if i.sym == "substr" => Some(&x.obj),
                            _ => None,
                        })
                        .and_then(|a| a.as_call())
                        .is_some_and(|a| {
                            a.args.len() == 1
                                && match &a.callee {
                                    Callee::Expr(e) => e.is_ident(),
                                    _ => false,
                                }
                        })
                })
            })
        }) {
            take(node);
        }
    }
}
