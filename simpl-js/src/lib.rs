use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use portal_jsc_swc_batch::ImportMapping;
use portal_jsc_swc_util::{Extract, ImportMapper, ImportOr, MakeSpanned};
use swc_atoms::Atom;
use swc_common::{Span, Spanned};
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignOp, BinExpr, BinaryOp, BindingIdent, BlockStmt, CallExpr, Expr,
    ExprOrSpread, ExprStmt, Ident, IdentName, IfStmt, LabeledStmt, Lit, MemberExpr, MemberProp,
    ReturnStmt, SimpleAssignTarget, Stmt, WhileStmt,
};
use typenum::Same;

pub trait Dialect {
    type Mark<T>: Extract<T>;
    type MarkSpanned<T: Spanned + Clone + Debug + Hash + Eq>: Same<Output = Self::Mark<T>>
        + Extract<T>
        + Spanned
        + Clone
        + Debug
        + Hash
        + Eq;
}

#[non_exhaustive]
#[derive(Clone, Hash, Debug, PartialEq, Eq, Spanned)]
pub enum SimplExpr<D: Dialect> {
    Lit(Lit),
    Ident(D::MarkSpanned<SimplPath>),
    Assign(MakeSpanned<SimplAssignment<D>>),
    Bin(MakeSpanned<SimplBinOp<D>>),
    Call(MakeSpanned<Box<SimplCallExpr<D>>>),
}
#[non_exhaustive]
#[derive(Clone, Hash, Debug, PartialEq, Eq, Spanned)]
pub enum SimplStmt<D: Dialect> {
    Expr(MakeSpanned<Box<SimplExpr<D>>>),
    Block(MakeSpanned<Vec<SimplStmt<D>>>),
    If(MakeSpanned<SimplIf<D>>),
    Return(MakeSpanned<Box<SimplExpr<D>>>),
}
#[derive(Clone, Hash, Debug, PartialEq, Eq, Spanned)]
pub struct SimplPath {
    #[span]
    pub root: Ident,
    pub keys: Vec<Atom>,
}
#[derive(Clone, Hash, Debug, PartialEq, Eq, Spanned)]
pub enum SimplCallExpr<D: Dialect> {
    Path {
        #[span]
        path: D::MarkSpanned<SimplPath>,
        args: Vec<SimplExpr<D>>,
    },
    Block(Box<SimplStmt<D>>),
}
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct SimplAssignment<D: Dialect> {
    pub target: D::MarkSpanned<SimplPath>,
    pub assign: AssignOp,
    pub body: Box<SimplExpr<D>>,
}
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct SimplBinOp<D: Dialect> {
    pub lhs: Box<SimplExpr<D>>,
    pub op: BinaryOp,
    pub rhs: Box<SimplExpr<D>>,
}
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct SimplIf<D: Dialect> {
    pub kind: SimplIfKind<D>,
    pub cond: Box<SimplExpr<D>>,
    pub body: Vec<SimplStmt<D>>,
}

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub enum SimplIfKind<D: Dialect> {
    If { r#else: Vec<SimplStmt<D>> },
    While { label: Ident },
}

impl<D: Dialect> SimplStmt<D> {
    pub fn apply_label(&mut self, label: &Ident) {
        match self {
            SimplStmt::Expr(make_spanned) | SimplStmt::Return(make_spanned) => {}
            SimplStmt::Block(make_spanned) => {
                for a in make_spanned.value.iter_mut() {
                    a.apply_label(label);
                }
            }
            SimplStmt::If(make_spanned) => match &mut make_spanned.value.kind {
                SimplIfKind::If { r#else } => {
                    for a in make_spanned.value.body.iter_mut().chain(r#else.iter_mut()) {
                        a.apply_label(label);
                    }
                }
                SimplIfKind::While { label: l } => {
                    *l = label.clone();
                }
            },
        }
    }
}

impl<D: Dialect> From<SimplExpr<D>> for Expr {
    fn from(value: SimplExpr<D>) -> Self {
        match value {
            SimplExpr::Ident(i) => match i.extract_own() {
                p => p.keys.into_iter().fold(Expr::Ident(p.root), |a, b| {
                    let asp = a.span();
                    Expr::Member(MemberExpr {
                        span: a.span(),
                        obj: Box::new(a),
                        prop: swc_ecma_ast::MemberProp::Ident(IdentName { span: asp, sym: b }),
                    })
                }),
            },
            SimplExpr::Assign(a) => match a.value.target.extract_own() {
                mut p => {
                    let h = match p.keys.pop() {
                        None => SimpleAssignTarget::Ident(BindingIdent {
                            id: p.root,
                            type_ann: None,
                        }),
                        Some(pa) => SimpleAssignTarget::Member(MemberExpr {
                            span: a.span,
                            obj: Box::new(p.keys.into_iter().fold(Expr::Ident(p.root), |a, b| {
                                let asp = a.span();
                                Expr::Member(MemberExpr {
                                    span: a.span(),
                                    obj: Box::new(a),
                                    prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                        span: asp,
                                        sym: b,
                                    }),
                                })
                            })),
                            prop: MemberProp::Ident(IdentName {
                                span: a.span,
                                sym: pa,
                            }),
                        }),
                    };
                    Expr::Assign(AssignExpr {
                        span: a.span,
                        op: a.value.assign,
                        left: swc_ecma_ast::AssignTarget::Simple(h),
                        right: Box::new((*a.value.body).into()),
                    })
                }
            },
            SimplExpr::Bin(b) => Expr::Bin(BinExpr {
                span: b.span,
                op: b.value.op,
                left: Box::new((*b.value.lhs).into()),
                right: Box::new((*b.value.rhs).into()),
            }),
            SimplExpr::Lit(l) => Expr::Lit(l),
            SimplExpr::Call(c) => match *c.value {
                SimplCallExpr::Path { path, args } => {
                    let pid = match path.extract_own() {
                        p => p.keys.into_iter().fold(Expr::Ident(p.root), |a, b| {
                            let asp = a.span();
                            Expr::Member(MemberExpr {
                                span: a.span(),
                                obj: Box::new(a),
                                prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                    span: asp,
                                    sym: b,
                                }),
                            })
                        }),
                    };
                    Expr::Call(CallExpr {
                        span: c.span,
                        ctxt: Default::default(),
                        callee: swc_ecma_ast::Callee::Expr(Box::new(pid)),
                        args: args
                            .into_iter()
                            .map(|a| a.into())
                            .map(|a| ExprOrSpread {
                                spread: None,
                                expr: Box::new(a),
                            })
                            .collect(),
                        type_args: None,
                    })
                }
                SimplCallExpr::Block(simpl_stmt) => {
                    let pid = Expr::Arrow(ArrowExpr {
                        span: c.span,
                        ctxt: Default::default(),
                        params: vec![],
                        body: Box::new(swc_ecma_ast::BlockStmtOrExpr::BlockStmt(BlockStmt {
                            span: c.span,
                            ctxt: Default::default(),
                            stmts: vec![(*simpl_stmt).into()],
                        })),
                        is_async: false,
                        is_generator: false,
                        type_params: None,
                        return_type: None,
                    });
                    Expr::Call(CallExpr {
                        span: c.span,
                        ctxt: Default::default(),
                        callee: swc_ecma_ast::Callee::Expr(Box::new(pid)),
                        args: vec![],
                        type_args: None,
                    })
                }
            },
            _ => todo!(),
        }
    }
}
impl<D: Dialect> SimplStmt<D> {
    pub fn flat(self) -> Vec<SimplStmt<D>> {
        match self {
            SimplStmt::Block(b) => b.value.into_iter().flat_map(|a| a.flat()).collect(),
            SimplStmt::If(i) => vec![SimplStmt::If(MakeSpanned {
                value: SimplIf {
                    kind: match i.value.kind {
                        SimplIfKind::If { r#else } => SimplIfKind::If {
                            r#else: r#else.into_iter().flat_map(|a| a.flat()).collect(),
                        },
                        a => a,
                    },
                    cond: i.value.cond,
                    body: i.value.body.into_iter().flat_map(|a| a.flat()).collect(),
                },
                span: i.span,
            })],
            a => vec![a],
        }
    }
}
impl<D: Dialect> From<SimplStmt<D>> for Stmt {
    fn from(value: SimplStmt<D>) -> Self {
        match value {
            SimplStmt::Expr(e) => Stmt::Expr(ExprStmt {
                expr: Box::new((*e.value).into()),
                span: e.span,
            }),
            SimplStmt::Block(b) => Stmt::Block(BlockStmt {
                span: b.span,
                ctxt: Default::default(),
                stmts: b.value.into_iter().map(|a| a.into()).collect(),
            }),
            SimplStmt::Return(e) => Stmt::Return(ReturnStmt {
                span: e.span,
                arg: Some(Box::new((*e.value).into())),
            }),
            SimplStmt::If(i) => match i.value.kind {
                SimplIfKind::If { r#else } => Stmt::If(IfStmt {
                    span: i.span,
                    test: Box::new((*i.value.cond).into()),
                    cons: Box::new(Stmt::Block(BlockStmt {
                        span: i.span,
                        ctxt: Default::default(),
                        stmts: i.value.body.into_iter().map(|a| a.into()).collect(),
                    })),
                    alt: if r#else.len() != 0 {
                        Some(Box::new(Stmt::Block(BlockStmt {
                            span: i.span,
                            ctxt: Default::default(),
                            stmts: r#else.into_iter().map(|a| a.into()).collect(),
                        })))
                    } else {
                        None
                    },
                }),
                SimplIfKind::While { label } => Stmt::Labeled(LabeledStmt {
                    span: i.span,
                    label,
                    body: Box::new(Stmt::While(WhileStmt {
                        span: i.span,
                        test: Box::new((*i.value.cond).into()),
                        body: Box::new(Stmt::Block(BlockStmt {
                            span: i.span,
                            ctxt: Default::default(),
                            stmts: i.value.body.into_iter().map(|a| a.into()).collect(),
                        })),
                    })),
                }),
            },
            _ => todo!(),
        }
    }
}
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum Error {
    Unsupported,
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Unsupported => write!(f, "unsupported syntax construct"),
        }
    }
}
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            _ => None,
        }
    }
}
pub trait ConvCtx: ImportMapper {}
impl<T: ImportMapper + ?Sized> ConvCtx for T {}
pub trait Conv {
    type Target<D: Dialect>;
    fn conv<D: ConvDialect>(&self, imports: &impl ConvCtx) -> Result<Self::Target<D>, Error>;
}
pub trait ConvDialect: Dialect {
    fn new_import<T: Eq + Hash + Clone + Spanned + Debug>(a: ImportOr<T>) -> Self::MarkSpanned<T>;
    fn get_import<T: Eq + Hash + Clone + Spanned + Debug>(a: Self::MarkSpanned<T>) -> ImportOr<T>;
}
impl Conv for Expr {
    type Target<D: Dialect> = SimplExpr<D>;

    fn conv<D: ConvDialect>(&self, imports: &impl ConvCtx) -> Result<Self::Target<D>, Error> {
        Ok(match self {
            Expr::Lit(l) => SimplExpr::Lit(l.clone()),
            Expr::Ident(i) => SimplExpr::Ident(match i.clone() {
                i => D::new_import(match imports.import_of(&i.to_id()) {
                    None => ImportOr::NotImport(SimplPath {
                        root: i,
                        keys: vec![],
                    }),
                    Some((a, b)) => ImportOr::Import {
                        value: SimplPath {
                            root: i,
                            keys: vec![],
                        },
                        module: a,
                        name: b,
                    },
                }),
            }),
            Expr::Member(m) => {
                let a: SimplExpr<D> = m.obj.conv(imports)?;
                let mut path: ImportOr<SimplPath> = D::get_import(match a {
                    SimplExpr::Ident(path) => path,
                    SimplExpr::Assign(a) => a.value.target,
                    _ => return Err(Error::Unsupported),
                });
                let MemberProp::Ident(i) = &m.prop else {
                    return Err(Error::Unsupported);
                };
                match &mut path {
                    ImportOr::NotImport(value) => {
                        value.keys.push(i.sym.clone());
                    }
                    ImportOr::Import {
                        value,
                        module,
                        name,
                    } => {
                        value.keys.push(i.sym.clone());
                    }
                };
                SimplExpr::Ident(D::new_import(path))
            }
            Expr::Assign(a) => {
                let e: SimplExpr<D> = match &a.left {
                    swc_ecma_ast::AssignTarget::Simple(simple_assign_target) => {
                        match simple_assign_target {
                            swc_ecma_ast::SimpleAssignTarget::Ident(binding_ident) => {
                                Expr::Ident(binding_ident.id.clone()).conv(imports)?
                            }
                            swc_ecma_ast::SimpleAssignTarget::Member(m) => {
                                let a: SimplExpr<D> = m.obj.conv(imports)?;
                                let mut path: ImportOr<SimplPath> = D::get_import(match a {
                                    SimplExpr::Ident(path) => path,
                                    SimplExpr::Assign(a) => a.value.target,
                                    _ => return Err(Error::Unsupported),
                                });
                                let MemberProp::Ident(i) = &m.prop else {
                                    return Err(Error::Unsupported);
                                };
                                match &mut path {
                                    ImportOr::NotImport(value) => {
                                        value.keys.push(i.sym.clone());
                                    }
                                    ImportOr::Import {
                                        value,
                                        module,
                                        name,
                                    } => {
                                        value.keys.push(i.sym.clone());
                                    }
                                };
                                SimplExpr::Ident(D::new_import(path))
                            }
                            swc_ecma_ast::SimpleAssignTarget::SuperProp(super_prop_expr) => todo!(),
                            swc_ecma_ast::SimpleAssignTarget::Paren(paren_expr) => todo!(),
                            swc_ecma_ast::SimpleAssignTarget::OptChain(opt_chain_expr) => todo!(),
                            swc_ecma_ast::SimpleAssignTarget::TsAs(ts_as_expr) => todo!(),
                            swc_ecma_ast::SimpleAssignTarget::TsSatisfies(ts_satisfies_expr) => {
                                todo!()
                            }
                            swc_ecma_ast::SimpleAssignTarget::TsNonNull(ts_non_null_expr) => {
                                todo!()
                            }
                            swc_ecma_ast::SimpleAssignTarget::TsTypeAssertion(
                                ts_type_assertion,
                            ) => todo!(),
                            swc_ecma_ast::SimpleAssignTarget::TsInstantiation(ts_instantiation) => {
                                todo!()
                            }
                            swc_ecma_ast::SimpleAssignTarget::Invalid(invalid) => todo!(),
                        }
                    }
                    swc_ecma_ast::AssignTarget::Pat(assign_target_pat) => todo!(),
                };
                let mut path = match e {
                    SimplExpr::Ident(path) => path,
                    SimplExpr::Assign(a) => a.value.target,
                    _ => return Err(Error::Unsupported),
                };
                SimplExpr::Assign(MakeSpanned {
                    value: SimplAssignment {
                        target: path,
                        assign: a.op,
                        body: Box::new(a.right.conv(imports)?),
                    },
                    span: a.span,
                })
            }
            Expr::Bin(b) => SimplExpr::Bin(MakeSpanned {
                value: SimplBinOp {
                    lhs: Box::new(b.left.conv(imports)?),
                    op: b.op,
                    rhs: Box::new(b.right.conv(imports)?),
                },
                span: b.span,
            }),
            Expr::Call(c) => match &c.callee {
                swc_ecma_ast::Callee::Super(_) => todo!(),
                swc_ecma_ast::Callee::Import(import) => todo!(),
                swc_ecma_ast::Callee::Expr(expr) => match &**expr {
                    Expr::Fn(f) if f.function.params.len() == 0 => SimplExpr::Call(MakeSpanned {
                        value: Box::new(SimplCallExpr::Block(Box::new(SimplStmt::Block(
                            MakeSpanned {
                                value: f
                                    .function
                                    .body
                                    .iter()
                                    .flat_map(|a| a.stmts.iter())
                                    .map(|s| s.conv(imports))
                                    .collect::<Result<Vec<_>, _>>()?,
                                span: f.span(),
                            },
                        )))),
                        span: f.span(),
                    }),
                    Expr::Arrow(f) if f.params.len() == 0 => SimplExpr::Call(MakeSpanned {
                        value: Box::new(SimplCallExpr::Block(Box::new(SimplStmt::Block(
                            MakeSpanned {
                                value: match &*f.body {
                                    swc_ecma_ast::BlockStmtOrExpr::BlockStmt(block_stmt) => {
                                        block_stmt
                                            .stmts
                                            .iter()
                                            .map(|s| s.conv(imports))
                                            .collect::<Result<Vec<_>, _>>()?
                                    }
                                    swc_ecma_ast::BlockStmtOrExpr::Expr(expr) => {
                                        vec![SimplStmt::Return(MakeSpanned {
                                            value: Box::new(expr.conv(imports)?),
                                            span: f.span(),
                                        })]
                                    }
                                },
                                span: f.span(),
                            },
                        )))),
                        span: f.span(),
                    }),
                    e => {
                        let a: SimplExpr<D> = e.conv(imports)?;
                        let mut path = match a {
                            SimplExpr::Ident(path) => path,
                            SimplExpr::Assign(a) => a.value.target,
                            _ => return Err(Error::Unsupported),
                        };
                        SimplExpr::Call(MakeSpanned {
                            value: Box::new(SimplCallExpr::Path {
                                path,
                                args: c
                                    .args
                                    .iter()
                                    .map(|a| a.expr.conv(imports))
                                    .collect::<Result<Vec<_>, _>>()?,
                            }),
                            span: c.span,
                        })
                    }
                },
            },
            _ => return Err(Error::Unsupported),
        })
    }
}
impl Conv for Stmt {
    type Target<D: Dialect> = SimplStmt<D>;

    fn conv<D: ConvDialect>(&self, imports: &impl ConvCtx) -> Result<Self::Target<D>, Error> {
        Ok(match self {
            Stmt::Expr(e) => SimplStmt::Expr(MakeSpanned {
                value: Box::new(e.expr.conv(imports)?),
                span: e.span,
            }),
            Stmt::Block(b) => SimplStmt::Block(MakeSpanned {
                value: b
                    .stmts
                    .iter()
                    .map(|a| a.conv(imports))
                    .collect::<Result<Vec<_>, _>>()?,
                span: b.span,
            }),
            Stmt::If(i) => SimplStmt::If(MakeSpanned {
                value: SimplIf {
                    kind: SimplIfKind::If {
                        r#else: i
                            .alt
                            .iter()
                            .map(|a| a.conv(imports))
                            .collect::<Result<Vec<_>, _>>()?,
                    },
                    cond: Box::new(i.test.conv(imports)?),
                    body: vec![i.cons.conv(imports)?],
                },
                span: i.span,
            }),
            Stmt::While(w) => SimplStmt::If(MakeSpanned {
                value: SimplIf {
                    kind: SimplIfKind::While {
                        label: Ident::new_private(Atom::new("$"), w.span),
                    },
                    cond: Box::new(w.test.conv(imports)?),
                    body: vec![w.body.conv(imports)?],
                },
                span: w.span,
            }),
            Stmt::Labeled(l) => {
                let mut w = l.body.conv(imports)?;
                w.apply_label(&l.label);
                w
            }
            Stmt::Return(r) => SimplStmt::Return(MakeSpanned {
                value: Box::new(match r.arg.as_ref() {
                    None => return Err(Error::Unsupported),
                    Some(a) => a.conv(imports)?,
                }),
                span: r.span,
            }),
            _ => return Err(Error::Unsupported),
        })
    }
}
