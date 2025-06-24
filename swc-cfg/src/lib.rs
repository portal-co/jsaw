use std::{
    collections::{BTreeSet, HashMap},
    default,
    iter::once,
};

use anyhow::Context;
use id_arena::{Arena, Id};
use relooper::ShapedBlock;
use swc_atoms::Atom;
use swc_common::{Span, Spanned, SyntaxContext};
use swc_ecma_ast::{
    ArrayLit, AssignExpr, BindingIdent, BlockStmt, Bool, BreakStmt, CallExpr, CatchClause,
    ContinueStmt, Decl, DoWhileStmt, Expr, ExprOrSpread, ExprStmt, Function, Ident, IdentName,
    IfStmt, LabeledStmt, Lit, MemberExpr, Param, Pat, ReturnStmt, Stmt, Str, SwitchCase,
    SwitchStmt, ThrowStmt, TryStmt, TsTypeAnn, TsTypeParamDecl, WhileStmt,
};
pub mod recfg;
pub mod simplify;
#[derive(Clone)]
pub struct Func {
    pub cfg: Cfg,
    pub entry: Id<Block>,
    pub params: Vec<Param>,
    pub is_generator: bool,
    pub is_async: bool,
}
impl TryFrom<Function> for Func {
    type Error = anyhow::Error;

    fn try_from(value: Function) -> Result<Self, Self::Error> {
        let mut cfg = Cfg::default();
        let entry = cfg.blocks.alloc(Default::default());
        let exit = ToCfgConversionCtx::default().transform_all(
            &mut cfg,
            value.body.map(|a| a.stmts).unwrap_or_else(Vec::new),
            entry,
        )?;
        cfg.blocks[exit].end.term = Term::Return(None);
        cfg.simplify();
        cfg.generics = value.type_params.map(|a| *a);
        cfg.ts_retty = value.return_type.map(|a| *a);
        return Ok(Self {
            cfg,
            entry,
            params: value.params,
            is_generator: value.is_generator,
            is_async: value.is_async,
        });
    }
}
impl Into<Function> for Func {
    fn into(self) -> Function {
        let k = ssa_reloop::go(&self, self.entry);
        let stmts = Cfg::process_block(&self.cfg, &k, Span::dummy_with_cmt(), Default::default());
        return Function {
            params: self.params,
            decorators: vec![],
            span: Span::dummy_with_cmt(),
            ctxt: Default::default(),
            body: Some(BlockStmt {
                span: Span::dummy_with_cmt(),
                ctxt: Default::default(),
                stmts,
            }),
            is_generator: self.is_generator,
            is_async: self.is_async,
            type_params: self.cfg.generics.map(Box::new),
            return_type: self.cfg.ts_retty.map(Box::new),
        };
    }
}

#[derive(Clone, Default)]
pub struct Cfg {
    pub blocks: Arena<Block>,
    pub generics: Option<TsTypeParamDecl>,
    pub ts_retty: Option<TsTypeAnn>,
}
impl Cfg {
    pub fn recfg(&self, entry: Id<Block>) -> (Cfg, Id<Block>) {
        let mut res = Cfg::default();
        res.generics = self.generics.clone();
        res.ts_retty = self.ts_retty.clone();
        let Ok(entry) = recfg::Recfg::default().go(self, &mut res, entry) else {
            return (self.clone(), entry);
        };
        return (res, entry);
    }
    // pub fn reloop_block(&self, entry: Id<Block>) -> ShapedBlock<Id<Block>> {
    //     return *relooper::reloop(
    //         self.blocks
    //             .iter()
    //             .map(|(a, k)| {
    //                 (
    //                     a,
    //                     match &k.end.catch {
    //                         Catch::Throw => None,
    //                         Catch::Jump { pat, k } => Some(*k),
    //                     }
    //                     .into_iter()
    //                     .chain(
    //                         match &k.end.term {
    //                             Term::Return(expr) => vec![],
    //                             Term::Throw(expr) => vec![],
    //                             Term::Jmp(id) => vec![*id],
    //                             Term::CondJmp {
    //                                 cond,
    //                                 if_true,
    //                                 if_false,
    //                             } => vec![*if_true, *if_false],
    //                             Term::Switch { x, blocks, default } => {
    //                                 blocks.values().cloned().chain(once(*default)).collect()
    //                             }
    //                             Term::Default => vec![],
    //                         }
    //                         .into_iter(),
    //                     )
    //                     .collect::<BTreeSet<_>>()
    //                     .into_iter()
    //                     .collect(),
    //                 )
    //             })
    //             .collect(),
    //         entry,
    //     );
    // }
    pub fn process_block(
        &self,
        k: &ShapedBlock<Id<Block>>,
        span: Span,
        ctxt: SyntaxContext,
    ) -> Vec<Stmt> {
        match k {
            ShapedBlock::Simple(simple_block) => {
                let span = match self.blocks[simple_block.label].end.orig_span.clone() {
                    None => span,
                    Some(s) => s,
                };
                let jmp = |k: Id<Block>| {
                    vec![Stmt::Expr(ExprStmt {
                        span,
                        expr: Box::new(Expr::Assign(AssignExpr {
                            span,
                            op: swc_ecma_ast::AssignOp::Assign,
                            left: Ident::new(Atom::new("cff"), span, ctxt).into(),
                            right: Box::new(Expr::Lit(Lit::Str(Str {
                                span,
                                value: Atom::new(k.index().to_string()),
                                raw: None,
                            }))),
                        })),
                    })]
                    .into_iter()
                    .chain(
                        match simple_block.branches.get(&k) {
                            None => vec![],
                            Some(a) => match a {
                                relooper::BranchMode::LoopBreak(l)
                                | relooper::BranchMode::LoopBreakIntoMulti(l) => {
                                    vec![Stmt::Break(BreakStmt {
                                        span,
                                        label: Some(Ident::new(
                                            Atom::new(format!("${l}")),
                                            span,
                                            ctxt,
                                        )),
                                    })]
                                }
                                relooper::BranchMode::LoopContinue(l)
                                | relooper::BranchMode::LoopContinueIntoMulti(l) => {
                                    vec![Stmt::Continue(ContinueStmt {
                                        span,
                                        label: Some(Ident::new(
                                            Atom::new(format!("${l}")),
                                            span,
                                            ctxt,
                                        )),
                                    })]
                                }
                                relooper::BranchMode::MergedBranch => vec![],
                                relooper::BranchMode::MergedBranchIntoMulti => vec![],
                                relooper::BranchMode::SetLabelAndBreak => {
                                    vec![Stmt::Break(BreakStmt { span, label: None })]
                                }
                            },
                        }
                        .into_iter(),
                    )
                    .collect::<Vec<_>>()
                };
                let l = simple_block.label;
                let body = self.blocks[l].stmts.iter().cloned().collect::<Vec<_>>();
                let mut body = match &self.blocks[l].end.catch {
                    Catch::Throw => body,
                    Catch::Jump { pat, k } => {
                        let id = Ident::new_private(Atom::new("caught"), span);
                        vec![Stmt::Try(Box::new(TryStmt {
                            span,
                            block: BlockStmt {
                                span,
                                ctxt,
                                stmts: body,
                            },
                            finalizer: None,
                            handler: Some(CatchClause {
                                span,
                                param: Some(Pat::Ident(id.clone().into())),
                                body: BlockStmt {
                                    span,
                                    ctxt,
                                    stmts: vec![Stmt::Expr(ExprStmt {
                                        span,
                                        expr: Box::new(Expr::Assign(AssignExpr {
                                            span,
                                            op: swc_ecma_ast::AssignOp::Assign,
                                            left: pat.clone().try_into().unwrap(),
                                            right: Box::new(Expr::Ident(id)),
                                        })),
                                    })]
                                    .into_iter()
                                    .chain(jmp(*k).into_iter())
                                    .collect(),
                                },
                            }),
                        }))]
                    }
                };
                match &self.blocks[l].end.term {
                    Term::Return(expr) => body.push(Stmt::Return(ReturnStmt {
                        span,
                        arg: expr.as_ref().cloned().map(Box::new),
                    })),
                    Term::Throw(expr) => body.push(Stmt::Throw(ThrowStmt {
                        span,
                        arg: Box::new(expr.clone()),
                    })),
                    Term::Jmp(id) => body.extend(jmp(*id).into_iter()),
                    Term::CondJmp {
                        cond,
                        if_true,
                        if_false,
                    } => body.push(Stmt::If(IfStmt {
                        span,
                        test: Box::new(cond.clone()),
                        cons: Box::new(Stmt::Block(BlockStmt {
                            span,
                            ctxt,
                            stmts: jmp(*if_true),
                        })),
                        alt: Some(Box::new(Stmt::Block(BlockStmt {
                            span,
                            ctxt,
                            stmts: jmp(*if_false),
                        }))),
                    })),
                    Term::Switch { x, blocks, default } => {
                        body.push(Stmt::Switch(SwitchStmt {
                            span,
                            discriminant: Box::new(x.clone()),
                            cases: blocks
                                .iter()
                                .map(|(i, k)| SwitchCase {
                                    span,
                                    test: Some(Box::new(i.clone())),
                                    cons: jmp(*k),
                                })
                                .chain(once(SwitchCase {
                                    span,
                                    test: None,
                                    cons: jmp(*default),
                                }))
                                .collect(),
                        }));
                    }
                    Term::Default => {}
                };
                for x in [simple_block.immediate.as_ref(), simple_block.next.as_ref()] {
                    body.extend(
                        x.into_iter()
                            .flat_map(|i| self.process_block(i.as_ref(), span, ctxt)),
                    );
                }
                return body;
            }
            ShapedBlock::Loop(loop_block) => once(Stmt::Labeled(LabeledStmt {
                span,
                label: Ident::new(Atom::new(format!("${}", loop_block.loop_id)), span, ctxt),
                body: Box::new(Stmt::While(WhileStmt {
                    span: span,
                    test: Box::new(Expr::Lit(Lit::Bool(Bool {
                        span: span,
                        value: true,
                    }))),
                    body: Box::new(Stmt::Block(BlockStmt {
                        span,
                        ctxt,
                        stmts: self.process_block(&*loop_block.inner, span, ctxt),
                    })),
                })),
            }))
            .chain(
                loop_block
                    .next
                    .as_ref()
                    .into_iter()
                    .flat_map(|a| self.process_block(&*a, span, ctxt).into_iter()),
            )
            .collect(),
            ShapedBlock::Multiple(multiple_block) => vec![Stmt::Switch(SwitchStmt {
                span,
                discriminant: Box::new(Expr::Lit(Lit::Bool(Bool { span, value: true }))),
                cases: multiple_block
                    .handled
                    .iter()
                    .map(|h| SwitchCase {
                        span,
                        test: Some(Box::new(Expr::Call(CallExpr {
                            span,
                            ctxt,
                            callee: swc_ecma_ast::Callee::Expr(Box::new(
                                MemberExpr {
                                    span,
                                    obj: Box::new(Expr::Array(ArrayLit {
                                        span,
                                        elems: h
                                            .labels
                                            .iter()
                                            .map(|l| {
                                                Some(ExprOrSpread {
                                                    spread: None,
                                                    expr: Box::new(Expr::Lit(Lit::Str(Str {
                                                        span,
                                                        value: Atom::new(l.index().to_string()),
                                                        raw: None,
                                                    }))),
                                                })
                                            })
                                            .collect(),
                                    })),
                                    prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                        span,
                                        sym: Atom::new("contains"),
                                    }),
                                }
                                .into(),
                            )),
                            args: vec![ExprOrSpread {
                                spread: None,
                                expr: Box::new(Expr::Ident(Ident::new(
                                    Atom::new("cff"),
                                    span,
                                    ctxt,
                                ))),
                            }],
                            type_args: None,
                        }))),
                        cons: self
                            .process_block(&h.inner, span, ctxt)
                            .into_iter()
                            .chain(
                                if h.break_after {
                                    Some(Stmt::Break(swc_ecma_ast::BreakStmt { span, label: None }))
                                } else {
                                    None
                                }
                                .into_iter(),
                            )
                            .collect(),
                    })
                    .collect(),
            })],
        }
    }
}

impl cfg_traits::Func for Func {
    type Block = Id<Block>;

    type Blocks = Arena<Block>;

    fn blocks(&self) -> &Self::Blocks {
        &self.cfg.blocks
    }

    fn blocks_mut(&mut self) -> &mut Self::Blocks {
        &mut self.cfg.blocks
    }

    fn entry(&self) -> Self::Block {
        self.entry
    }
}

impl cfg_traits::Block<Func> for Block {
    type Terminator = End;

    fn term(&self) -> &Self::Terminator {
        &self.end
    }

    fn term_mut(&mut self) -> &mut Self::Terminator {
        &mut self.end
    }
}

impl cfg_traits::Term<Func> for End {
    type Target = Id<Block>;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        Func: 'a,
    {
        Box::new(
            match &self.catch {
                Catch::Throw => None,
                Catch::Jump { pat, k } => Some(k),
            }
            .into_iter()
            .chain(
                match &self.term {
                    Term::Return(expr) => vec![],
                    Term::Throw(expr) => vec![],
                    Term::Jmp(id) => vec![id],
                    Term::CondJmp {
                        cond,
                        if_true,
                        if_false,
                    } => vec![if_true, if_false],
                    Term::Switch { x, blocks, default } => {
                        blocks.values().chain(once(default)).collect()
                    }
                    Term::Default => vec![],
                }
                .into_iter(),
            ),
        )
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        Func: 'a,
    {
        Box::new(
            match &mut self.catch {
                Catch::Throw => None,
                Catch::Jump { pat, k } => Some(k),
            }
            .into_iter()
            .chain(
                match &mut self.term {
                    Term::Return(expr) => vec![],
                    Term::Throw(expr) => vec![],
                    Term::Jmp(id) => vec![id],
                    Term::CondJmp {
                        cond,
                        if_true,
                        if_false,
                    } => vec![if_true, if_false],
                    Term::Switch { x, blocks, default } => {
                        blocks.values_mut().chain(once(default)).collect()
                    }
                    Term::Default => vec![],
                }
                .into_iter(),
            ),
        )
    }
}

impl cfg_traits::Term<Func> for Id<Block> {
    type Target = Id<Block>;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        Func: 'a,
    {
        Box::new(once(self))
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        Func: 'a,
    {
        Box::new(once(self))
    }
}

impl cfg_traits::Target<Func> for Id<Block> {
    fn block(&self) -> <Func as cfg_traits::Func>::Block {
        *self
    }

    fn block_mut(&mut self) -> &mut <Func as cfg_traits::Func>::Block {
        self
    }
}

#[derive(Default, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub end: End,
}
#[derive(Default, Clone)]
pub struct End {
    pub catch: Catch,
    pub term: Term,
    pub orig_span: Option<Span>,
}
#[derive(Clone, Default)]
pub enum Catch {
    #[default]
    Throw,
    Jump {
        pat: Pat,
        k: Id<Block>,
    },
}
#[derive(Default, Clone)]
pub enum Term {
    Return(Option<Expr>),
    Throw(Expr),
    Jmp(Id<Block>),
    CondJmp {
        cond: Expr,
        if_true: Id<Block>,
        if_false: Id<Block>,
    },
    Switch {
        x: Expr,
        blocks: HashMap<Expr, Id<Block>>,
        default: Id<Block>,
    },
    #[default]
    Default,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Loop<T = Id<Block>> {
    pub r#break: T,
    pub r#continue: T,
}
#[derive(Clone, Default)]
pub struct ToCfgConversionCtx {
    pub catch: Catch,
    pub cur_loop: Option<Loop>,
    pub labelled: HashMap<Ident, Loop>,
}
impl ToCfgConversionCtx {
    pub fn new_block(&self, cfg: &mut Cfg) -> Id<Block> {
        return cfg.blocks.alloc(Block {
            stmts: vec![],
            end: End {
                catch: self.catch.clone(),
                term: Term::Default,
                orig_span: None,
            },
        });
    }
    pub fn transform_all(
        &self,
        cfg: &mut Cfg,
        statements: Vec<Stmt>,
        mut current: Id<Block>,
    ) -> anyhow::Result<Id<Block>> {
        for statement in statements {
            current = self.transform(cfg, statement, current, None)?;
        }
        Ok(current)
    }
    pub fn transform(
        &self,
        cfg: &mut Cfg,
        statement: Stmt,
        current: Id<Block>,
        label: Option<Ident>,
    ) -> anyhow::Result<Id<Block>> {
        if let Stmt::Throw(throw_stmt) = statement {
            cfg.blocks[current].end.orig_span = Some(throw_stmt.span());
            cfg.blocks[current].end.term = Term::Throw(*throw_stmt.arg);
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Return(return_stmt) = statement {
            cfg.blocks[current].end.orig_span = Some(return_stmt.span());
            cfg.blocks[current].end.term = Term::Return(return_stmt.arg.map(|a| *a));
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Try(try_stmt) = statement {
            let span = try_stmt.span();
            let next = self.new_block(cfg);
            let catch = match try_stmt.handler {
                None => None,
                Some(catch_clause) => Some({
                    let catch_block_id = self.new_block(cfg);
                    let catch_end_id = self.transform_all(cfg, catch_clause.body.stmts, catch_block_id)?;
                    cfg.blocks[catch_end_id].end.term = Term::Jmp(next);
                    (
                        catch_clause.param.unwrap_or(Pat::Ident(BindingIdent {
                            id: Ident::new(Atom::new("_error"), catch_clause.span, SyntaxContext::default()),
                            type_ann: None,
                        })),
                        catch_block_id,
                    )
                }),
            };
            let mut new = self.clone();
            if let Some((catch_param, catch_block_id)) = catch {
                new.catch = Catch::Jump { pat: catch_param, k: catch_block_id };
            };
            let try_end_id = new.transform_all(cfg, try_stmt.block.stmts, current)?;
            cfg.blocks[try_end_id].end.term = Term::Jmp(next);
            cfg.blocks[try_end_id].end.orig_span = Some(span);
            let next = match try_stmt.finalizer {
                Some(finalizer) => self.transform_all(cfg, finalizer.stmts, next)?,
                None => next,
            };
            return Ok(next);
        }
        if let Stmt::Block(block) = statement {
            return self.transform_all(cfg, block.stmts, current);
        }
        if let Stmt::If(if_stmt) = statement {
            let span = if_stmt.span();
            let next = self.new_block(cfg);
            let then = self.new_block(cfg);
            let then_end = self.transform(
                cfg,
                *if_stmt.cons,
                current,
                match if_stmt.alt.as_ref() {
                    None => label,
                    Some(_) => None,
                },
            )?;
            cfg.blocks[then_end].end.term = Term::Jmp(next);
            let els = match if_stmt.alt {
                None => then,
                Some(else_stmt) => {
                    let els = self.new_block(cfg);
                    let els_end = self.transform(cfg, *else_stmt, current, None)?;
                    cfg.blocks[els_end].end.term = Term::Jmp(next);
                    els
                }
            };
            cfg.blocks[current].end.term = Term::CondJmp {
                cond: *if_stmt.test,
                if_true: then,
                if_false: els,
            };
            cfg.blocks[current].end.orig_span = Some(span);
            return Ok(next);
        }
        if let Stmt::Switch(switch_stmt) = statement {
            let span = switch_stmt.span();
            let next = self.new_block(cfg);
            let mut target = self.clone();
            if let None = target.cur_loop {
                target.cur_loop = Some(Loop {
                    r#break: next,
                    r#continue: next,
                })
            };
            target.cur_loop.as_mut().unwrap().r#break = next;
            let mut cur = self.new_block(cfg);
            let mut default = next;
            let mut blocks = HashMap::new();
            for case in switch_stmt.cases {
                match case.test {
                    None => {
                        default = cur;
                        cur = target.transform_all(cfg, case.cons, cur)?;
                    }
                    Some(test) => {
                        blocks.insert(*test, cur);
                        cur = target.transform_all(cfg, case.cons, cur)?;
                    }
                }
            }
            cfg.blocks[cur].end.term = Term::Jmp(next);
            cfg.blocks[current].end.term = Term::Switch {
                x: *switch_stmt.discriminant,
                blocks: blocks,
                default: default,
            };
            cfg.blocks[current].end.orig_span = Some(span);
            return Ok(next);
        }
        if let Stmt::Break(break_stmt) = statement {
            cfg.blocks[current].end.orig_span = Some(break_stmt.span());
            cfg.blocks[current].end.term = Term::Jmp(
                match break_stmt.label {
                    Some(l) => self.labelled.get(&l),
                    None => self.cur_loop.as_ref(),
                }
                .context("in getting the current loop")?
                .r#break,
            );
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Continue(continue_stmt) = statement {
            cfg.blocks[current].end.orig_span = Some(continue_stmt.span());
            cfg.blocks[current].end.term = Term::Jmp(
                match continue_stmt.label {
                    Some(l) => self.labelled.get(&l),
                    None => self.cur_loop.as_ref(),
                }
                .context("in getting the current loop")?
                .r#continue,
            );
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Labeled(labeled_stmt) = statement {
            let next = self.new_block(cfg);
            let cont = self.new_block(cfg);
            cfg.blocks[current].end.orig_span = Some(labeled_stmt.span());
            cfg.blocks[current].end.term = Term::Jmp(cont);
            let mut new = self.clone();
            new.labelled.insert(
                labeled_stmt.label.clone(),
                Loop {
                    r#break: next,
                    r#continue: cont,
                },
            );
            let k = new.transform(cfg, *labeled_stmt.body, cont, Some(labeled_stmt.label))?;
            cfg.blocks[k].end.term = Term::Jmp(next);
            return Ok(next);
        }
        if let Stmt::DoWhile(do_while_stmt) = statement {
            let next = self.new_block(cfg);
            let cont = self.new_block(cfg);
            cfg.blocks[current].end.orig_span = Some(do_while_stmt.span());
            cfg.blocks[current].end.term = Term::Jmp(cont);
            let mut new = self.clone();
            new.cur_loop = Some(Loop {
                r#break: next,
                r#continue: cont,
            });
            if let Some(l) = label {
                new.labelled
                    .insert(l, new.cur_loop.as_ref().cloned().unwrap());
            }
            let k = new.transform(cfg, *do_while_stmt.body, cont, None)?;
            cfg.blocks[k].end.term = Term::CondJmp {
                cond: *do_while_stmt.test,
                if_true: cont,
                if_false: next,
            };
            return Ok(next);
        }
        if let Stmt::While(while_stmt) = statement {
            return self.transform(
                cfg,
                Stmt::If(IfStmt {
                    span: while_stmt.span,
                    test: while_stmt.test.clone(),
                    alt: None,
                    cons: Box::new(Stmt::DoWhile(DoWhileStmt {
                        span: while_stmt.span,
                        test: while_stmt.test,
                        body: while_stmt.body,
                    })),
                }),
                current,
                label,
            );
        }
        if let Stmt::For(for_stmt) = statement {
            if let Some(init) = for_stmt.init {
                cfg.blocks[current].stmts.push(match init {
                    swc_ecma_ast::VarDeclOrExpr::VarDecl(var_decl) => {
                        Stmt::Decl(Decl::Var(var_decl))
                    }
                    swc_ecma_ast::VarDeclOrExpr::Expr(expr) => Stmt::Expr(ExprStmt {
                        span: expr.span(),
                        expr,
                    }),
                });
            }
            return self.transform(
                cfg,
                Stmt::While(WhileStmt {
                    span: for_stmt.span,
                    test: for_stmt.test.unwrap_or_else(|| {
                        Box::new(Expr::Lit(Lit::Bool(Bool {
                            span: for_stmt.span,
                            value: true,
                        })))
                    }),
                    body: Box::new(Stmt::Block(BlockStmt {
                        span: for_stmt.span,
                        ctxt: SyntaxContext::default(),
                        stmts: vec![for_stmt.body]
                            .into_iter()
                            .chain(
                                for_stmt.update
                                    .map(|a| {
                                        Box::new(Stmt::Expr(ExprStmt {
                                            span: a.span(),
                                            expr: a,
                                        }))
                                    })
                                    .into_iter(),
                            )
                            .map(|a| *a)
                            .collect(),
                    })),
                }),
                current,
                label,
            );
        }
        cfg.blocks[current].stmts.push(statement);
        Ok(current)
    }
}
