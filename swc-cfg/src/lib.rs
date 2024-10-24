use std::collections::HashMap;

use anyhow::Context;
use id_arena::{Arena, Id};
use swc_atoms::Atom;
use swc_common::{Spanned, SyntaxContext};
use swc_ecma_ast::{
    BindingIdent, BlockStmt, Bool, Decl, DoWhileStmt, Expr, ExprStmt, Ident, IfStmt, Lit, Pat,
    Stmt, WhileStmt,
};

pub struct Cfg {
    pub blocks: Arena<Block>,
}
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub catch: Catch,
    pub term: Term,
}
#[derive(Clone)]
pub enum Catch {
    Throw,
    Jump { pat: Pat, k: Id<Block> },
}
#[derive(Default)]
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
#[derive(Clone)]
pub struct Loop {
    pub r#break: Id<Block>,
    pub r#continue: Id<Block>,
}
#[derive(Clone)]
pub struct Ctx {
    pub catch: Catch,
    pub cur_loop: Option<Loop>,
    pub labelled: HashMap<Ident, Loop>,
}
impl Ctx {
    pub fn new_block(&self, cfg: &mut Cfg) -> Id<Block> {
        return cfg.blocks.alloc(Block {
            stmts: vec![],
            catch: self.catch.clone(),
            term: Term::Default,
        });
    }
    pub fn transform_all(
        &self,
        cfg: &mut Cfg,
        x: Vec<Stmt>,
        mut current: Id<Block>,
    ) -> anyhow::Result<Id<Block>> {
        for x in x {
            current = self.transform(cfg, x, current, None)?;
        }
        Ok(current)
    }
    pub fn transform(
        &self,
        cfg: &mut Cfg,
        x: Stmt,
        current: Id<Block>,
        label: Option<Ident>,
    ) -> anyhow::Result<Id<Block>> {
        if let Stmt::Throw(t) = x {
            cfg.blocks[current].term = Term::Throw(*t.arg);
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Return(r) = x {
            cfg.blocks[current].term = Term::Return(r.arg.map(|a| *a));
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Try(t) = x {
            let next = self.new_block(cfg);
            let catch = match t.handler {
                None => None,
                Some(a) => Some({
                    let x = self.new_block(cfg);
                    let y = self.transform_all(cfg, a.body.stmts, x)?;
                    cfg.blocks[y].term = Term::Jmp(next);
                    (
                        a.param.unwrap_or(Pat::Ident(BindingIdent {
                            id: Ident::new(Atom::new("_error"), a.span, SyntaxContext::default()),
                            type_ann: None,
                        })),
                        x,
                    )
                }),
            };
            let mut new = self.clone();
            if let Some((a, b)) = catch {
                new.catch = Catch::Jump { pat: a, k: b };
            };
            let a = new.transform_all(cfg, t.block.stmts, current)?;
            cfg.blocks[a].term = Term::Jmp(next);
            let next = match t.finalizer {
                Some(f) => self.transform_all(cfg, f.stmts, next)?,
                None => next,
            };
            return Ok(next);
        }
        if let Stmt::Block(b) = x {
            return self.transform_all(cfg, b.stmts, current);
        }
        if let Stmt::If(i) = x {
            let next = self.new_block(cfg);
            let then = self.new_block(cfg);
            let then_end = self.transform(
                cfg,
                *i.cons,
                current,
                match i.alt.as_ref() {
                    None => label,
                    Some(_) => None,
                },
            )?;
            cfg.blocks[then_end].term = Term::Jmp(next);
            let els = match i.alt {
                None => then,
                Some(e) => {
                    let els = self.new_block(cfg);
                    let els_end = self.transform(cfg, *e, current, None)?;
                    cfg.blocks[els_end].term = Term::Jmp(next);
                    els
                }
            };
            cfg.blocks[current].term = Term::CondJmp {
                cond: *i.test,
                if_true: then,
                if_false: els,
            };
            return Ok(next);
        }
        if let Stmt::Switch(i) = x {
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
            for c in i.cases {
                match c.test {
                    None => {
                        default = cur;
                        cur = target.transform_all(cfg, c.cons, cur)?;
                    }
                    Some(t) => {
                        blocks.insert(*t, cur);
                        cur = target.transform_all(cfg, c.cons, cur)?;
                    }
                }
            }
            cfg.blocks[cur].term = Term::Jmp(next);
            cfg.blocks[current].term = Term::Switch {
                x: *i.discriminant,
                blocks: blocks,
                default: default,
            };
            return Ok(next);
        }
        if let Stmt::Break(b) = x {
            cfg.blocks[current].term = Term::Jmp(
                match b.label {
                    Some(l) => self.labelled.get(&l),
                    None => self.cur_loop.as_ref(),
                }
                .context("in getting the current loop")?
                .r#break,
            );
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Continue(b) = x {
            cfg.blocks[current].term = Term::Jmp(
                match b.label {
                    Some(l) => self.labelled.get(&l),
                    None => self.cur_loop.as_ref(),
                }
                .context("in getting the current loop")?
                .r#continue,
            );
            return Ok(self.new_block(cfg));
        }
        if let Stmt::Labeled(l) = x {
            let next = self.new_block(cfg);
            let cont = self.new_block(cfg);
            cfg.blocks[current].term = Term::Jmp(cont);
            let mut new = self.clone();
            new.labelled.insert(
                l.label.clone(),
                Loop {
                    r#break: next,
                    r#continue: cont,
                },
            );
            let k = new.transform(cfg, *l.body, cont, Some(l.label))?;
            cfg.blocks[k].term = Term::Jmp(next);
            return Ok(next);
        }
        if let Stmt::DoWhile(w) = x {
            let next = self.new_block(cfg);
            let cont = self.new_block(cfg);
            cfg.blocks[current].term = Term::Jmp(cont);
            let mut new = self.clone();
            new.cur_loop = Some(Loop {
                r#break: next,
                r#continue: cont,
            });
            if let Some(l) = label {
                new.labelled
                    .insert(l, new.cur_loop.as_ref().cloned().unwrap());
            }
            let k = new.transform(cfg, *w.body, cont, None)?;
            cfg.blocks[k].term = Term::CondJmp {
                cond: *w.test,
                if_true: cont,
                if_false: next,
            };
            return Ok(next);
        }
        if let Stmt::While(w) = x {
            return self.transform(
                cfg,
                Stmt::If(IfStmt {
                    span: w.span,
                    test: w.test.clone(),
                    alt: None,
                    cons: Box::new(Stmt::DoWhile(DoWhileStmt {
                        span: w.span,
                        test: w.test,
                        body: w.body,
                    })),
                }),
                current,
                label,
            );
        }
        if let Stmt::For(f) = x {
            if let Some(i) = f.init {
                cfg.blocks[current].stmts.push(match i {
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
                    span: f.span,
                    test: f.test.unwrap_or_else(|| {
                        Box::new(Expr::Lit(Lit::Bool(Bool {
                            span: f.span,
                            value: true,
                        })))
                    }),
                    body: Box::new(Stmt::Block(BlockStmt {
                        span: f.span,
                        ctxt: SyntaxContext::default(),
                        stmts: vec![f.body]
                            .into_iter()
                            .chain(
                                f.update
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
        cfg.blocks[current].stmts.push(x);
        Ok(current)
    }
}
