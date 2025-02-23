use std::collections::BTreeMap;

use arena_traits::{Arena as TArena, IndexAlloc};
use id_arena::{Arena, Id};
use portal_jsc_simpl_js::{
    self as simpl_ast, Dialect, SimplExpr, SimplPath, SimplPathId, SimplStmt,
};
use swc_cfg::Loop;
use swc_common::{Span, Spanned};
use swc_ecma_ast::{BinaryOp, Id as Ident, Lit};

use crate::{lam::LAM, ValFlags};

pub trait TacDialect: Dialect {}

pub struct TSimplCfg<D: TacDialect> {
    pub regs: LAM<()>,
    pub blocks: Arena<TSimplBlock<D>>,
}
impl<D: TacDialect> Default for TSimplCfg<D> {
    fn default() -> Self {
        Self {
            regs: Default::default(),
            blocks: Default::default(),
        }
    }
}
impl<D: TacDialect> Clone for TSimplCfg<D> {
    fn clone(&self) -> Self {
        Self {
            regs: self.regs.clone(),
            blocks: self.blocks.clone(),
        }
    }
}

pub struct TSimplBlock<D: TacDialect> {
    pub stmts: Vec<(SimplPathId, ValFlags, SimplItem<D>, Span)>,
    pub term: TSimplTerm<D>,
    pub orig_span: Option<Span>,
}
impl<D: TacDialect> Default for TSimplBlock<D> {
    fn default() -> Self {
        Self {
            stmts: Default::default(),
            term: Default::default(),
            orig_span: Default::default(),
        }
    }
}
impl<D: TacDialect> Clone for TSimplBlock<D> {
    fn clone(&self) -> Self {
        Self {
            stmts: self.stmts.clone(),
            term: self.term.clone(),
            orig_span: self.orig_span.clone(),
        }
    }
}

#[non_exhaustive]
pub enum SimplItem<D: TacDialect, P = SimplPathId> {
    Just { id: P },
    Bin { left: P, right: P, op: BinaryOp },
    Lit { lit: Lit },
    CallStatic { r#fn: P, args: Vec<P> },
    CallTag { tag: D::Tag, args: Vec<P> },
}
impl<D: TacDialect, P: Clone> Clone for SimplItem<D, P> {
    fn clone(&self) -> Self {
        match self {
            Self::Just { id } => Self::Just { id: id.clone() },
            Self::Bin { left, right, op } => Self::Bin {
                left: left.clone(),
                right: right.clone(),
                op: op.clone(),
            },
            Self::Lit { lit } => Self::Lit { lit: lit.clone() },
            Self::CallStatic { r#fn, args } => Self::CallStatic {
                r#fn: r#fn.clone(),
                args: args.clone(),
            },
            Self::CallTag { tag, args } => Self::CallTag {
                tag: tag.clone(),
                args: args.clone(),
            },
        }
    }
}

pub enum TSimplTerm<D: TacDialect> {
    Return(SimplPathId),
    // Throw(Ident),
    Jmp(Id<TSimplBlock<D>>),
    CondJmp {
        cond: SimplPathId,
        if_true: Id<TSimplBlock<D>>,
        if_false: Id<TSimplBlock<D>>,
    },
    Switch {
        scrutinee: SimplPathId,
        cases: BTreeMap<Ident, (Id<TSimplBlock<D>>, Vec<SimplPathId>)>,
    },
    Default,
}
impl<D: TacDialect> Clone for TSimplTerm<D> {
    fn clone(&self) -> Self {
        match self {
            Self::Return(arg0) => Self::Return(arg0.clone()),
            Self::Jmp(arg0) => Self::Jmp(arg0.clone()),
            Self::CondJmp {
                cond,
                if_true,
                if_false,
            } => Self::CondJmp {
                cond: cond.clone(),
                if_true: if_true.clone(),
                if_false: if_false.clone(),
            },
            Self::Switch { scrutinee, cases } => Self::Switch {
                scrutinee: scrutinee.clone(),
                cases: cases.clone(),
            },
            Self::Default => Self::Default,
        }
    }
}
impl<D: TacDialect> Default for TSimplTerm<D> {
    fn default() -> Self {
        Self::Default
    }
}
pub trait Bake<D: TacDialect> {
    type Res;
    fn bake(
        &self,
        labels: &(dyn Fn(Ident) -> Loop<Id<TSimplBlock<D>>> + '_),
        ret: Option<&(Id<TSimplBlock<D>>, SimplPathId)>,
        cfg: &mut TSimplCfg<D>,
        start_block: Id<TSimplBlock<D>>,
    ) -> (Self::Res, Id<TSimplBlock<D>>);
}
impl<D: TacDialect> Bake<D> for SimplExpr<D> {
    type Res = SimplPathId;

    fn bake(
        &self,
        labels: &(dyn Fn(Ident) -> Loop<Id<TSimplBlock<D>>> + '_),
        ret: Option<&(Id<TSimplBlock<D>>, SimplPathId)>,
        cfg: &mut TSimplCfg<D>,
        start_block: Id<TSimplBlock<D>>,
    ) -> (Self::Res, Id<TSimplBlock<D>>) {
        match self {
            SimplExpr::Lit(lit) => {
                let i = SimplPathId {
                    root: cfg.regs.alloc(()),
                    keys: vec![],
                };
                cfg.blocks[start_block].stmts.push((
                    i.clone(),
                    ValFlags::SSA_LIKE,
                    SimplItem::Lit { lit: lit.clone() },
                    lit.span(),
                ));
                (i, start_block)
            }
            SimplExpr::Ident(i) => (i.as_ref().to_id(), start_block),
            SimplExpr::Assign(make_spanned) => {
                let (v, start_block) = make_spanned.value.body.bake(labels, ret, cfg, start_block);
                let o = make_spanned.value.target.as_ref().to_id();
                cfg.blocks[start_block].stmts.push((
                    o.clone(),
                    Default::default(),
                    match make_spanned.value.assign.to_update() {
                        None => SimplItem::Just { id: v },
                        Some(b) => SimplItem::Bin {
                            left: o.clone(),
                            right: v,
                            op: b,
                        },
                    },
                    make_spanned.span,
                ));
                (o, start_block)
            }
            SimplExpr::Bin(make_spanned) => {
                let (left, start_block) =
                    make_spanned.value.lhs.bake(labels, ret, cfg, start_block);
                let (right, start_block) =
                    make_spanned.value.rhs.bake(labels, ret, cfg, start_block);
                let i = SimplPathId {
                    root: cfg.regs.alloc(()),
                    keys: vec![],
                };
                cfg.blocks[start_block].stmts.push((
                    i.clone(),
                    ValFlags::SSA_LIKE,
                    SimplItem::Bin {
                        left: left,
                        right: right,
                        op: make_spanned.value.op,
                    },
                    make_spanned.span,
                ));
                (i, start_block)
            }
            SimplExpr::Call(make_spanned) => match &*make_spanned.value {
                portal_jsc_simpl_js::SimplCallExpr::Path { path, args } => {
                    let (args, start_block) = args.bake(labels, ret, cfg, start_block);
                    let i = SimplPathId {
                        root: cfg.regs.alloc(()),
                        keys: vec![],
                    };
                    cfg.blocks[start_block].stmts.push((
                        i.clone(),
                        ValFlags::SSA_LIKE,
                        SimplItem::CallStatic {
                            r#fn: path.as_ref().to_id(),
                            args: args,
                        },
                        make_spanned.span,
                    ));
                    (i, start_block)
                }
                portal_jsc_simpl_js::SimplCallExpr::Tag { tag, args } => {
                    let (args, start_block) = args.bake(labels, ret, cfg, start_block);
                    let i = SimplPathId {
                        root: cfg.regs.alloc(()),
                        keys: vec![],
                    };
                    cfg.blocks[start_block].stmts.push((
                        i.clone(),
                        ValFlags::SSA_LIKE,
                        SimplItem::CallTag {
                            tag: tag.clone(),
                            args: args,
                        },
                        make_spanned.span,
                    ));
                    (i, start_block)
                }
                portal_jsc_simpl_js::SimplCallExpr::Block(simpl_stmt) => {
                    let i = SimplPathId {
                        root: cfg.regs.alloc(()),
                        keys: vec![],
                    };
                    let then = cfg.blocks.alloc(Default::default());
                    let (_, start_block) =
                        simpl_stmt.bake(labels, Some(&(then, i.clone())), cfg, start_block);
                    cfg.blocks[start_block].term = TSimplTerm::Jmp(then);
                    cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                    (i, then)
                }
            },
            SimplExpr::Switch(make_spanned) => {
                let (v, start_block) =
                    make_spanned
                        .value
                        .scrutinee
                        .bake(labels, ret, cfg, start_block);
                let i = SimplPathId {
                    root: cfg.regs.alloc(()),
                    keys: vec![],
                };
                let then = cfg.blocks.alloc(Default::default());
                let xs = make_spanned
                    .value
                    .cases
                    .iter()
                    .map(|(a, (s, i2))| {
                        let k = cfg.blocks.alloc(Default::default());
                        let (_, start_block) = s.bake(labels, Some(&(then, i.clone())), cfg, k);
                        cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                        cfg.blocks[start_block].term = TSimplTerm::Jmp(then);
                        (
                            a.clone(),
                            (
                                k,
                                i2.iter()
                                    .map(|a| SimplPathId {
                                        root: a.to_id(),
                                        keys: vec![],
                                    })
                                    .collect(),
                            ),
                        )
                    })
                    .collect();
                cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                cfg.blocks[start_block].term = TSimplTerm::Switch {
                    scrutinee: v,
                    cases: xs,
                };
                (i, then)
                // (i, then)
            }
            _ => todo!(),
        }
    }
}
impl<D: TacDialect, B: Bake<D>> Bake<D> for Vec<B> {
    type Res = Vec<B::Res>;

    fn bake(
        &self,
        labels: &(dyn Fn(Ident) -> Loop<Id<TSimplBlock<D>>> + '_),
        ret: Option<&(Id<TSimplBlock<D>>, SimplPathId)>,
        cfg: &mut TSimplCfg<D>,
        mut start_block: Id<TSimplBlock<D>>,
    ) -> (Self::Res, Id<TSimplBlock<D>>) {
        let mut res = vec![];
        for a in self.iter() {
            let v;
            (v, start_block) = a.bake(labels, ret, cfg, start_block);
            res.push(v);
        }
        (res, start_block)
    }
}
impl<D: TacDialect> Bake<D> for SimplStmt<D> {
    type Res = ();

    fn bake(
        &self,
        labels: &(dyn Fn(Ident) -> Loop<Id<TSimplBlock<D>>> + '_),
        ret: Option<&(Id<TSimplBlock<D>>, SimplPathId)>,
        cfg: &mut TSimplCfg<D>,
        start_block: Id<TSimplBlock<D>>,
    ) -> (Self::Res, Id<TSimplBlock<D>>) {
        (
            (),
            match self {
                SimplStmt::Expr(make_spanned) => {
                    let (_, start_block) = make_spanned.value.bake(labels, ret, cfg, start_block);
                    start_block
                }
                SimplStmt::Block(make_spanned) => make_spanned.value.iter().fold(
                    start_block,
                    |start_block: Id<TSimplBlock<D>>, x| {
                        let (_, start_block) = x.bake(labels, ret, cfg, start_block);
                        return start_block;
                    },
                ),
                SimplStmt::If(make_spanned) => match &make_spanned.value.kind {
                    portal_jsc_simpl_js::SimplIfKind::If { r#else } => {
                        let after = cfg.blocks.alloc(Default::default());
                        let then = cfg.blocks.alloc(Default::default());
                        let els = cfg.blocks.alloc(Default::default());
                        let (v, start_block) =
                            make_spanned.value.cond.bake(labels, ret, cfg, start_block);
                        cfg.blocks[start_block].term = TSimplTerm::CondJmp {
                            cond: v,
                            if_true: then,
                            if_false: els,
                        };
                        cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                        let (_, then) = make_spanned.value.body.bake(labels, ret, cfg, then);
                        let (_, els) = r#else.bake(labels, ret, cfg, els);
                        cfg.blocks[then].term = TSimplTerm::Jmp(after);
                        cfg.blocks[els].term = TSimplTerm::Jmp(after);
                        after
                    }
                    portal_jsc_simpl_js::SimplIfKind::While { label } => {
                        let cont = cfg.blocks.alloc(Default::default());
                        let brk = cfg.blocks.alloc(Default::default());
                        let bb = cfg.blocks.alloc(Default::default());
                        cfg.blocks[start_block].term = TSimplTerm::Jmp(cont);
                        cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                        let (v, ct) = make_spanned.value.cond.bake(labels, ret, cfg, cont);
                        cfg.blocks[ct].term = TSimplTerm::CondJmp {
                            cond: v,
                            if_true: bb,
                            if_false: brk,
                        };
                        let (_, bb) = make_spanned.value.body.bake(
                            &|l| {
                                if l == label.to_id() {
                                    Loop {
                                        r#break: brk,
                                        r#continue: cont,
                                    }
                                } else {
                                    labels(l)
                                }
                            },
                            ret,
                            cfg,
                            bb,
                        );
                        cfg.blocks[bb].term = TSimplTerm::Jmp(cont);
                        brk
                    }
                },
                SimplStmt::Return(make_spanned) => {
                    let (v2, start_block) = make_spanned.value.bake(labels, ret, cfg, start_block);
                    match ret.map(|a| a.clone()) {
                        None => cfg.blocks[start_block].term = TSimplTerm::Return(v2),
                        Some((k, v)) => {
                            cfg.blocks[start_block].stmts.push((
                                v,
                                Default::default(),
                                SimplItem::Just { id: v2 },
                                make_spanned.span,
                            ));
                            cfg.blocks[start_block].term = TSimplTerm::Jmp(k);
                        }
                    };
                    cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                    cfg.blocks.alloc(Default::default())
                }
                _ => todo!(),
            },
        )
    }
}
