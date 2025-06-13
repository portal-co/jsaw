use std::collections::{BTreeMap, BTreeSet};

use arena_traits::{Arena as TArena, IndexAlloc};
use id_arena::{Arena, Id};
use portal_jsc_simpl_js::{
    self as simpl_ast, Dialect, FuncId, SimplExpr, SimplPath, SimplPathId, SimplStmt,
};
use swc_cfg::Loop;
use swc_common::{Span, Spanned};
use swc_ecma_ast::{BinaryOp, Expr, Id as Ident, Lit};

use crate::{lam::LAM, ValFlags};

pub mod impls;
pub mod reloop;

pub trait TacDialect: Dialect<Mark<()>: Clone + Default> {}

pub struct TSimplCfg<D: TacDialect> {
    pub regs: LAM<()>,
    pub blocks: Arena<TSimplBlock<D>>,
}
pub struct TSimplFunc<D: TacDialect> {
    pub cfg: TSimplCfg<D>,
    pub entry: Id<TSimplBlock<D>>,
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
impl<D: TacDialect> Clone for TSimplFunc<D> {
    fn clone(&self) -> Self {
        Self {
            cfg: self.cfg.clone(),
            entry: self.entry.clone(),
        }
    }
}
impl<D: TacDialect> Default for TSimplFunc<D> {
    fn default() -> Self {
        let mut cfg = TSimplCfg::default();
        let e = cfg.blocks.alloc(Default::default());
        Self { cfg, entry: e }
    }
}
pub struct TSimplBlock<D: TacDialect> {
    pub stmts: Vec<(SimplPathId, D::Mark<()>, ValFlags, SimplItem<D>, Span)>,
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
    Just {
        id: (P, D::Mark<()>),
    },
    Bin {
        left: (P, D::Mark<()>),
        right: (P, D::Mark<()>),
        op: BinaryOp,
    },
    Lit {
        lit: Lit,
    },
    CallStatic {
        r#fn: FuncId<Expr, (P, D::Mark<()>)> ,
        args: Vec<(P, D::Mark<()>)>,
    },
    CallTag {
        tag: FuncId<Expr, D::Tag>,
        args: Vec<(P, D::Mark<()>)>,
    },
    DiscriminantIn {
        value: (P, D::Mark<()>),
        ids: BTreeMap<Ident, Vec<(P, D::Mark<()>)>>,
    },
}
impl<D: TacDialect, P> SimplItem<D, P> {
    pub fn map<Q, E>(self, mut go: impl FnMut(P) -> Result<Q, E>) -> Result<SimplItem<D, Q>, E> {
        Ok(match self {
            SimplItem::Just { id } => SimplItem::Just {
                id: (go(id.0)?, id.1),
            },
            SimplItem::Bin { left, right, op } => SimplItem::Bin {
                left: (go(left.0)?, left.1),
                right: (go(right.0)?, right.1),
                op: op,
            },
            SimplItem::Lit { lit } => SimplItem::Lit { lit: lit },
            SimplItem::CallStatic { r#fn, args } => SimplItem::CallStatic {
                r#fn: r#fn.map(|r#fn| Ok::<_, E>((go(r#fn.0)?, r#fn.1)), Ok)?,
                args: args
                    .into_iter()
                    .map(|(a, b)| Ok((go(a)?, b)))
                    .collect::<Result<_, E>>()?,
            },
            SimplItem::CallTag { tag, args } => SimplItem::CallTag {
                tag: tag,
                args: args
                    .into_iter()
                    .map(|(a, b)| Ok((go(a)?, b)))
                    .collect::<Result<_, E>>()?,
            },
            SimplItem::DiscriminantIn { value, ids } => SimplItem::DiscriminantIn {
                value: (go(value.0)?, value.1),
                ids: ids
                    .into_iter()
                    .map(|(i, v)| {
                        Ok((
                            i,
                            v.into_iter()
                                .map(|(a, b)| Ok((go(a)?, b)))
                                .collect::<Result<_, E>>()?,
                        ))
                    })
                    .collect::<Result<_, E>>()?,
            },
        })
    }
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
            Self::DiscriminantIn { value, ids } => Self::DiscriminantIn {
                value: value.clone(),
                ids: ids.clone(),
            },
        }
    }
}

pub enum TSimplTerm<D: TacDialect> {
    Return((SimplPathId, D::Mark<()>)),
    // Throw(Ident),
    Jmp(Id<TSimplBlock<D>>),
    CondJmp {
        cond: (SimplPathId, D::Mark<()>),
        if_true: Id<TSimplBlock<D>>,
        if_false: Id<TSimplBlock<D>>,
    },
    Select {
        scrutinee: (SimplPathId, D::Mark<()>),
        cases: BTreeMap<Ident, (Id<TSimplBlock<D>>, Vec<(SimplPathId, D::Mark<()>)>)>,
    },
    Switch {
        scrutinee: (SimplPathId, D::Mark<()>),
        cases: Vec<((SimplPathId, D::Mark<()>), Id<TSimplBlock<D>>)>,
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
            Self::Select { scrutinee, cases } => Self::Select {
                scrutinee: scrutinee.clone(),
                cases: cases.clone(),
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
    type Res = (SimplPathId, D::Mark<()>);

    fn bake(
        &self,
        labels: &(dyn Fn(Ident) -> Loop<Id<TSimplBlock<D>>> + '_),
        ret: Option<&(Id<TSimplBlock<D>>, SimplPathId)>,
        cfg: &mut TSimplCfg<D>,
        start_block: Id<TSimplBlock<D>>,
    ) -> (Self::Res, Id<TSimplBlock<D>>) {
        match self {
            SimplExpr::Lit(literal) => {
                let i = SimplPathId {
                    root: cfg.regs.alloc(()),
                    keys: vec![],
                };
                cfg.blocks[start_block].stmts.push((
                    i.clone(),
                    Default::default(),
                    ValFlags::SSA_LIKE,
                    SimplItem::Lit { lit: literal.clone() },
                    literal.span(),
                ));
                ((i, Default::default()), start_block)
            }
            SimplExpr::Ident(i) => (
                match D::despan(i.clone()) {
                    (a, b) => (b.to_id(), a),
                },
                start_block,
            ),
            SimplExpr::Assign(make_spanned) => {
                let (value, start_block) = make_spanned.value.body.bake(labels, ret, cfg, start_block);
                let o = make_spanned.value.target.clone();
                let (mark, o) = D::despan(o);
                let o = o.to_id();
                cfg.blocks[start_block].stmts.push((
                    o.clone(),
                    mark.clone(),
                    Default::default(),
                    match make_spanned.value.assign.to_update() {
                        None => SimplItem::Just { id: value },
                        Some(b) => SimplItem::Bin {
                            left: (o.clone(), mark.clone()),
                            right: value,
                            op: b,
                        },
                    },
                    make_spanned.span,
                ));
                ((o, mark), start_block)
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
                    Default::default(),
                    ValFlags::SSA_LIKE,
                    SimplItem::Bin {
                        left: left,
                        right: right,
                        op: make_spanned.value.op,
                    },
                    make_spanned.span,
                ));
                ((i, Default::default()), start_block)
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
                        Default::default(),
                        ValFlags::SSA_LIKE,
                        SimplItem::CallStatic {
                            r#fn: FuncId {
                                path: match D::despan(path.path.clone()) {
                                    (a, b) => (b.to_id(), a),
                                },
                                template_args: path.template_args.clone(),
                            },
                            args: args,
                        },
                        make_spanned.span,
                    ));
                    ((i, Default::default()), start_block)
                }
                portal_jsc_simpl_js::SimplCallExpr::Tag { tag, args } => {
                    let (args, start_block) = args.bake(labels, ret, cfg, start_block);
                    let i = SimplPathId {
                        root: cfg.regs.alloc(()),
                        keys: vec![],
                    };
                    cfg.blocks[start_block].stmts.push((
                        i.clone(),
                        Default::default(),
                        ValFlags::SSA_LIKE,
                        SimplItem::CallTag {
                            tag: tag.clone(),
                            args: args,
                        },
                        make_spanned.span,
                    ));
                    ((i, Default::default()), start_block)
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
                    ((i, Default::default()), then)
                }
            },
            SimplExpr::Select(make_spanned) => {
                let (value, start_block) =
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
                                    .map(|a| (a, Default::default()))
                                    .collect(),
                            ),
                        )
                    })
                    .collect();
                cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                cfg.blocks[start_block].term = TSimplTerm::Select {
                    scrutinee: value,
                    cases: xs,
                };
                ((i, Default::default()), then)
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
                SimplStmt::Switch(switch_stmt) => {
                    let after = cfg.blocks.alloc(Default::default());
                    let (scrutinee, mut start_block) =
                        switch_stmt.value.scrutinee.bake(labels, ret, cfg, start_block);
                    let mut cases = vec![];
                    let start =
                        switch_stmt
                            .value
                            .cases
                            .iter()
                            .enumerate()
                            .rev()
                            .fold(after, |previous_block, (i, case)| {
                                let value;
                                (value, start_block) = case.0.bake(labels, ret, cfg, start_block);
                                let new_block = cfg.blocks.alloc(Default::default());
                                let (_, next_flow) = case.1.bake(labels, ret, cfg, new_block);
                                cfg.blocks[next_flow].term = TSimplTerm::Jmp(previous_block);
                                cases.push((value, new_block));
                                if i == switch_stmt.value.cases.len() - 1 {
                                    after
                                } else {
                                    match &switch_stmt.value.cases[i + 1].2 {
                                        portal_jsc_swc_util::BreakKind::BreakAfter => after,
                                        portal_jsc_swc_util::BreakKind::DoNotBreakAfter => new_block,
                                    }
                                }
                            });
                    cfg.blocks[start_block].orig_span = Some(switch_stmt.span);
                    cfg.blocks[start_block].term = TSimplTerm::Switch { scrutinee, cases };
                    cfg.blocks.alloc(Default::default())
                }
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
                    let (value, start_block) = make_spanned.value.bake(labels, ret, cfg, start_block);
                    match ret.map(|a| a.clone()) {
                        None => cfg.blocks[start_block].term = TSimplTerm::Return(value),
                        Some((k, v)) => {
                            cfg.blocks[start_block].stmts.push((
                                v,
                                Default::default(),
                                Default::default(),
                                SimplItem::Just { id: value },
                                make_spanned.span,
                            ));
                            cfg.blocks[start_block].term = TSimplTerm::Jmp(k);
                        }
                    };
                    cfg.blocks[start_block].orig_span = Some(make_spanned.span);
                    cfg.blocks.alloc(Default::default())
                }
                SimplStmt::Break(b) => {
                    let b2 = labels(b.to_id()).r#break;
                    cfg.blocks[start_block].orig_span = Some(b.span);
                    cfg.blocks[start_block].term = TSimplTerm::Jmp(b2);
                    cfg.blocks.alloc(Default::default())
                }
                SimplStmt::Continue(b) => {
                    let b2 = labels(b.to_id()).r#continue;
                    cfg.blocks[start_block].orig_span = Some(b.span);
                    cfg.blocks[start_block].term = TSimplTerm::Jmp(b2);
                    cfg.blocks.alloc(Default::default())
                }
                _ => todo!(),
            },
        )
    }
}
