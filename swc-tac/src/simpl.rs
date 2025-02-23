use arena_traits::{Arena as TArena, IndexAlloc};
use id_arena::{Arena, Id};
use portal_jsc_simpl_js::{self as simpl_ast, Dialect, SimplPath, SimplPathId};
use swc_ecma_ast::{BinaryOp, Id as Ident, Lit};

use crate::lam::LAM;

pub trait TacDialect: Dialect {}

#[derive(Default, Clone)]
pub struct TSimplCfg<D: TacDialect> {
    pub regs: LAM<()>,
    pub blocks: Arena<TSimplBlock<D>>,
}
#[derive(Clone, Default)]
pub struct TSimplBlock<D: TacDialect> {
    pub stmts: Vec<(SimplPathId, SimplItem<D>)>,
    pub term: TSimplTerm<D>,
}
#[derive(Clone)]
#[non_exhaustive]
pub enum SimplItem<D: TacDialect, P = SimplPathId> {
    Just { id: P },
    Bin { left: P, right: P, op: BinaryOp },
    Lit { lit: Lit },
    CallStatic { r#fn: P, args: Vec<P> },
    CallTag { tag: D::Tag, args: Vec<P> },
}
#[derive(Clone, Default)]
pub enum TSimplTerm<D: TacDialect> {
    Return(SimplPathId),
    // Throw(Ident),
    Jmp(Id<TSimplBlock<D>>),
    CondJmp {
        cond: SimplPathId,
        if_true: Id<TSimplBlock<D>>,
        if_false: Id<TSimplBlock<D>>,
    },
    #[default]
    Default,
}
