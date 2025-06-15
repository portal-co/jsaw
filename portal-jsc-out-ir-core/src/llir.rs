use id_arena::{Arena, Id};
use swc_common::{Span, Spanned};

use crate::WithSpan;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Size {
    Bits { bits: usize },
    Ptr,
}
#[derive(Clone, Default)]
pub struct LLCfg {
    pub blocks: Arena<LLBlock>,
    pub values: Arena<LLValue>,
}
impl LLCfg {
    pub fn add_blockparam(&mut self, k: Id<LLBlock>, ty: Size) -> Id<LLValue> {
        let v = self.values.alloc(LLValue {
            internal: crate::WithSpan {
                wrap: LLValueInternal::Param {
                    block: k,
                    size: ty,
                    index: self.blocks[k].params.len(),
                },
                item: Span::dummy_with_cmt(),
            },
        });
        self.blocks[k].params.push((v, ty));
        return v;
    }
    pub fn append_to_block(&mut self, k: Id<LLBlock>, a: LLValue) -> Id<LLValue> {
        let val = self.values.alloc(a);
        self.blocks[k].stmts.push(val);
        val
    }
}
#[derive(Clone)]
pub struct LLFunc {
    pub cfg: LLCfg,
    pub root: Id<LLBlock>,
}
impl Default for LLFunc {
    fn default() -> Self {
        let mut cfg = LLCfg::default();
        let root = cfg.blocks.alloc(Default::default());
        Self { cfg, root }
    }
}
#[derive(Clone, Default)]
pub struct LLBlock {
    pub params: Vec<(Id<LLValue>, Size)>,
    pub stmts: Vec<Id<LLValue>>,
    pub term: WithSpan<LLTerm>,
}
#[derive(Clone, Spanned)]
pub struct LLValue {
    #[span]
    pub internal: WithSpan<LLValueInternal>,
}
#[derive(Clone)]
#[non_exhaustive]
pub enum LLValueInternal {
    ArithOp {
        size: Size,
        lhs: Id<LLValue>,
        op: portal_pc_asm_common::types::Arith,
        rhs: Id<LLValue>,
    },
    CmpOp {
        size: Size,
        lhs: Id<LLValue>,
        op: portal_pc_asm_common::types::Cmp,
        rhs: Id<LLValue>,
    },
    Load {
        size: Size,
        val: Id<LLValue>,
    },
    Store {
        size: Size,
        ptr: Id<LLValue>,
        val: Id<LLValue>,
    },
    ExtOrTrunc {
        extension: portal_pc_asm_common::types::Ext,
        val: Id<LLValue>,
        target: Size,
    },
    Param {
        block: Id<LLBlock>,
        size: Size,
        index: usize,
    },
    Const {
        size: Size,
        val: bitvec::vec::BitVec,
    },
}
#[derive(Clone)]
pub struct LLTarget {
    pub block: Id<LLBlock>,
    pub args: Vec<Id<LLValue>>,
}
#[derive(Clone, Default)]
pub enum LLTerm {
    Ret(Id<LLValue>),
    Jmp(LLTarget),
    CondJmp {
        val: Id<LLValue>,
        then: LLTarget,
        otherwise: LLTarget,
    },
    #[default]
    Default,
}
