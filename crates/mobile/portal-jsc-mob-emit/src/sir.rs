//! Structured, target-neutral function IR ("SIR").
//!
//! An [`SFunc`] is built from exactly the artifacts the Wasm opcode encoder
//! consumes — `Trees`, the Stackify `WasmBlock` control tree, and the
//! `Localifier` local assignment — so the mobile emitters and the Wasm
//! backend agree on semantics by construction:
//!
//! - structured control flow: `Block`/`Loop` labels, `Break`/`Continue`,
//!   `If`; no raw branches, no operand stack;
//! - every value lives in a numbered local (params first), except
//!   treeified/rematerialized values, which appear as nested [`SExpr`]
//!   subexpressions at their single use site — just like the encoder
//!   places them on the Wasm stack;
//! - tail calls stay explicit as [`SStmt::TailCall`]/[`SStmt::TailCallRef`]
//!   so the backends can implement trampolining.

use portal_pc_waffle::{Func, Operator, Signature, Type};

/// A structured rendering of one function body.
#[derive(Clone, Debug)]
pub struct SFunc {
    /// All locals, params first: `locals[0..n_params]` are the function's
    /// parameters and must not be re-declared by the renderer.
    pub locals: Vec<Type>,
    /// Number of leading entries of [`Self::locals`] that are params.
    pub n_params: usize,
    /// Return types: the audit guarantees `rets.len() <= 1`.
    pub rets: Vec<Type>,
    /// Function body as a sequence of structured statements.
    pub body: Vec<SStmt>,
}

/// A branch-target label. Labels are unique per function.
pub type SLabel = u32;

/// A local index into [`SFunc::locals`].
pub type SLocal = u32;

#[derive(Clone, Debug)]
pub enum SStmt {
    /// `local = expr`
    Assign { local: SLocal, expr: SExpr },
    /// An operator evaluated for its side effects; results (if any) are
    /// dropped, mirroring the encoder's `Drop` of local-less results.
    Effect { expr: SExpr },
    /// A labeled block; `Break { label }` jumps to its end.
    Block { label: SLabel, body: Vec<SStmt> },
    /// A labeled loop; `Continue { label }` jumps to its top.
    Loop { label: SLabel, body: Vec<SStmt> },
    /// Branch to the end of an enclosing [`SStmt::Block`] or
    /// [`SStmt::If`] label.
    Break { label: SLabel },
    /// Branch to the top of an enclosing [`SStmt::Loop`] label.
    Continue { label: SLabel },
    /// Conditional. Branching to an `if`'s label (Wasm semantics) means
    /// its end, so the label behaves like [`SStmt::Block`]'s.
    If {
        label: SLabel,
        cond: SExpr,
        if_true: Vec<SStmt>,
        if_false: Vec<SStmt>,
    },
    /// Plain return. `None` only for functions with no results.
    Return { value: Option<SExpr> },
    /// `return_call f(args..)` — frame-replacing static tail call.
    TailCall { func: Func, args: Vec<SExpr> },
    /// `return_call_ref sig(args..)` — frame-replacing indirect tail call;
    /// the last arg is the funcref operand, in Wasm operand order.
    TailCallRef { sig: Signature, args: Vec<SExpr> },
    /// Trap (`unreachable`).
    Unreachable,
}

/// A pure-or-impure expression tree. Mirrors the encoder's stack order:
/// `Op`'s args are evaluated left to right before the operator runs.
#[derive(Clone, Debug)]
pub enum SExpr {
    /// Read a local.
    LocalGet(SLocal),
    /// An operator application. `ty` is the operator's single result type
    /// when it has one (the audit guarantees at most one result).
    Op {
        op: Operator,
        args: Vec<SExpr>,
        ty: Option<Type>,
    },
}
