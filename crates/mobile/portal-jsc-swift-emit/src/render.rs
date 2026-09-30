//! Render the target-neutral SIR to Swift method bodies.
//!
//! Mirrors the Java renderer (`portal-jsc-jvm-emit::render`) with Swift
//! syntax and semantics:
//!
//! - labeled statements: `Block` → `label: do { ... }`, `Loop` →
//!   `label: while true { ...; break }` (one-trip), `If` → labeled `if`
//!   (branching to an if-label jumps to its end, like a block);
//! - Swift has no unreachable-statement errors, so no dead-code elision
//!   is needed; non-Void bodies end with a defensive `fatalError` after
//!   the SIR's mirrored trailing trap;
//! - wrapping integer ops (`&+`/`&-`/`&*`) match Wasm; Swift's `/`, `%`,
//!   and `Int32(Double)` trap exactly where Wasm traps; masking shifts
//!   `&<<`/`&>>` match Wasm shift-count masking; NaN/`±0`-correct
//!   `min`/`max` and saturating conversions are `W` helpers;
//! - all casts/tests route through `Any` (`(x as Any) is S19`,
//!   `(x as Any) as! S19`) because Swift rejects statically-impossible
//!   casts between unrelated classes;
//! - reference locals are Optional; member access force-unwraps (a Wasm
//!   null trap becomes a Swift runtime trap);
//! - the trampoline protocol mirrors the JVM one: `W.Step` =
//!   `W.Value`/`W.Tail`, `fN$step` bodies, `fN` wrappers, funcref boxes
//!   carry both `body` and `step` closures.

use std::collections::BTreeSet;
use std::fmt::Write as _;

use anyhow::bail;
use portal_jsc_mob_emit::names;
use portal_jsc_mob_emit::sir::{SExpr, SFunc, SLabel, SStmt};
use portal_pc_waffle::{
    EntityRef, Func, HeapType, Module, Operator, SignatureData, StorageType, Type,
};

use crate::{arr_name, elem_ty, fnbox_name, swift_ty};

/// The trampoline-body method name (`$step` is not a legal Swift identifier).
pub(crate) fn step_name(index: usize) -> String {
    format!("f{index}_step")
}

/// The Swift type of an expression, tracked so i32-valued Wasm booleans
/// are materialized as `? 1 : 0` where Wasm wants an `Int32`.
#[derive(Clone, Debug, PartialEq, Eq)]
enum JTy {
    Int,
    Long,
    Float,
    Double,
    Boolean,
    Ref,
}

/// The rendered body of one function plus its trampoline-protocol flags.
pub struct RenderedBody {
    pub needs_step: bool,
    pub has_self_tail: bool,
    pub body_src: String,
}

pub struct Renderer<'m> {
    pub module: &'m Module<'m>,
    pub tail_set: &'m BTreeSet<Func>,
    locals: Vec<Type>,
    current: Func,
    needs_step: bool,
    has_self_tail: bool,
    rets: Vec<Type>,
    temp_counter: u32,
    /// State-machine mode: label entry/exit states, when the body is too
    /// deeply nested for `swiftc`'s 256-level structure limit.
    label_entry: std::collections::HashMap<SLabel, i32>,
    label_exit: std::collections::HashMap<SLabel, i32>,
    machine_mode: bool,
}

impl<'m> Renderer<'m> {
    pub fn new(module: &'m Module<'m>, tail_set: &'m BTreeSet<Func>) -> Self {
        Self {
            module,
            tail_set,
            locals: Vec::new(),
            current: Func::invalid(),
            needs_step: false,
            has_self_tail: false,
            rets: Vec::new(),
            temp_counter: 0,
            label_entry: std::collections::HashMap::new(),
            label_exit: std::collections::HashMap::new(),
            machine_mode: false,
        }
    }

    pub fn render_func(&mut self, func: Func, sfunc: &SFunc) -> anyhow::Result<RenderedBody> {
        self.locals = sfunc.locals.clone();
        self.current = func;
        self.rets = sfunc.rets.clone();
        self.temp_counter = 0;
        let mut cross_tail = false;
        let mut self_tail = false;
        scan_tails(&sfunc.body, func, &mut cross_tail, &mut self_tail);
        self.needs_step = self.tail_set.contains(&func) || cross_tail;
        self.has_self_tail = self_tail;
        let mut out = String::new();
        for (i, &ty) in sfunc.locals.iter().enumerate().skip(sfunc.n_params) {
            let sty = swift_ty(self.module, ty);
            let default = match ty {
                Type::I32 | Type::I64 | Type::F32 | Type::F64 => "0",
                _ => "nil",
            };
            let _ = writeln!(
                out,
                "    var {}: {sty} = {default}",
                names::local_name(i as u32)
            );
        }
        // swiftc rejects structure nesting beyond 256 levels; deeply
        // nested bodies (e.g. the property-trie walkers) emit as a flat
        // state machine instead.
        if max_nesting_depth(&sfunc.body, 0) > 220 {
            self.render_machine(&sfunc.body, &mut out)?;
        } else {
            self.render_seq(&sfunc.body, &mut out, 1)?;
        }
        // Swift requires a return on every path it cannot prove is
        // `Never`; the SIR's mirrored trailing trap covers semantics but
        // not the compiler's analysis, so append a defensive trap.
        if !self.rets.is_empty() {
            let _ = writeln!(out, "    fatalError(\"unreachable end of function\")");
        }
        Ok(RenderedBody {
            needs_step: self.needs_step,
            has_self_tail: self.has_self_tail,
            body_src: out,
        })
    }

    fn render_seq(
        &mut self,
        stmts: &[SStmt],
        out: &mut String,
        indent: usize,
    ) -> anyhow::Result<()> {
        for stmt in stmts {
            self.render_stmt(stmt, out, indent)?;
        }
        Ok(())
    }

    fn render_stmt(&mut self, stmt: &SStmt, out: &mut String, indent: usize) -> anyhow::Result<()> {
        let pad = "    ".repeat(indent);
        match stmt {
            SStmt::Assign { local, expr } => {
                let ty = self.locals[*local as usize];
                let e = self.coerce(expr, ty)?;
                let _ = writeln!(out, "{pad}{} = {e}", names::local_name(*local));
            }
            SStmt::Effect { expr } => {
                if let SExpr::Op {
                    op: Operator::Nop, ..
                } = expr
                {
                    return Ok(());
                }
                let e = self.expr(expr)?;
                let _ = writeln!(out, "{pad}_ = {e}");
            }
            SStmt::Block { label, body } => {
                let _ = writeln!(out, "{pad}{}: do {{", names::label_name(*label));
                self.render_seq(body, out, indent + 1)?;
                let _ = writeln!(out, "{pad}}}");
            }
            SStmt::Loop { label, body } => {
                let _ = writeln!(out, "{pad}{}: while true {{", names::label_name(*label));
                self.render_seq(body, out, indent + 1)?;
                // One-trip exit unless the body re-enters the loop.
                if !seq_definitely_never_falls_off(body) {
                    let _ = writeln!(out, "{pad}    break");
                }
                let _ = writeln!(out, "{pad}}}");
            }
            SStmt::Break { label } => {
                if self.machine_mode {
                    let target = self.label_exit[label];
                    let _ = writeln!(out, "{pad}pc = {target}");
                } else {
                    let _ = writeln!(out, "{pad}break {}", names::label_name(*label));
                }
            }
            SStmt::Continue { label } => {
                if self.machine_mode {
                    let target = self.label_entry[label];
                    let _ = writeln!(out, "{pad}pc = {target}");
                } else {
                    let _ = writeln!(out, "{pad}continue {}", names::label_name(*label));
                }
            }
            SStmt::If {
                label,
                cond,
                if_true,
                if_false,
            } => {
                let cond = self.cond_expr(cond)?;
                let _ = writeln!(out, "{pad}{}: if {cond} {{", names::label_name(*label));
                self.render_seq(if_true, out, indent + 1)?;
                if if_false.is_empty() {
                    let _ = writeln!(out, "{pad}}}");
                } else {
                    let _ = writeln!(out, "{pad}}} else {{");
                    self.render_seq(if_false, out, indent + 1)?;
                    let _ = writeln!(out, "{pad}}}");
                }
            }
            SStmt::Return { value } => match (value, self.needs_step) {
                (Some(v), false) => {
                    let e = self.coerce(v, self.rets[0])?;
                    let _ = writeln!(out, "{pad}return {e}");
                }
                (None, false) => {
                    let _ = writeln!(out, "{pad}return");
                }
                (Some(v), true) => {
                    let e = self.coerce(v, self.rets[0])?;
                    let _ = writeln!(out, "{pad}return W.Step.value({e})");
                }
                (None, true) => {
                    let _ = writeln!(out, "{pad}return W.Step.value(nil)");
                }
            },
            SStmt::TailCall { func, args } => {
                if *func == self.current {
                    let site = self.temp_counter;
                    self.temp_counter += 1;
                    let params = self.sig_params_of_func(*func)?;
                    let mut exprs = Vec::with_capacity(params.len());
                    for (i, &ty) in params.iter().enumerate() {
                        exprs.push(self.coerce(&args[i], ty)?);
                    }
                    for (i, e) in exprs.iter().enumerate() {
                        let _ = writeln!(out, "{pad}let t{site}_{i} = {e}");
                    }
                    for (i, _) in exprs.iter().enumerate() {
                        let _ = writeln!(out, "{pad}{} = t{site}_{i}", names::local_name(i as u32));
                    }
                    let _ = writeln!(out, "{pad}continue selfTail");
                } else if self.needs_step {
                    let site = self.temp_counter;
                    self.temp_counter += 1;
                    let params = self.sig_params_of_func(*func)?;
                    let mut names_out = Vec::with_capacity(params.len());
                    for (i, &ty) in params.iter().enumerate() {
                        let e = self.coerce(&args[i], ty)?;
                        let _ = writeln!(out, "{pad}let t{site}_{i} = {e}");
                        names_out.push(format!("t{site}_{i}"));
                    }
                    let args = names_out.join(", ");
                    let _ = writeln!(
                        out,
                        "{pad}return W.Step.tail({{ {}({args}) }})",
                        step_name(func.index())
                    );
                } else {
                    bail!("cross-function tail call in a non-protocol function");
                }
            }
            SStmt::TailCallRef { sig, args } => {
                if !self.needs_step {
                    bail!("tail-call-ref in a non-protocol function");
                }
                let (params, _) = self.sig_parts(*sig)?;
                let site = self.temp_counter;
                self.temp_counter += 1;
                let mut names_out = Vec::with_capacity(params.len());
                for (i, &ty) in params.iter().enumerate() {
                    let e = self.coerce(&args[i], ty)?;
                    let _ = writeln!(out, "{pad}let t{site}_{i} = {e}");
                    names_out.push(format!("t{site}_{i}"));
                }
                let funcref = self.expr(args.last().unwrap())?;
                let _ = writeln!(out, "{pad}let t{site}_fn = {funcref}");
                let rendered = names_out.join(", ");
                let _ = writeln!(
                    out,
                    "{pad}return (t{site}_fn as! {}).step({rendered})",
                    fnbox_name(sig.index())
                );
            }
            SStmt::Unreachable => {
                let _ = writeln!(out, "{pad}fatalError(\"unreachable\")");
            }
        }
        Ok(())
    }

    fn cond_expr(&self, e: &SExpr) -> anyhow::Result<String> {
        let (s, jty) = self.expr_typed(e)?;
        Ok(match jty {
            JTy::Boolean => s,
            _ => format!("({s} != 0)"),
        })
    }

    pub fn coerce(&self, e: &SExpr, target: Type) -> anyhow::Result<String> {
        let (s, jty) = self.expr_typed(e)?;
        Ok(match (&jty, target) {
            (JTy::Boolean, Type::I32) => format!("({s} ? 1 : 0)"),
            _ => s,
        })
    }

    fn expr(&self, e: &SExpr) -> anyhow::Result<String> {
        Ok(self.expr_typed(e)?.0)
    }

    fn expr_typed(&self, e: &SExpr) -> anyhow::Result<(String, JTy)> {
        match e {
            SExpr::LocalGet(local) => {
                let jty = self
                    .locals
                    .get(*local as usize)
                    .map(|t| jty_of(*t))
                    .unwrap_or(JTy::Ref);
                Ok((names::local_name(*local), jty))
            }
            SExpr::Op { op, args, .. } => self.op_expr(op, args),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn op_expr(&self, op: &Operator, args: &[SExpr]) -> anyhow::Result<(String, JTy)> {
        use Operator::*;
        let a = |i: usize, ty: Type| self.coerce(&args[i], ty);
        let any = |i: usize| self.expr(&args[i]);
        Ok(match op {
            I32Const { value } => (i32_lit(*value as i32), JTy::Int),
            I64Const { value } => (i64_lit(*value as i64), JTy::Long),
            F32Const { value } => (
                format!("Float(bitPattern: {})", u32_lit(*value)),
                JTy::Float,
            ),
            F64Const { value } => (
                format!("Double(bitPattern: {})", u64_lit(*value)),
                JTy::Double,
            ),
            // ---- i32 ----
            I32Eqz => (format!("({} == 0)", a(0, Type::I32)?), JTy::Boolean),
            I32Eq => (
                format!("({} == {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Boolean,
            ),
            I32Ne => (
                format!("({} != {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Boolean,
            ),
            I32LtS => (
                format!("({} < {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Boolean,
            ),
            I32GtS => (
                format!("({} > {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Boolean,
            ),
            I32LeS => (
                format!("({} <= {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Boolean,
            ),
            I32GeS => (
                format!("({} >= {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Boolean,
            ),
            I32LtU => (
                format!(
                    "(UInt32(bitPattern: {}) < UInt32(bitPattern: {}))",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Boolean,
            ),
            I32GtU => (
                format!(
                    "(UInt32(bitPattern: {}) > UInt32(bitPattern: {}))",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Boolean,
            ),
            I32LeU => (
                format!(
                    "(UInt32(bitPattern: {}) <= UInt32(bitPattern: {}))",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Boolean,
            ),
            I32GeU => (
                format!(
                    "(UInt32(bitPattern: {}) >= UInt32(bitPattern: {}))",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Boolean,
            ),
            I32Clz => (
                format!("Int32(({}).leadingZeroBitCount)", a(0, Type::I32)?),
                JTy::Int,
            ),
            I32Ctz => (
                format!("Int32(({}).trailingZeroBitCount)", a(0, Type::I32)?),
                JTy::Int,
            ),
            I32Popcnt => (
                format!("Int32(({}).nonzeroBitCount)", a(0, Type::I32)?),
                JTy::Int,
            ),
            I32Add => (
                format!("({} &+ {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32Sub => (
                format!("({} &- {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32Mul => (
                format!("({} &* {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            // Swift's `/` and `%` trap exactly where Wasm traps (division
            // by zero and signed overflow).
            I32DivS => (
                format!("({} / {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32RemS => (
                format!("({} % {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32DivU => (
                format!(
                    "Int32(bitPattern: UInt32(bitPattern: {}) / UInt32(bitPattern: {}))",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Int,
            ),
            I32RemU => (
                format!(
                    "Int32(bitPattern: UInt32(bitPattern: {}) % UInt32(bitPattern: {}))",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Int,
            ),
            I32And => (
                format!("({} & {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32Or => (
                format!("({} | {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32Xor => (
                format!("({} ^ {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32Shl => (
                format!("({} &<< {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32ShrS => (
                format!("({} &>> {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32ShrU => (
                format!(
                    "Int32(bitPattern: UInt32(bitPattern: {}) &>> {})",
                    a(0, Type::I32)?,
                    a(1, Type::I32)?
                ),
                JTy::Int,
            ),
            I32Rotl => (
                format!("W.rotl32({}, {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            I32Rotr => (
                format!("W.rotr32({}, {})", a(0, Type::I32)?, a(1, Type::I32)?),
                JTy::Int,
            ),
            // ---- i64 ----
            I64Eqz => (format!("({} == 0)", a(0, Type::I64)?), JTy::Boolean),
            I64Eq => (
                format!("({} == {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Boolean,
            ),
            I64Ne => (
                format!("({} != {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Boolean,
            ),
            I64LtS => (
                format!("({} < {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Boolean,
            ),
            I64GtS => (
                format!("({} > {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Boolean,
            ),
            I64LeS => (
                format!("({} <= {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Boolean,
            ),
            I64GeS => (
                format!("({} >= {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Boolean,
            ),
            I64LtU => (
                format!(
                    "(UInt64(bitPattern: {}) < UInt64(bitPattern: {}))",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Boolean,
            ),
            I64GtU => (
                format!(
                    "(UInt64(bitPattern: {}) > UInt64(bitPattern: {}))",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Boolean,
            ),
            I64LeU => (
                format!(
                    "(UInt64(bitPattern: {}) <= UInt64(bitPattern: {}))",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Boolean,
            ),
            I64GeU => (
                format!(
                    "(UInt64(bitPattern: {}) >= UInt64(bitPattern: {}))",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Boolean,
            ),
            I64Clz => (
                format!("Int64(({}).leadingZeroBitCount)", a(0, Type::I64)?),
                JTy::Int,
            ),
            I64Ctz => (
                format!("Int64(({}).trailingZeroBitCount)", a(0, Type::I64)?),
                JTy::Int,
            ),
            I64Popcnt => (
                format!("Int64(({}).nonzeroBitCount)", a(0, Type::I64)?),
                JTy::Int,
            ),
            I64Add => (
                format!("({} &+ {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64Sub => (
                format!("({} &- {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64Mul => (
                format!("({} &* {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64DivS => (
                format!("({} / {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64RemS => (
                format!("({} % {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64DivU => (
                format!(
                    "Int64(bitPattern: UInt64(bitPattern: {}) / UInt64(bitPattern: {}))",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Long,
            ),
            I64RemU => (
                format!(
                    "Int64(bitPattern: UInt64(bitPattern: {}) % UInt64(bitPattern: {}))",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Long,
            ),
            I64And => (
                format!("({} & {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64Or => (
                format!("({} | {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64Xor => (
                format!("({} ^ {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64Shl => (
                format!("({} &<< {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64ShrS => (
                format!("({} &>> {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64ShrU => (
                format!(
                    "Int64(bitPattern: UInt64(bitPattern: {}) &>> {})",
                    a(0, Type::I64)?,
                    a(1, Type::I64)?
                ),
                JTy::Long,
            ),
            I64Rotl => (
                format!("W.rotl64({}, {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            I64Rotr => (
                format!("W.rotr64({}, {})", a(0, Type::I64)?, a(1, Type::I64)?),
                JTy::Long,
            ),
            // ---- f32 ----
            F32Eq => (
                format!("({} == {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Boolean,
            ),
            F32Ne => (
                format!("({} != {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Boolean,
            ),
            F32Lt => (
                format!("({} < {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Boolean,
            ),
            F32Gt => (
                format!("({} > {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Boolean,
            ),
            F32Le => (
                format!("({} <= {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Boolean,
            ),
            F32Ge => (
                format!("({} >= {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Boolean,
            ),
            F32Abs => (format!("({}).magnitude", a(0, Type::F32)?), JTy::Float),
            F32Neg => (format!("(-{})", a(0, Type::F32)?), JTy::Float),
            F32Ceil => (format!("({}).rounded(.up)", a(0, Type::F32)?), JTy::Float),
            F32Floor => (format!("({}).rounded(.down)", a(0, Type::F32)?), JTy::Float),
            F32Trunc => (
                format!("({}).rounded(.towardZero)", a(0, Type::F32)?),
                JTy::Float,
            ),
            F32Nearest => (
                format!("({}).rounded(.toNearestOrEven)", a(0, Type::F32)?),
                JTy::Float,
            ),
            F32Sqrt => (format!("({}).squareRoot()", a(0, Type::F32)?), JTy::Float),
            F32Add => (
                format!("({} + {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Float,
            ),
            F32Sub => (
                format!("({} - {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Float,
            ),
            F32Mul => (
                format!("({} * {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Float,
            ),
            F32Div => (
                format!("({} / {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Float,
            ),
            F32Min => (
                format!("W.f32min({}, {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Float,
            ),
            F32Max => (
                format!("W.f32max({}, {})", a(0, Type::F32)?, a(1, Type::F32)?),
                JTy::Float,
            ),
            F32Copysign => (
                format!(
                    "Float(signOf: {}, magnitudeOf: {})",
                    a(1, Type::F32)?,
                    a(0, Type::F32)?
                ),
                JTy::Float,
            ),
            // ---- f64 ----
            F64Eq => (
                format!("({} == {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Boolean,
            ),
            F64Ne => (
                format!("({} != {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Boolean,
            ),
            F64Lt => (
                format!("({} < {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Boolean,
            ),
            F64Gt => (
                format!("({} > {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Boolean,
            ),
            F64Le => (
                format!("({} <= {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Boolean,
            ),
            F64Ge => (
                format!("({} >= {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Boolean,
            ),
            F64Abs => (format!("({}).magnitude", a(0, Type::F64)?), JTy::Double),
            F64Neg => (format!("(-{})", a(0, Type::F64)?), JTy::Double),
            F64Ceil => (format!("({}).rounded(.up)", a(0, Type::F64)?), JTy::Double),
            F64Floor => (
                format!("({}).rounded(.down)", a(0, Type::F64)?),
                JTy::Double,
            ),
            F64Trunc => (
                format!("({}).rounded(.towardZero)", a(0, Type::F64)?),
                JTy::Double,
            ),
            F64Nearest => (
                format!("({}).rounded(.toNearestOrEven)", a(0, Type::F64)?),
                JTy::Double,
            ),
            F64Sqrt => (format!("({}).squareRoot()", a(0, Type::F64)?), JTy::Double),
            F64Add => (
                format!("({} + {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Double,
            ),
            F64Sub => (
                format!("({} - {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Double,
            ),
            F64Mul => (
                format!("({} * {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Double,
            ),
            F64Div => (
                format!("({} / {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Double,
            ),
            F64Min => (
                format!("W.f64min({}, {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Double,
            ),
            F64Max => (
                format!("W.f64max({}, {})", a(0, Type::F64)?, a(1, Type::F64)?),
                JTy::Double,
            ),
            F64Copysign => (
                format!(
                    "Double(signOf: {}, magnitudeOf: {})",
                    a(1, Type::F64)?,
                    a(0, Type::F64)?
                ),
                JTy::Double,
            ),
            // ---- conversions ----
            I32WrapI64 => (
                format!("Int32(truncatingIfNeeded: {})", a(0, Type::I64)?),
                JTy::Int,
            ),
            // Swift's trapping conversions trap exactly where Wasm traps.
            I32TruncF32S => (format!("Int32({})", a(0, Type::F32)?), JTy::Int),
            I32TruncF64S => (format!("Int32({})", a(0, Type::F64)?), JTy::Int),
            I32TruncF32U => (
                format!("Int32(bitPattern: UInt32({}))", a(0, Type::F32)?),
                JTy::Int,
            ),
            I32TruncF64U => (
                format!("Int32(bitPattern: UInt32({}))", a(0, Type::F64)?),
                JTy::Int,
            ),
            I64ExtendI32S => (format!("Int64({})", a(0, Type::I32)?), JTy::Long),
            I64ExtendI32U => (
                format!("Int64(UInt32(bitPattern: {}))", a(0, Type::I32)?),
                JTy::Long,
            ),
            I64TruncF32S => (format!("Int64({})", a(0, Type::F32)?), JTy::Long),
            I64TruncF64S => (format!("Int64({})", a(0, Type::F64)?), JTy::Long),
            I64TruncF32U => (
                format!("Int64(bitPattern: UInt64({}))", a(0, Type::F32)?),
                JTy::Long,
            ),
            I64TruncF64U => (
                format!("Int64(bitPattern: UInt64({}))", a(0, Type::F64)?),
                JTy::Long,
            ),
            F32ConvertI32S => (format!("Float({})", a(0, Type::I32)?), JTy::Float),
            F32ConvertI32U => (
                format!("Float(UInt32(bitPattern: {}))", a(0, Type::I32)?),
                JTy::Float,
            ),
            F32ConvertI64S => (format!("Float({})", a(0, Type::I64)?), JTy::Float),
            F32ConvertI64U => (
                format!("Float(UInt64(bitPattern: {}))", a(0, Type::I64)?),
                JTy::Float,
            ),
            F32DemoteF64 => (format!("Float({})", a(0, Type::F64)?), JTy::Float),
            F64ConvertI32S => (format!("Double({})", a(0, Type::I32)?), JTy::Double),
            F64ConvertI32U => (
                format!("Double(UInt32(bitPattern: {}))", a(0, Type::I32)?),
                JTy::Double,
            ),
            F64ConvertI64S => (format!("Double({})", a(0, Type::I64)?), JTy::Double),
            F64ConvertI64U => (
                format!("Double(UInt64(bitPattern: {}))", a(0, Type::I64)?),
                JTy::Double,
            ),
            F64PromoteF32 => (format!("Double({})", a(0, Type::F32)?), JTy::Double),
            I32Extend8S => (
                format!("Int32(Int8(truncatingIfNeeded: {}))", a(0, Type::I32)?),
                JTy::Int,
            ),
            I32Extend16S => (
                format!("Int32(Int16(truncatingIfNeeded: {}))", a(0, Type::I32)?),
                JTy::Int,
            ),
            I64Extend8S => (
                format!("Int64(Int8(truncatingIfNeeded: {}))", a(0, Type::I64)?),
                JTy::Long,
            ),
            I64Extend16S => (
                format!("Int64(Int16(truncatingIfNeeded: {}))", a(0, Type::I64)?),
                JTy::Long,
            ),
            I64Extend32S => (
                format!("Int64(Int32(truncatingIfNeeded: {}))", a(0, Type::I64)?),
                JTy::Long,
            ),
            I32TruncSatF32S | I32TruncSatF64S => (format!("W.truncSatS32({})", any(0)?), JTy::Int),
            I32TruncSatF32U | I32TruncSatF64U => (format!("W.truncSatU32({})", any(0)?), JTy::Int),
            I64TruncSatF32S | I64TruncSatF64S => (format!("W.truncSatS64({})", any(0)?), JTy::Long),
            I64TruncSatF32U | I64TruncSatF64U => (format!("W.truncSatU64({})", any(0)?), JTy::Long),
            F32ReinterpretI32 => (
                format!(
                    "Float(bitPattern: UInt32(bitPattern: {}))",
                    a(0, Type::I32)?
                ),
                JTy::Float,
            ),
            I32ReinterpretF32 => (
                format!("Int32(bitPattern: ({}).bitPattern)", a(0, Type::F32)?),
                JTy::Int,
            ),
            F64ReinterpretI64 => (
                format!(
                    "Double(bitPattern: UInt64(bitPattern: {}))",
                    a(0, Type::I64)?
                ),
                JTy::Double,
            ),
            I64ReinterpretF64 => (
                format!("Int64(bitPattern: ({}).bitPattern)", a(0, Type::F64)?),
                JTy::Long,
            ),
            // ---- select ----
            TypedSelect { ty } => {
                let cond = self.cond_expr(&args[2])?;
                let t = self.coerce(&args[0], *ty)?;
                let f = self.coerce(&args[1], *ty)?;
                let jty = match ty {
                    Type::I32 => JTy::Int,
                    Type::I64 => JTy::Long,
                    Type::F32 => JTy::Float,
                    Type::F64 => JTy::Double,
                    _ => JTy::Ref,
                };
                (format!("({cond} ? {t} : {f})"), jty)
            }
            Select => bail!("untyped select is outside the mobile closure"),
            // ---- calls ----
            Call { function_index } => {
                let params = self.sig_params_of_func(*function_index)?;
                let rendered = params
                    .iter()
                    .enumerate()
                    .map(|(i, &ty)| self.coerce(&args[i], ty))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .join(", ");
                let jty = self.func_ret_jty(*function_index)?;
                (
                    format!("{}({rendered})", names::func_name(function_index.index())),
                    jty,
                )
            }
            CallRef { sig_index } => {
                let (params, rets) = self.sig_parts(*sig_index)?;
                let rendered = params
                    .iter()
                    .enumerate()
                    .map(|(i, &ty)| self.coerce(&args[i], ty))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .join(", ");
                let funcref = self.expr(args.last().unwrap())?;
                let jty = rets.first().map(|t| jty_of(*t)).unwrap_or(JTy::Ref);
                (
                    format!(
                        "(({funcref}) as! {}).body({rendered})",
                        fnbox_name(sig_index.index())
                    ),
                    jty,
                )
            }
            // ---- structs ----
            StructNew { sig: sig_index } => {
                let fields = self.struct_field_tys(*sig_index)?;
                let name = names::struct_name(sig_index.index());
                let rendered = fields
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| Ok(format!("f{i}: {}", self.coerce(&args[i], *ty)?)))
                    .collect::<anyhow::Result<Vec<String>>>()?
                    .join(", ");
                (format!("{name}({rendered})"), JTy::Ref)
            }
            StructNewDefault { sig: sig_index } => (
                format!("{}()", names::struct_name(sig_index.index())),
                JTy::Ref,
            ),
            StructGet {
                sig: sig_index,
                idx,
            } => {
                let fields = self.struct_field_tys(*sig_index)?;
                let jty = fields.get(*idx).map(|t| jty_of(*t)).unwrap_or(JTy::Ref);
                (
                    format!(
                        "((({}) as! {}).f{idx})",
                        any(0)?,
                        names::struct_name(sig_index.index())
                    ),
                    jty,
                )
            }
            StructSet {
                sig: sig_index,
                idx,
            } => {
                let fields = self.struct_field_tys(*sig_index)?;
                let fty = fields[*idx];
                (
                    format!(
                        "((({}) as! {}).f{idx} = {})",
                        any(0)?,
                        names::struct_name(sig_index.index()),
                        self.coerce(&args[1], fty)?
                    ),
                    JTy::Ref,
                )
            }
            StructGetS { .. } | StructGetU { .. } => {
                bail!("packed struct fields are outside the mobile closure")
            }
            // ---- arrays ----
            ArrayNewFixed {
                sig: array_type_index,
                ..
            } => {
                let elem = self.array_elem(*array_type_index)?;
                let name = arr_name(array_type_index.index());
                let rendered = (0..args.len())
                    .map(|i| self.array_elem_expr(elem, i, args))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .join(", ");
                (format!("{name}([{rendered}])"), JTy::Ref)
            }
            ArrayNewDefault {
                sig: array_type_index,
            } => {
                let elem = self.array_elem(*array_type_index)?;
                let name = arr_name(array_type_index.index());
                let default = match elem {
                    StorageType::I8 | StorageType::I16 => "0".to_string(),
                    StorageType::Val(ty) => match ty {
                        Type::I32 | Type::I64 | Type::F32 | Type::F64 => "0".to_string(),
                        _ => "nil".to_string(),
                    },
                    _ => bail!("array storage outside the mobile closure"),
                };
                let ety = elem_ty(self.module, elem);
                (
                    format!(
                        "{name}([{ety}](repeating: {default}, count: Int({})))",
                        a(0, Type::I32)?
                    ),
                    JTy::Ref,
                )
            }
            ArrayNew {
                sig: array_type_index,
            } => {
                let elem = self.array_elem(*array_type_index)?;
                let name = arr_name(array_type_index.index());
                let init = match elem {
                    StorageType::I8 => format!("UInt8(truncatingIfNeeded: {})", a(0, Type::I32)?),
                    StorageType::I16 => format!("UInt16(truncatingIfNeeded: {})", a(0, Type::I32)?),
                    StorageType::Val(ty) => self.coerce(&args[0], ty)?,
                    _ => bail!("array storage outside the mobile closure"),
                };
                let ety = elem_ty(self.module, elem);
                (
                    format!(
                        "{name}([{ety}](repeating: {init}, count: Int({})))",
                        a(1, Type::I32)?
                    ),
                    JTy::Ref,
                )
            }
            ArrayGet {
                sig: array_type_index,
            }
            | ArrayGetS {
                sig: array_type_index,
            }
            | ArrayGetU {
                sig: array_type_index,
            } => {
                let elem = self.array_elem(*array_type_index)?;
                let aty = arr_name(array_type_index.index());
                let arr = any(0)?;
                let idx = a(1, Type::I32)?;
                let is_unsigned = matches!(op, ArrayGetU { .. });
                let is_signed = matches!(op, ArrayGetS { .. });
                let get = format!("((({arr}) as! {aty}).items[Int({idx})])");
                let rendered = match elem {
                    StorageType::I8 if is_unsigned => format!("Int32({get})"),
                    StorageType::I8 if !is_unsigned || is_signed => {
                        format!("Int32(Int8(bitPattern: {get}))")
                    }
                    StorageType::I16 if is_unsigned => format!("Int32({get})"),
                    StorageType::I16 if is_signed => format!("Int32(Int16(bitPattern: {get}))"),
                    StorageType::I16 => format!("Int32({get})"),
                    _ => get,
                };
                let jty = match elem {
                    StorageType::I8 | StorageType::I16 => JTy::Int,
                    StorageType::Val(ty) => jty_of(ty),
                    _ => JTy::Ref,
                };
                (rendered, jty)
            }
            ArraySet {
                sig: array_type_index,
            } => {
                let elem = self.array_elem(*array_type_index)?;
                let arr = any(0)?;
                let idx = a(1, Type::I32)?;
                let val = match elem {
                    StorageType::I8 => format!("UInt8(truncatingIfNeeded: {})", a(2, Type::I32)?),
                    StorageType::I16 => format!("UInt16(truncatingIfNeeded: {})", a(2, Type::I32)?),
                    StorageType::Val(ty) => self.coerce(&args[2], ty)?,
                    _ => bail!("array storage outside the mobile closure"),
                };
                let aty = arr_name(array_type_index.index());
                (
                    format!("((({arr}) as! {aty}).items[Int({idx})] = {val})"),
                    JTy::Ref,
                )
            }
            ArrayLen => (format!("Int32(W.arrLen({}))", any(0)?), JTy::Int),
            ArrayCopy { dest, src } => {
                let dst = any(0)?;
                let dst_off = a(1, Type::I32)?;
                let src_e = any(2)?;
                let src_off = a(3, Type::I32)?;
                let len = a(4, Type::I32)?;
                let dty = arr_name(dest.index());
                let sty = arr_name(src.index());
                (
                    format!(
                        "W.arrayCopy(({dst}) as! {dty}, Int({dst_off}), ({src_e}) as! {sty}, Int({src_off}), Int({len}))"
                    ),
                    JTy::Ref,
                )
            }
            // ---- references ----
            RefNull { .. } => ("nil".to_string(), JTy::Ref),
            RefIsNull => (format!("({} == nil)", any(0)?), JTy::Boolean),
            RefFunc { func_index } => {
                let sig = self.sig_of_func(*func_index)?;
                let box_name = fnbox_name(sig.index());
                (
                    format!(
                        "{}(body: {}, step: {})",
                        box_name,
                        names::func_name(func_index.index()),
                        step_name(func_index.index())
                    ),
                    JTy::Ref,
                )
            }
            RefTest { ty } => (self.ref_test(*ty, &any(0)?)?, JTy::Boolean),
            RefCast { ty } => (self.ref_cast(*ty, &any(0)?)?, JTy::Ref),
            RefEq => (
                format!(
                    "(({} as AnyObject?) === ({} as AnyObject?))",
                    any(0)?,
                    any(1)?
                ),
                JTy::Boolean,
            ),
            RefI31 => ("JsNull.shared".to_string(), JTy::Ref),
            I31GetS | I31GetU => {
                bail!("i31 payload reads are outside the mobile closure (sentinel-only)")
            }
            Unreachable => bail!("unreachable has no value"),
            Nop => bail!("nop has no value"),
            other => bail!("operator {other:?} is outside the mobile closure"),
        })
    }

    // ---- helpers ----

    fn sig_parts(
        &self,
        sig: portal_pc_waffle::Signature,
    ) -> anyhow::Result<(Vec<Type>, Vec<Type>)> {
        match &self.module.signatures[sig] {
            SignatureData::Func {
                params, returns, ..
            } => Ok((params.clone(), returns.clone())),
            _ => bail!("signature {sig:?} is not a function signature"),
        }
    }

    fn sig_params_of_func(&self, func: Func) -> anyhow::Result<Vec<Type>> {
        Ok(self.sig_parts(self.sig_of_func(func)?)?.0)
    }

    fn func_ret_jty(&self, func: Func) -> anyhow::Result<JTy> {
        let (_, rets) = self.sig_parts(self.sig_of_func(func)?)?;
        Ok(rets.first().map(|t| jty_of(*t)).unwrap_or(JTy::Ref))
    }

    fn sig_of_func(&self, func: Func) -> anyhow::Result<portal_pc_waffle::Signature> {
        match &self.module.funcs[func] {
            portal_pc_waffle::FuncDecl::Body(sig, _, _) => Ok(*sig),
            _ => bail!("function {func:?} has no body in the mobile closure"),
        }
    }

    fn struct_field_tys(&self, sig: portal_pc_waffle::Signature) -> anyhow::Result<Vec<Type>> {
        match &self.module.signatures[sig] {
            SignatureData::Struct { fields, .. } => fields
                .iter()
                .map(|f| match f.value {
                    StorageType::Val(ty) => Ok(ty),
                    _ => bail!("packed struct fields are outside the mobile closure"),
                })
                .collect(),
            _ => bail!("signature {sig:?} is not a struct"),
        }
    }

    fn array_elem(&self, sig: portal_pc_waffle::Signature) -> anyhow::Result<StorageType> {
        match &self.module.signatures[sig] {
            SignatureData::Array { ty, .. } => Ok(ty.value),
            _ => bail!("signature {sig:?} is not an array"),
        }
    }

    fn array_elem_expr(
        &self,
        elem: StorageType,
        i: usize,
        args: &[SExpr],
    ) -> anyhow::Result<String> {
        match elem {
            StorageType::I8 => Ok(format!(
                "UInt8(truncatingIfNeeded: {})",
                self.coerce(&args[i], Type::I32)?
            )),
            StorageType::I16 => Ok(format!(
                "UInt16(truncatingIfNeeded: {})",
                self.coerce(&args[i], Type::I32)?
            )),
            StorageType::Val(ty) => self.coerce(&args[i], ty),
            _ => bail!("array storage outside the mobile closure"),
        }
    }

    fn ref_test(&self, ty: Type, e: &str) -> anyhow::Result<String> {
        let Type::Heap(h) = ty else {
            bail!("ref.test of a non-reference type {ty:?}")
        };
        Ok(match h.value {
            HeapType::Sig { sig_index } => match &self.module.signatures[sig_index] {
                SignatureData::Struct { .. } => {
                    format!("(({}) is {})", e, names::struct_name(sig_index.index()))
                }
                SignatureData::Array { .. } => {
                    format!("(({}) is {})", e, arr_name(sig_index.index()))
                }
                SignatureData::Func { .. } => {
                    format!("(({}) is {})", e, fnbox_name(sig_index.index()))
                }
                _ => bail!("ref.test target outside the mobile closure"),
            },
            HeapType::FuncRef => format!("(({e}) is IFun)"),
            HeapType::Any | HeapType::Eq => format!("(({e}) != nil)"),
            HeapType::I31 => format!("(({e}) is JsNull)"),
            HeapType::Struct => format!("(({e}) is IStruct)"),
            HeapType::Array => bail!("abstract ref.test array is unsupported in Swift emission"),
            HeapType::None | HeapType::NoFunc => "false".to_string(),
            other => bail!("ref.test heap type {other:?} is outside the mobile closure"),
        })
    }

    fn ref_cast(&self, ty: Type, e: &str) -> anyhow::Result<String> {
        let Type::Heap(h) = ty else {
            bail!("ref.cast of a non-reference type {ty:?}")
        };
        Ok(match h.value {
            HeapType::Sig { sig_index } => match &self.module.signatures[sig_index] {
                SignatureData::Struct { .. } => {
                    format!("(({}) as! {})", e, names::struct_name(sig_index.index()))
                }
                SignatureData::Array { .. } => {
                    format!("(({}) as! {})", e, arr_name(sig_index.index()))
                }
                SignatureData::Func { .. } => {
                    format!("(({}) as! {})", e, fnbox_name(sig_index.index()))
                }
                _ => bail!("ref.cast target outside the mobile closure"),
            },
            HeapType::FuncRef => format!("(({e}) as! IFun)"),
            HeapType::Any | HeapType::Eq => e.to_string(),
            HeapType::I31 => format!("(({e}) as! JsNull)"),
            HeapType::None | HeapType::NoFunc => "fatalError(\"cast to bottom type\")".to_string(),
            other => bail!("ref.cast heap type {other:?} is outside the mobile closure"),
        })
    }
}

/// The deepest nesting level in a statement sequence.
fn max_nesting_depth(stmts: &[SStmt], base: usize) -> usize {
    let mut depth = base;
    for stmt in stmts {
        match stmt {
            SStmt::Block { body, .. } | SStmt::Loop { body, .. } => {
                depth = depth.max(max_nesting_depth(body, base + 1))
            }
            SStmt::If {
                if_true, if_false, ..
            } => {
                depth = depth.max(max_nesting_depth(if_true, base + 1));
                depth = depth.max(max_nesting_depth(if_false, base + 1));
            }
            _ => {}
        }
    }
    depth
}

impl<'m> Renderer<'m> {
    /// Render the body as a flat `switch`-in-`while` state machine:
    /// every statement sequence becomes a state, structured constructs
    /// split states, and branches become `pc` assignments. Used when the
    /// structured nesting would exceed swiftc's 256-level limit.
    fn render_machine(&mut self, body: &[SStmt], out: &mut String) -> anyhow::Result<()> {
        // Pass 1: linearize into states (reverse order threads the
        // fallthrough target naturally).
        self.label_entry.clear();
        self.label_exit.clear();
        let mut flat = Flatten {
            states: Vec::new(),
            label_entry: std::collections::HashMap::new(),
            label_exit: std::collections::HashMap::new(),
        };
        let entry = flat.flatten_seq(body);
        self.label_entry = flat.label_entry;
        self.label_exit = flat.label_exit;
        self.machine_mode = true;
        // Pass 2: emit the states.
        let _ = writeln!(out, "    var pc: Int32 = {entry}");
        let _ = writeln!(out, "    machine: while true {{");
        let _ = writeln!(out, "        switch pc {{");
        for (i, state) in flat.states.iter().enumerate() {
            let _ = writeln!(out, "        case {i}:");
            match state {
                FlatState::FellOff => {
                    let _ = writeln!(
                        out,
                        "            fatalError(\"fell off the state machine\")"
                    );
                }
                FlatState::Stmts { stmts, next } => {
                    let mut text = String::new();
                    let mut terminal = false;
                    for stmt in stmts {
                        self.render_stmt(stmt, &mut text, 3)?;
                        terminal |= matches!(
                            stmt,
                            SStmt::Return { .. }
                                | SStmt::TailCall { .. }
                                | SStmt::TailCallRef { .. }
                                | SStmt::Unreachable
                        );
                    }
                    out.push_str(&text);
                    if !terminal {
                        let _ = writeln!(out, "            pc = {next}");
                    }
                }
                FlatState::If {
                    cond,
                    then_s,
                    else_s,
                } => {
                    let cond = self.cond_expr(cond)?;
                    let _ = writeln!(out, "            if {cond} {{");
                    let _ = writeln!(out, "                pc = {then_s}");
                    let _ = writeln!(out, "            }} else {{");
                    let _ = writeln!(out, "                pc = {else_s}");
                    let _ = writeln!(out, "            }}");
                }
            }
        }
        let _ = writeln!(out, "        default:");
        let _ = writeln!(out, "            fatalError(\"bad state\")");
        let _ = writeln!(out, "        }}");
        let _ = writeln!(out, "    }}");
        self.machine_mode = false;
        Ok(())
    }
}

enum FlatState {
    /// The empty fall-off-the-end state (unreachable per the SIR's
    /// mirrored trailing trap, but it must exist as a target).
    FellOff,
    /// A run of leaf statements with a fallthrough state.
    Stmts { stmts: Vec<SStmt>, next: i32 },
    /// A conditional branch to two states.
    If {
        cond: SExpr,
        then_s: i32,
        else_s: i32,
    },
}

struct Flatten {
    states: Vec<FlatState>,
    label_entry: std::collections::HashMap<SLabel, i32>,
    label_exit: std::collections::HashMap<SLabel, i32>,
}

impl Flatten {
    fn fresh(&mut self, state: FlatState) -> i32 {
        self.states.push(state);
        (self.states.len() - 1) as i32
    }

    /// Linearize a sequence, returning its entry state. `next` (the state
    /// to run after the sequence completes) is created by the caller via
    /// [`Self::flatten_seq_with_exit`].
    fn flatten_seq(&mut self, stmts: &[SStmt]) -> i32 {
        let exit = self.fresh(FlatState::FellOff);
        self.flatten_seq_with_exit(stmts, exit)
    }

    fn flatten_seq_with_exit(&mut self, stmts: &[SStmt], exit: i32) -> i32 {
        let mut next = exit;
        for stmt in stmts.iter().rev() {
            next = match stmt {
                SStmt::Block { label, body } => {
                    let entry = self.flatten_seq_with_exit(body, next);
                    self.label_exit.insert(*label, next);
                    entry
                }
                SStmt::Loop { label, body } => {
                    let entry = self.flatten_seq_with_exit(body, next);
                    self.label_entry.insert(*label, entry);
                    self.label_exit.insert(*label, next);
                    entry
                }
                SStmt::If {
                    label,
                    cond,
                    if_true,
                    if_false,
                } => {
                    let then_s = self.flatten_seq_with_exit(if_true, next);
                    let else_s = if if_false.is_empty() {
                        next
                    } else {
                        self.flatten_seq_with_exit(if_false, next)
                    };
                    self.label_exit.insert(*label, next);
                    self.fresh(FlatState::If {
                        cond: cond.clone(),
                        then_s,
                        else_s,
                    })
                }
                leaf => self.fresh(FlatState::Stmts {
                    stmts: vec![leaf.clone()],
                    next,
                }),
            };
        }
        next
    }
}

/// Scan for self/cross tail calls (determines the protocol membership).
fn scan_tails(stmts: &[SStmt], current: Func, cross: &mut bool, self_tail: &mut bool) {
    for stmt in stmts {
        match stmt {
            SStmt::TailCall { func, .. } => {
                if *func == current {
                    *self_tail = true;
                } else {
                    *cross = true;
                }
            }
            SStmt::TailCallRef { .. } => *cross = true,
            SStmt::Block { body, .. } | SStmt::Loop { body, .. } => {
                scan_tails(body, current, cross, self_tail)
            }
            SStmt::If {
                if_true, if_false, ..
            } => {
                scan_tails(if_true, current, cross, self_tail);
                scan_tails(if_false, current, cross, self_tail);
            }
            _ => {}
        }
    }
}

/// A conservative "can never fall off the end" analysis for the one-trip
/// loop break. Wrong answers are harmless in Swift (no unreachable-code
/// errors, and the trailing `fatalError` satisfies the return analysis).
fn seq_definitely_never_falls_off(stmts: &[SStmt]) -> bool {
    match stmts.last() {
        Some(
            SStmt::Break { .. }
            | SStmt::Continue { .. }
            | SStmt::Return { .. }
            | SStmt::TailCall { .. }
            | SStmt::TailCallRef { .. }
            | SStmt::Unreachable,
        ) => true,
        Some(SStmt::If {
            if_true, if_false, ..
        }) => {
            !if_false.is_empty()
                && seq_definitely_never_falls_off(if_true)
                && seq_definitely_never_falls_off(if_false)
        }
        _ => false,
    }
}

fn jty_of(ty: Type) -> JTy {
    match ty {
        Type::I32 => JTy::Int,
        Type::I64 => JTy::Long,
        Type::F32 => JTy::Float,
        Type::F64 => JTy::Double,
        _ => JTy::Ref,
    }
}

/// Swift Int32 literal.
pub fn i32_lit(v: i32) -> String {
    if v == i32::MIN {
        "(-2147483647 - 1)".to_string()
    } else {
        v.to_string()
    }
}

/// Swift Int64 literal (no suffix; context infers the type).
pub fn i64_lit(v: i64) -> String {
    if v == i64::MIN {
        "(-9223372036854775807 - 1)".to_string()
    } else {
        v.to_string()
    }
}

pub fn u32_lit(v: u32) -> String {
    format!("UInt32(0x{v:08x})")
}

pub fn u64_lit(v: u64) -> String {
    format!("UInt64(0x{v:016x})")
}
