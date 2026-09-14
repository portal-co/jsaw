//! Render the target-neutral SIR to Java method bodies.
//!
//! One renderer instance per module; [`render_func`] renders one function.
//! The renderer mirrors Wasm semantics per operator (see the numeric
//! helpers on `W`) and handles Java's `javac` reachability rules by
//! eliding statements that follow an unconditional terminator within a
//! sequence — Wasm-shaped control flow that `javac` would reject as
//! unreachable — while treating loops conservatively (the emitted
//! `while (W.T)` has a non-constant condition, so `javac` always considers
//! their fall-through reachable).

use std::collections::BTreeSet;
use std::fmt::Write as _;

use anyhow::bail;
use portal_jsc_mob_emit::names;
use portal_jsc_mob_emit::sir::{SExpr, SFunc, SLabel, SStmt};
use portal_pc_waffle::{EntityRef, Func, HeapType, Module, Operator, SignatureData, StorageType, Type};

use crate::{array_ty, iface_name, java_ty};

/// The Java type of an expression, tracked so i32-valued Wasm booleans
/// (comparisons, `ref.test`, `ref.eq`) are materialized as `? 1 : 0` where
/// Wasm wants an `i32`.
#[derive(Clone, Debug, PartialEq, Eq)]
enum JTy {
    Int,
    Long,
    Float,
    Double,
    /// A Java `boolean` (a Wasm i32 comparison before materialization).
    Boolean,
    /// Any reference type.
    Ref,
}

pub struct Renderer<'m> {
    pub module: &'m Module<'m>,
    /// Functions targeted by (or containing) cross-function tail calls:
    /// members of the trampoline protocol.
    pub tail_set: &'m BTreeSet<Func>,
    broken_to: BTreeSet<SLabel>,
    /// Wasm types of the current function's locals (set by `render_func`).
    locals: Vec<Type>,
    /// The function being rendered (for self-tail detection).
    current: Func,
    /// The current function participates in the Step protocol.
    needs_step: bool,
    /// The current function tail-calls itself (rendered as a loop).
    has_self_tail: bool,
    /// The current function's return types.
    rets: Vec<Type>,
    /// Per-site temp counter for self-tail argument assignment.
    temp_counter: u32,
    /// Optional mutable frame variable used when a large Java method is split
    /// into helpers. Every Wasm local then resolves through this object.
    frame: Option<String>,
}

impl<'m> Renderer<'m> {
    pub fn new(module: &'m Module<'m>, tail_set: &'m BTreeSet<Func>) -> Self {
        Self {
            module,
            tail_set,
            broken_to: BTreeSet::new(),
            locals: Vec::new(),
            current: Func::invalid(),
            needs_step: false,
            has_self_tail: false,
            rets: Vec::new(),
            temp_counter: 0,
            frame: None,
        }
    }

    /// Render a straight-line body fragment against a mutable local frame.
    /// The splitter only passes top-level assignment/effect ranges, so no
    /// branch label can cross the Java method boundary.
    pub fn render_fragment_in_frame(
        &mut self,
        func: Func,
        sfunc: &SFunc,
        stmts: &[SStmt],
        frame: &str,
    ) -> anyhow::Result<String> {
        self.broken_to = BTreeSet::new();
        self.locals = sfunc.locals.clone();
        self.current = func;
        self.rets = sfunc.rets.clone();
        self.temp_counter = 0;
        self.needs_step = false;
        self.has_self_tail = false;
        self.frame = Some(frame.to_owned());
        let mut out = String::new();
        self.render_seq(stmts, &mut out, 1)?;
        self.frame = None;
        Ok(out)
    }

    fn local_name(&self, local: u32) -> String {
        match &self.frame {
            Some(frame) => format!("{frame}.{}", names::local_name(local)),
            None => names::local_name(local),
        }
    }

    /// Render one function body to a Java method body (without the method
    /// signature line and outer braces).
    ///
    /// The returned [`RenderedBody`] reports whether the function
    /// participates in the trampoline protocol (`needs_step`: the body
    /// returns `W.Step` and belongs in `fN$step`) and whether it tail-calls
    /// itself (`has_self_tail`: the body must be wrapped in a `selfTail`
    /// loop).
    pub fn render_func(&mut self, func: Func, sfunc: &SFunc) -> anyhow::Result<RenderedBody> {
        self.broken_to = BTreeSet::new();
        collect_breaks(&sfunc.body, &mut self.broken_to);
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
        // Non-param locals, default-initialized so `javac`'s definite
        // assignment analysis is satisfied along Wasm CFG paths.
        for (i, &ty) in sfunc.locals.iter().enumerate().skip(sfunc.n_params) {
            let jty = java_ty(self.module, ty);
            let default = match ty {
                Type::I32 | Type::I64 | Type::F32 | Type::F64 => "0",
                _ => "null",
            };
            let _ = writeln!(out, "    {jty} {} = {default};", names::local_name(i as u32));
        }
        self.render_seq(&sfunc.body, &mut out, 1)?;
        Ok(RenderedBody {
            needs_step: self.needs_step,
            has_self_tail: self.has_self_tail,
            body_src: out,
        })
    }

    /// Render one statement at `indent` levels. The structured arms
    /// (Block/Loop/If) recurse with deliberately thin stack frames — Wasm
    /// CFGs nest arbitrarily deeply — while all leaf work happens in
    /// [`Self::render_leaf`], kept out of the recursive frames.
    fn render_stmt_indented(
        &mut self,
        stmt: &SStmt,
        out: &mut String,
        indent: usize,
    ) -> anyhow::Result<()> {
        let pad = "    ".repeat(indent);
        match stmt {
            SStmt::Block { label, body } => {
                let _ = writeln!(out, "{pad}{}: {{", names::label_name(*label));
                self.render_seq(body, out, indent + 1)?;
                let _ = writeln!(out, "{pad}}}");
            }
            SStmt::Loop { label, body } => {
                let _ = writeln!(out, "{pad}{}: while (W.T) {{", names::label_name(*label));
                self.render_seq(body, out, indent + 1)?;
                // Wasm loops run one trip unless re-entered by a branch to
                // their label; the trailing break provides the one-trip
                // exit. Elided when the body cannot fall through.
                if !self.seq_terminates(body) {
                    let _ = writeln!(out, "{pad}    break;");
                }
                let _ = writeln!(out, "{pad}}}");
            }
            SStmt::If {
                label,
                cond,
                if_true,
                if_false,
            } => {
                let cond = self.cond_expr(cond)?;
                let _ = writeln!(out, "{pad}{}: if ({cond}) {{", names::label_name(*label));
                self.render_seq(if_true, out, indent + 1)?;
                if if_false.is_empty() {
                    let _ = writeln!(out, "{pad}}}");
                } else {
                    let _ = writeln!(out, "{pad}}} else {{");
                    self.render_seq(if_false, out, indent + 1)?;
                    let _ = writeln!(out, "{pad}}}");
                }
            }
            _ => {
                let mut inner = String::new();
                self.render_leaf(stmt, &mut inner)?;
                for line in inner.split_inclusive('\n') {
                    out.push_str(&pad);
                    out.push_str(line);
                }
            }
        }
        Ok(())
    }

    // ---- statements ----

    fn render_seq(&mut self, stmts: &[SStmt], out: &mut String, indent: usize) -> anyhow::Result<()> {
        for stmt in stmts {
            self.render_stmt_indented(stmt, out, indent)?;
            if self.stmt_terminates(stmt) {
                // Elide the rest of the sequence: it is unreachable per
                // Wasm semantics, and `javac` would reject it as an
                // unreachable statement.
                return Ok(());
            }
        }
        Ok(())
    }

    /// Can execution pass this statement (per Wasm semantics, with the
    /// Java-conservative loop rule)?
    fn stmt_terminates(&self, stmt: &SStmt) -> bool {
        match stmt {
            SStmt::Assign { .. } | SStmt::Effect { .. } => false,
            SStmt::Break { .. }
            | SStmt::Continue { .. }
            | SStmt::Return { .. }
            | SStmt::TailCall { .. }
            | SStmt::TailCallRef { .. }
            | SStmt::Unreachable => true,
            SStmt::Block { label, body } => {
                !self.broken_to.contains(label) && self.seq_terminates(body)
            }
            // javac treats `while (W.T)` as able to complete (non-constant
            // condition), so never elide after a loop.
            SStmt::Loop { .. } => false,
            SStmt::If {
                label,
                if_true,
                if_false,
                ..
            } => {
                !self.broken_to.contains(label)
                    && !if_false.is_empty()
                    && self.seq_terminates(if_true)
                    && self.seq_terminates(if_false)
            }
        }
    }

    fn seq_terminates(&self, stmts: &[SStmt]) -> bool {
        stmts.iter().any(|s| self.stmt_terminates(s))
    }

    /// Render a leaf (non-structured) statement. `#[inline(never)]` keeps
    /// its working set out of the recursive structured frames.
    #[inline(never)]
    fn render_leaf(&mut self, stmt: &SStmt, out: &mut String) -> anyhow::Result<()> {
        let pad = "";
        match stmt {
            SStmt::Assign { local, expr } => {
                let ty = self.locals[*local as usize];
                let e = self.coerce(expr, ty)?;
                let _ = writeln!(out, "{pad}{} = {};", self.local_name(*local), e);
            }
            SStmt::Effect { expr } => {
                if let SExpr::Op { op, .. } = expr {
                    match op {
                        Operator::Nop => return Ok(()),
                        Operator::Call { .. }
                        | Operator::CallRef { .. }
                        | Operator::StructSet { .. }
                        | Operator::ArraySet { .. }
                        | Operator::ArrayCopy { .. } => {
                            let e = self.expr(expr)?;
                            let _ = writeln!(out, "{pad}{e};");
                            return Ok(());
                        }
                        _ => {}
                    }
                }
                // Dropped pure value: write to a shared sink (Java rejects
                // non-statement expressions).
                let (e, jty) = self.expr_typed(expr)?;
                match jty {
                    JTy::Ref => {
                        let _ = writeln!(out, "{pad}W.devnullObj = {e};");
                    }
                    JTy::Boolean => {
                        let _ = writeln!(out, "{pad}W.devnullObj = (Object)({e} ? 1 : 0);");
                    }
                    _ => {
                        let _ = writeln!(out, "{pad}W.devnullObj = (Object)({e});");
                    }
                }
            }
            SStmt::Block { .. } | SStmt::Loop { .. } | SStmt::If { .. } => {
                unreachable!("structured statements are rendered by render_stmt_indented")
            }
            SStmt::Break { label } => {
                let _ = writeln!(out, "{pad}break {};", names::label_name(*label));
            }
            SStmt::Continue { label } => {
                let _ = writeln!(out, "{pad}continue {};", names::label_name(*label));
            }
            SStmt::Return { value } => match (value, self.needs_step) {
                (Some(v), false) => {
                    let e = self.coerce(v, self.rets[0])?;
                    let _ = writeln!(out, "{pad}return {e};");
                }
                (None, false) => {
                    let _ = writeln!(out, "{pad}return;");
                }
                (Some(v), true) => {
                    let e = self.coerce(v, self.rets[0])?;
                    let _ = writeln!(out, "{pad}return W.Step.value({e});");
                }
                (None, true) => {
                    let _ = writeln!(out, "{pad}return W.Step.value(null);");
                }
            },
            SStmt::TailCall { func, args } => {
                if *func == self.current {
                    // Self tail call: assign arguments to the parameter
                    // locals (through temps, since the sources may read
                    // them) and loop.
                    let site = self.temp_counter;
                    self.temp_counter += 1;
                    let params = self.sig_params_of_func(*func)?;
                    let mut exprs = Vec::with_capacity(params.len());
                    for (i, &ty) in params.iter().enumerate() {
                        exprs.push((ty, self.coerce(&args[i], ty)?));
                    }
                    for (i, (ty, e)) in exprs.iter().enumerate() {
                        let jty = java_ty(self.module, *ty);
                        let _ = writeln!(out, "{pad}{jty} s{site}_{i} = {e};");
                    }
                    for i in 0..exprs.len() {
                        let _ = writeln!(out, "{pad}{} = s{site}_{i};", self.local_name(i as u32));
                    }
                    let _ = writeln!(out, "{pad}continue selfTail;");
                } else if self.needs_step {
                    // Cross-function tail call: hand a thunk to the
                    // trampoline loop in this function's public wrapper.
                    // Lambdas capture only effectively-final locals, so
                    // evaluate the arguments into fresh final temps first.
                    let site = self.temp_counter;
                    self.temp_counter += 1;
                    let params = self.sig_params_of_func(*func)?;
                    let mut names_out = Vec::with_capacity(params.len());
                    for (i, &ty) in params.iter().enumerate() {
                        let e = self.coerce(&args[i], ty)?;
                        let jty = java_ty(self.module, ty);
                        let _ = writeln!(out, "{pad}final {jty} t{site}_{i} = {e};");
                        names_out.push(format!("t{site}_{i}"));
                    }
                    let args = names_out.join(", ");
                    let _ = writeln!(
                        out,
                        "{pad}return W.Step.tail(() -> {}({args}));",
                        names::step_name(func.index())
                    );
                } else {
                    bail!(
                        "cross-function tail call in a non-protocol function \
                         (tail-callable set computation is incomplete)"
                    );
                }
            }
            SStmt::TailCallRef { sig, args } => {
                let (params, _) = self.sig_parts(*sig)?;
                let funcref = self.expr(args.last().unwrap())?;
                let iface = iface_name(sig.index());
                if !self.needs_step {
                    bail!(
                        "tail-call-ref in a non-protocol function \
                         (tail-callable set computation is incomplete)"
                    );
                }
                // Lambdas capture only effectively-final locals, so
                // evaluate the arguments into fresh final temps first.
                let site = self.temp_counter;
                self.temp_counter += 1;
                let mut names_out = Vec::with_capacity(params.len());
                for (i, &ty) in params.iter().enumerate() {
                    let e = self.coerce(&args[i], ty)?;
                    let jty = java_ty(self.module, ty);
                    let _ = writeln!(out, "{pad}final {jty} t{site}_{i} = {e};");
                    names_out.push(format!("t{site}_{i}"));
                }
                let _ = writeln!(out, "{pad}final Object t{site}_fn = {funcref};");
                let rendered = names_out.join(", ");
                let _ = writeln!(
                    out,
                    "{pad}return (({iface})(t{site}_fn)).apply$step({rendered});"
                );
            }
            SStmt::Unreachable => {
                let _ = writeln!(out, "{pad}throw new W.WasmTrap(\"unreachable\");");
            }
        }
        Ok(())
    }

    // ---- conditions ----

    fn cond_expr(&self, e: &SExpr) -> anyhow::Result<String> {
        let (s, jty) = self.expr_typed(e)?;
        Ok(match jty {
            JTy::Boolean => s,
            _ => format!("({s} != 0)"),
        })
    }

    fn arg_exprs(&self, args: &[SExpr]) -> anyhow::Result<String> {
        Ok(args
            .iter()
            .map(|a| self.expr(a))
            .collect::<anyhow::Result<Vec<_>>>()?
            .join(", "))
    }

    // ---- expressions ----

    /// Render an expression that Wasm types as `target`, materializing
    /// Java booleans as `i32` where needed.
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
                Ok((self.local_name(*local), jty))
            }
            SExpr::Op { op, args, ty } => self.op_expr(op, args, *ty),
        }
    }

    fn op_expr(&self, op: &Operator, args: &[SExpr], _ty: Option<Type>) -> anyhow::Result<(String, JTy)> {
        use Operator::*;
        let a = |i: usize, ty: Type| self.coerce(&args[i], ty);
        let any = |i: usize| self.expr(&args[i]);
        Ok(match op {
            // ---- constants ----
            I32Const { value } => (i32_lit(*value as i32), JTy::Int),
            I64Const { value } => (i64_lit(*value as i64), JTy::Long),
            F32Const { value } => (format!("Float.intBitsToFloat({})", i32_lit(*value as i32)), JTy::Float),
            F64Const { value } => (format!("Double.longBitsToDouble({})", i64_lit(*value as i64)), JTy::Double),
            // ---- i32 ----
            I32Eqz => (format!("({} == 0)", a(0, Type::I32)?), JTy::Boolean),
            I32Eq => (format!("({} == {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32Ne => (format!("({} != {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32LtS => (format!("({} < {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32GtS => (format!("({} > {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32LeS => (format!("({} <= {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32GeS => (format!("({} >= {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32LtU => (format!("(Integer.compareUnsigned({}, {}) < 0)", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32GtU => (format!("(Integer.compareUnsigned({}, {}) > 0)", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32LeU => (format!("(Integer.compareUnsigned({}, {}) <= 0)", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32GeU => (format!("(Integer.compareUnsigned({}, {}) >= 0)", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Boolean),
            I32Clz => (format!("Integer.numberOfLeadingZeros({})", a(0, Type::I32)?), JTy::Int),
            I32Ctz => (format!("Integer.numberOfTrailingZeros({})", a(0, Type::I32)?), JTy::Int),
            I32Popcnt => (format!("Integer.bitCount({})", a(0, Type::I32)?), JTy::Int),
            I32Add => (format!("({} + {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Sub => (format!("({} - {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Mul => (format!("({} * {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32DivS => (format!("W.divS({}, {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32DivU => (format!("Integer.divideUnsigned({}, {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32RemS => (format!("W.remS({}, {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32RemU => (format!("Integer.remainderUnsigned({}, {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32And => (format!("({} & {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Or => (format!("({} | {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Xor => (format!("({} ^ {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Shl => (format!("({} << {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32ShrS => (format!("({} >> {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32ShrU => (format!("({} >>> {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Rotl => (format!("Integer.rotateLeft({}, {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            I32Rotr => (format!("Integer.rotateRight({}, {})", a(0, Type::I32)?, a(1, Type::I32)?), JTy::Int),
            // ---- i64 ----
            I64Eqz => (format!("({} == 0L)", a(0, Type::I64)?), JTy::Boolean),
            I64Eq => (format!("({} == {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64Ne => (format!("({} != {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64LtS => (format!("({} < {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64GtS => (format!("({} > {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64LeS => (format!("({} <= {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64GeS => (format!("({} >= {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64LtU => (format!("(Long.compareUnsigned({}, {}) < 0)", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64GtU => (format!("(Long.compareUnsigned({}, {}) > 0)", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64LeU => (format!("(Long.compareUnsigned({}, {}) <= 0)", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64GeU => (format!("(Long.compareUnsigned({}, {}) >= 0)", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Boolean),
            I64Clz => (format!("Long.numberOfLeadingZeros({})", a(0, Type::I64)?), JTy::Int),
            I64Ctz => (format!("Long.numberOfTrailingZeros({})", a(0, Type::I64)?), JTy::Int),
            I64Popcnt => (format!("Long.bitCount({})", a(0, Type::I64)?), JTy::Int),
            I64Add => (format!("({} + {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Sub => (format!("({} - {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Mul => (format!("({} * {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64DivS => (format!("W.divS64({}, {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64DivU => (format!("Long.divideUnsigned({}, {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64RemS => (format!("W.remS64({}, {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64RemU => (format!("Long.remainderUnsigned({}, {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64And => (format!("({} & {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Or => (format!("({} | {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Xor => (format!("({} ^ {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Shl => (format!("({} << {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64ShrS => (format!("({} >> {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64ShrU => (format!("({} >>> {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Rotl => (format!("Long.rotateLeft({}, {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            I64Rotr => (format!("Long.rotateRight({}, {})", a(0, Type::I64)?, a(1, Type::I64)?), JTy::Long),
            // ---- f32 ----
            F32Eq => (format!("({} == {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Boolean),
            F32Ne => (format!("({} != {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Boolean),
            F32Lt => (format!("({} < {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Boolean),
            F32Gt => (format!("({} > {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Boolean),
            F32Le => (format!("({} <= {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Boolean),
            F32Ge => (format!("({} >= {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Boolean),
            F32Abs => (format!("Math.abs({})", a(0, Type::F32)?), JTy::Float),
            F32Neg => (format!("(-{})", a(0, Type::F32)?), JTy::Float),
            F32Ceil => (format!("(float)Math.ceil((double){})", a(0, Type::F32)?), JTy::Float),
            F32Floor => (format!("(float)Math.floor((double){})", a(0, Type::F32)?), JTy::Float),
            F32Trunc => (format!("W.truncF({})", a(0, Type::F32)?), JTy::Float),
            F32Nearest => (format!("(float)Math.rint((double){})", a(0, Type::F32)?), JTy::Float),
            F32Sqrt => (format!("(float)Math.sqrt((double){})", a(0, Type::F32)?), JTy::Float),
            F32Add => (format!("({} + {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            F32Sub => (format!("({} - {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            F32Mul => (format!("({} * {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            F32Div => (format!("({} / {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            F32Min => (format!("W.f32min({}, {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            F32Max => (format!("W.f32max({}, {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            F32Copysign => (format!("Math.copySign({}, {})", a(0, Type::F32)?, a(1, Type::F32)?), JTy::Float),
            // ---- f64 ----
            F64Eq => (format!("({} == {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Boolean),
            F64Ne => (format!("({} != {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Boolean),
            F64Lt => (format!("({} < {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Boolean),
            F64Gt => (format!("({} > {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Boolean),
            F64Le => (format!("({} <= {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Boolean),
            F64Ge => (format!("({} >= {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Boolean),
            F64Abs => (format!("Math.abs({})", a(0, Type::F64)?), JTy::Double),
            F64Neg => (format!("(-{})", a(0, Type::F64)?), JTy::Double),
            F64Ceil => (format!("Math.ceil({})", a(0, Type::F64)?), JTy::Double),
            F64Floor => (format!("Math.floor({})", a(0, Type::F64)?), JTy::Double),
            F64Trunc => (format!("W.trunc({})", a(0, Type::F64)?), JTy::Double),
            F64Nearest => (format!("Math.rint({})", a(0, Type::F64)?), JTy::Double),
            F64Sqrt => (format!("Math.sqrt({})", a(0, Type::F64)?), JTy::Double),
            F64Add => (format!("({} + {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            F64Sub => (format!("({} - {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            F64Mul => (format!("({} * {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            F64Div => (format!("({} / {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            F64Min => (format!("W.f64min({}, {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            F64Max => (format!("W.f64max({}, {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            F64Copysign => (format!("Math.copySign({}, {})", a(0, Type::F64)?, a(1, Type::F64)?), JTy::Double),
            // ---- conversions ----
            I32WrapI64 => (format!("(int)({})", a(0, Type::I64)?), JTy::Int),
            I32TruncF32S => (format!("W.truncF32I32({})", a(0, Type::F32)?), JTy::Int),
            I32TruncF32U => (format!("W.truncF32U32({})", a(0, Type::F32)?), JTy::Int),
            I32TruncF64S => (format!("W.truncF64I32({})", a(0, Type::F64)?), JTy::Int),
            I32TruncF64U => (format!("W.truncF64U32({})", a(0, Type::F64)?), JTy::Int),
            I64ExtendI32S => (format!("(long)({})", a(0, Type::I32)?), JTy::Long),
            I64ExtendI32U => (format!("(({}) & 0xffffffffL)", a(0, Type::I32)?), JTy::Long),
            I64TruncF32S => (format!("W.truncF32I64({})", a(0, Type::F32)?), JTy::Long),
            I64TruncF32U => (format!("W.truncF32U64({})", a(0, Type::F32)?), JTy::Long),
            I64TruncF64S => (format!("W.truncF64I64({})", a(0, Type::F64)?), JTy::Long),
            I64TruncF64U => (format!("W.truncF64U64({})", a(0, Type::F64)?), JTy::Long),
            F32ConvertI32S => (format!("(float)({})", a(0, Type::I32)?), JTy::Float),
            F32ConvertI32U => (format!("(float)(({}) & 0xffffffffL)", a(0, Type::I32)?), JTy::Float),
            F32ConvertI64S => (format!("(float)({})", a(0, Type::I64)?), JTy::Float),
            F32ConvertI64U => (format!("W.u64ToF32({})", a(0, Type::I64)?), JTy::Float),
            F32DemoteF64 => (format!("(float)({})", a(0, Type::F64)?), JTy::Float),
            F64ConvertI32S => (format!("(double)({})", a(0, Type::I32)?), JTy::Double),
            F64ConvertI32U => (format!("(double)(({}) & 0xffffffffL)", a(0, Type::I32)?), JTy::Double),
            F64ConvertI64S => (format!("(double)({})", a(0, Type::I64)?), JTy::Double),
            F64ConvertI64U => (format!("W.u64ToF64({})", a(0, Type::I64)?), JTy::Double),
            F64PromoteF32 => (format!("(double)({})", a(0, Type::F32)?), JTy::Double),
            I32Extend8S => (format!("(int)(byte)({})", a(0, Type::I32)?), JTy::Int),
            I32Extend16S => (format!("(int)(short)({})", a(0, Type::I32)?), JTy::Int),
            I64Extend8S => (format!("(long)(byte)({})", a(0, Type::I64)?), JTy::Long),
            I64Extend16S => (format!("(long)(short)({})", a(0, Type::I64)?), JTy::Long),
            I64Extend32S => (format!("(long)(int)({})", a(0, Type::I64)?), JTy::Long),
            I32TruncSatF32S | I32TruncSatF64S => (format!("(int)({})", any(0)?), JTy::Int),
            I32TruncSatF32U | I32TruncSatF64U => (format!("W.truncSatU32({})", any(0)?), JTy::Int),
            I64TruncSatF32S | I64TruncSatF64S => (format!("(long)({})", any(0)?), JTy::Long),
            I64TruncSatF32U | I64TruncSatF64U => (format!("W.truncSatU64({})", any(0)?), JTy::Long),
            F32ReinterpretI32 => (format!("Float.intBitsToFloat({})", a(0, Type::I32)?), JTy::Float),
            I32ReinterpretF32 => (format!("Float.floatToRawIntBits({})", a(0, Type::F32)?), JTy::Int),
            F64ReinterpretI64 => (format!("Double.longBitsToDouble({})", a(0, Type::I64)?), JTy::Double),
            I64ReinterpretF64 => (format!("Double.doubleToRawLongBits({})", a(0, Type::F64)?), JTy::Long),
            // ---- select ----
            TypedSelect { ty } => {
                let cond = self.cond_expr(&args[2])?;
                let t = self.coerce(&args[0], *ty)?;
                let f = self.coerce(&args[1], *ty)?;
                let inner = format!("({cond} ? {t} : {f})");
                // Java's ternary needs a common type; pin ref-typed
                // selects to their declared type.
                let rendered = match ty {
                    Type::Heap(h) if !matches!(h.value, HeapType::Any | HeapType::Eq | HeapType::I31 | HeapType::None) => {
                        format!("({})((Object)({inner}))", java_ty(self.module, *ty))
                    }
                    _ => inner,
                };
                let jty = match ty {
                    Type::I32 => JTy::Int,
                    Type::I64 => JTy::Long,
                    Type::F32 => JTy::Float,
                    Type::F64 => JTy::Double,
                    _ => JTy::Ref,
                };
                (rendered, jty)
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
                (format!("{}({rendered})", names::func_name(function_index.index())), jty)
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
                    format!("(({})((Object)({funcref}))).apply({rendered})", iface_name(sig_index.index())),
                    jty,
                )
            }
            // ---- structs ----
            StructNew { sig: sig_index } => {
                let fields = self.struct_field_tys(*sig_index)?;
                let name = names::struct_name(sig_index.index());
                if fields.len() <= 255 {
                    let rendered = fields
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| self.coerce(&args[i], *ty))
                        .collect::<anyhow::Result<Vec<_>>>()?
                        .join(", ");
                    (format!("new {name}({rendered})"), JTy::Ref)
                } else {
                    // Java limits methods to 255 parameter slots: populate
                    // oversized structs field-by-field.
                    let mut init = String::new();
                    for (i, ty) in fields.iter().enumerate() {
                        let _ = write!(init, " s.f{i} = {};", self.coerce(&args[i], *ty)?);
                    }
                    (format!("W.build(new {name}(), (final {name} s) -> {{{init} }})"), JTy::Ref)
                }
            }
            StructNewDefault { sig: sig_index } => (
                format!("new {}()", names::struct_name(sig_index.index())),
                JTy::Ref,
            ),
            StructGet { sig: sig_index, idx: field_index } => {
                let fields = self.struct_field_tys(*sig_index)?;
                let jty = fields
                    .get(*field_index as usize)
                    .map(|t| jty_of(*t))
                    .unwrap_or(JTy::Ref);
                (
                    format!(
                        "(({})((Object)({}))).f{field_index}",
                        names::struct_name(sig_index.index()),
                        any(0)?
                    ),
                    jty,
                )
            }
            StructSet { sig: sig_index, idx: field_index } => {
                let fields = self.struct_field_tys(*sig_index)?;
                let fty = fields[*field_index as usize];
                (
                    format!(
                        "(({})((Object)({}))).f{field_index} = {}",
                        names::struct_name(sig_index.index()),
                        any(0)?,
                        self.coerce(&args[1], fty)?
                    ),
                    JTy::Ref,
                )
            }
            StructGetS { .. } | StructGetU { .. } => {
                bail!("packed struct fields are outside the mobile closure")
            }
            // ---- arrays ----
            ArrayNewFixed { sig: array_type_index, .. } => {
                let elem = self.array_elem(*array_type_index)?;
                let aty = array_ty(self.module, elem);
                let rendered = (0..args.len())
                    .map(|i| self.array_elem_expr(elem, i, args))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .join(", ");
                (format!("new {aty}{{{rendered}}}"), JTy::Ref)
            }
            ArrayNewDefault { sig: array_type_index } => {
                let elem = self.array_elem(*array_type_index)?;
                let aty = array_ty(self.module, elem);
                (
                    format!("new {}[{}]", &aty[..aty.len() - 2], a(0, Type::I32)?),
                    JTy::Ref,
                )
            }
            ArrayNew { sig: array_type_index } => {
                let elem = self.array_elem(*array_type_index)?;
                let aty = array_ty(self.module, elem);
                let base = &aty[..aty.len() - 2];
                let init = match elem {
                    StorageType::I8 => format!("(byte)({})", a(0, Type::I32)?),
                    StorageType::I16 => format!("(char)({})", a(0, Type::I32)?),
                    StorageType::Val(ty) => self.coerce(&args[0], ty)?,
                    _ => bail!("array storage outside the mobile closure"),
                };
                (
                    format!("W.fill(new {base}[{}], {init})", a(1, Type::I32)?),
                    JTy::Ref,
                )
            }
            ArrayGet { sig: array_type_index } | ArrayGetS { sig: array_type_index } | ArrayGetU { sig: array_type_index } => {
                let elem = self.array_elem(*array_type_index)?;
                let aty = array_ty(self.module, elem);
                let arr = any(0)?;
                let idx = a(1, Type::I32)?;
                let is_unsigned = matches!(op, ArrayGetU { .. });
                let is_signed = matches!(op, ArrayGetS { .. });
                let get = format!("((({aty})((Object)({arr})))[{idx}])");
                let rendered = match elem {
                    StorageType::I8 if is_unsigned => format!("({get} & 0xff)"),
                    StorageType::I16 if is_signed => format!("(short){get}"),
                    _ => get,
                };
                let jty = match elem {
                    StorageType::I8 | StorageType::I16 => JTy::Int,
                    StorageType::Val(ty) => jty_of(ty),
                    _ => JTy::Ref,
                };
                (rendered, jty)
            }
            ArraySet { sig: array_type_index } => {
                let elem = self.array_elem(*array_type_index)?;
                let arr = any(0)?;
                let idx = a(1, Type::I32)?;
                let aty = array_ty(self.module, elem);
                let val = match elem {
                    StorageType::I8 => format!("(byte)({})", a(2, Type::I32)?),
                    StorageType::I16 => format!("(char)({})", a(2, Type::I32)?),
                    StorageType::Val(ty) => self.coerce(&args[2], ty)?,
                    _ => bail!("array storage outside the mobile closure"),
                };
                (format!("(({aty})((Object)({arr})))[{idx}] = {val}"), JTy::Ref)
            }
            ArrayLen => (format!("(({})).length", any(0)?), JTy::Int),
            ArrayCopy { .. } => (
                format!(
                    "System.arraycopy({}, {}, {}, {}, {})",
                    any(2)?,
                    a(3, Type::I32)?,
                    any(0)?,
                    a(1, Type::I32)?,
                    a(4, Type::I32)?
                ),
                JTy::Ref,
            ),
            // ---- references ----
            RefNull { .. } => ("null".to_string(), JTy::Ref),
            RefIsNull => (format!("({} == null)", any(0)?), JTy::Boolean),
            RefFunc { func_index } => {
                // Every RefFunc target is in the tail-callable set (by
                // construction), hence a protocol member: the funcref
                // overrides `apply$step` to enter the target's trampoline
                // body directly, so `return_call_ref` chains flow through
                // Steps consumed by the outermost loop — O(1) stack rather
                // than trampolines nested per hop.
                let (params, rets) = self.sig_parts(self.sig_of_func(*func_index)?)?;
                let sig = self.sig_of_func(*func_index)?;
                let iface = iface_name(sig.index());
                let fname = names::func_name(func_index.index());
                let step = names::step_name(func_index.index());
                let decls = params
                    .iter()
                    .enumerate()
                    .map(|(i, &ty)| format!("{} a{i}", java_ty(self.module, ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let pass = (0..params.len())
                    .map(|i| format!("a{i}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let apply_ret = match rets.first() {
                    Some(&ty) => java_ty(self.module, ty),
                    None => "void".to_string(),
                };
                let apply_body = if apply_ret == "void" {
                    format!("Mod.{fname}({pass});")
                } else {
                    format!("return Mod.{fname}({pass});")
                };
                (
                    format!(
                        "(({}) new {iface}() {{ \
                        public {apply_ret} apply({decls}) {{ {apply_body} }} \
                        public W.Step apply$step({decls}) {{ return Mod.{step}({pass}); }} \
                        }})",
                        iface
                    ),
                    JTy::Ref,
                )
            }
            RefTest { ty } => (self.ref_test(*ty, &any(0)?)?, JTy::Boolean),
            RefCast { ty } => (self.ref_cast(*ty, &any(0)?)?, JTy::Ref),
            RefEq => (format!("((Object)({}) == (Object)({}))", any(0)?, any(1)?), JTy::Boolean),
            // The only i31 ever materialized is the JS-null sentinel.
            RefI31 => ("W.JsNull.INSTANCE".to_string(), JTy::Ref),
            I31GetS | I31GetU => {
                bail!("i31 payload reads are outside the mobile closure (sentinel-only)")
            }
            Unreachable => bail!("unreachable has no value"),
            Nop => bail!("nop has no value"),
            other => bail!("operator {other:?} is outside the mobile closure"),
        })
    }

    // ---- helpers ----

    fn sig_parts(&self, sig: portal_pc_waffle::Signature) -> anyhow::Result<(Vec<Type>, Vec<Type>)> {
        match &self.module.signatures[sig] {
            SignatureData::Func { params, returns, .. } => Ok((params.clone(), returns.clone())),
            _ => bail!("signature {sig:?} is not a function signature"),
        }
    }

    fn sig_params_of_func(&self, func: portal_pc_waffle::Func) -> anyhow::Result<Vec<Type>> {
        Ok(self.sig_parts(self.sig_of_func(func)?)?.0)
    }

    fn func_ret_jty(&self, func: portal_pc_waffle::Func) -> anyhow::Result<JTy> {
        let (_, rets) = self.sig_parts(self.sig_of_func(func)?)?;
        Ok(rets.first().map(|t| jty_of(*t)).unwrap_or(JTy::Ref))
    }

    fn sig_of_func(&self, func: portal_pc_waffle::Func) -> anyhow::Result<portal_pc_waffle::Signature> {
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

    fn array_elem_expr(&self, elem: StorageType, i: usize, args: &[SExpr]) -> anyhow::Result<String> {
        match elem {
            StorageType::I8 => Ok(format!("(byte)({})", self.coerce(&args[i], Type::I32)?)),
            StorageType::I16 => Ok(format!("(char)({})", self.coerce(&args[i], Type::I32)?)),
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
                    format!("((Object)({e}) instanceof {})", names::struct_name(sig_index.index()))
                }
                SignatureData::Array { ty, .. } => {
                    format!("((Object)({e}) instanceof {})", array_ty(self.module, ty.value))
                }
                SignatureData::Func { .. } => format!("((Object)({e}) instanceof {})", iface_name(sig_index.index())),
                _ => bail!("ref.test target outside the mobile closure"),
            },
            HeapType::FuncRef => format!("((Object)({e}) instanceof IFun)"),
            HeapType::Any | HeapType::Eq => format!("({e} != null)"),
            HeapType::I31 => format!("((Object)({e}) instanceof W.JsNull)"),
            HeapType::Struct => format!("W.isStruct({e})"),
            HeapType::Array => format!("W.isArray({e})"),
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
                    format!("(({})((Object)({e})))", names::struct_name(sig_index.index()))
                }
                SignatureData::Array { ty, .. } => {
                    format!("(({})((Object)({e})))", array_ty(self.module, ty.value))
                }
                SignatureData::Func { .. } => format!("(({})((Object)({e})))", iface_name(sig_index.index())),
                _ => bail!("ref.cast target outside the mobile closure"),
            },
            HeapType::FuncRef => format!("((IFun)((Object)({e})))"),
            HeapType::Any | HeapType::Eq => e.to_string(),
            HeapType::I31 => format!("((W.JsNull)((Object)({e})))"),
            HeapType::Struct | HeapType::Array => format!("W.castAny({e})"),
            HeapType::None | HeapType::NoFunc => "W.trapExpr(\"cast to bottom type\")".to_string(),
            other => bail!("ref.cast heap type {other:?} is outside the mobile closure"),
        })
    }
}

/// The rendered body of one function plus its trampoline-protocol flags.
pub struct RenderedBody {
    pub needs_step: bool,
    pub has_self_tail: bool,
    pub body_src: String,
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

fn collect_breaks(stmts: &[SStmt], out: &mut BTreeSet<SLabel>) {
    for stmt in stmts {
        match stmt {
            SStmt::Break { label } => {
                out.insert(*label);
            }
            SStmt::Block { body, .. } | SStmt::Loop { body, .. } => collect_breaks(body, out),
            SStmt::If {
                if_true, if_false, ..
            } => {
                collect_breaks(if_true, out);
                collect_breaks(if_false, out);
            }
            _ => {}
        }
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

/// Java int literal (avoids the `-2147483648` literal pitfall).
pub fn i32_lit(v: i32) -> String {
    if v == i32::MIN {
        "(-2147483647 - 1)".to_string()
    } else {
        v.to_string()
    }
}

/// Java long literal (avoids the `-9223372036854775808L` pitfall).
pub fn i64_lit(v: i64) -> String {
    if v == i64::MIN {
        "(-9223372036854775807L - 1L)".to_string()
    } else {
        format!("{v}L")
    }
}
