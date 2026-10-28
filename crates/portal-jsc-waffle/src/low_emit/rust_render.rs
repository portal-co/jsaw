//! The Rust renderer (DreamComp #40): `LeModule` → stable Rust source.
//!
//! See `docs/plan-rust-backend-40.md`. Mirrors the C renderer's structure
//! (`super::c_render`), with Rust-native semantics: `wrapping_*` arithmetic,
//! saturating float→int `as` casts (correct behind the skeleton's trapping
//! guards, and exactly `TruncSat` without them), true f32 arithmetic, and a
//! state-machine `pc = match pc { .. }` rendering of block CFGs (Rust has no
//! `goto`). Function tables render as match-dispatch fns returning `usize`
//! function addresses (fn→usize casts are not const-evaluable, so a static
//! array cannot hold them).

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

use super::rust_runtime::{RUST_PRELUDE, RUST_RUNTIME_COREGC, RUST_RUNTIME_NANO};
use super::{
    LeBinOp, LeBlock, LeCallee, LeDiag, LeExpr, LeFunc, LeLocal, LeModule, LeStmt, LeType, LeUnOp,
};

/// Sanitize an arbitrary name into a Rust identifier (deterministic).
pub fn rust_ident(s: &str) -> String {
    super::c_render::c_ident(s)
}

fn rty(ty: LeType) -> &'static str {
    match ty {
        LeType::I8 => "i8",
        LeType::I16 => "i16",
        LeType::I32 => "i32",
        LeType::I64 => "i64",
        LeType::F32 => "f32",
        LeType::F64 => "f64",
        LeType::Ref => "JsRef",
        LeType::Slot => "u64",
    }
}

fn unop_result(op: LeUnOp) -> LeType {
    match op {
        LeUnOp::I32Eqz
        | LeUnOp::I32WrapI64
        | LeUnOp::I32Extend8S
        | LeUnOp::I32Extend8U
        | LeUnOp::I32Extend16S
        | LeUnOp::I32Extend16U
        | LeUnOp::I32TruncF64S
        | LeUnOp::I32TruncF64U
        | LeUnOp::I32TruncSatF64S
        | LeUnOp::I32TruncSatF64U
        | LeUnOp::I32ReinterpretF32 => LeType::I32,
        LeUnOp::I64ExtendI32S
        | LeUnOp::I64ExtendI32U
        | LeUnOp::I64TruncF64S
        | LeUnOp::I64TruncF64U
        | LeUnOp::I64TruncSatF64S
        | LeUnOp::I64TruncSatF64U
        | LeUnOp::I64ReinterpretF64 => LeType::I64,
        LeUnOp::F32DemoteF64 | LeUnOp::F32Abs | LeUnOp::F32ReinterpretI32 => LeType::F32,
        LeUnOp::F64PromoteF32
        | LeUnOp::F64Abs
        | LeUnOp::F64Ceil
        | LeUnOp::F64Floor
        | LeUnOp::F64Sqrt
        | LeUnOp::F64Trunc
        | LeUnOp::F64Nearest
        | LeUnOp::F64Neg
        | LeUnOp::F64ConvertI32S
        | LeUnOp::F64ConvertI32U
        | LeUnOp::F64ConvertI64S
        | LeUnOp::F64ConvertI64U
        | LeUnOp::F64ReinterpretI64 => LeType::F64,
    }
}

fn binop_result(op: LeBinOp) -> LeType {
    use LeBinOp::*;
    match op {
        I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
        | I32Xor | I32Shl | I32ShrS | I32ShrU => LeType::I32,
        I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
        | I64Xor | I64Shl | I64ShrS | I64ShrU => LeType::I64,
        F32Add | F32Sub | F32Mul | F32Div => LeType::F32,
        F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max => LeType::F64,
        I32Eq | I32Ne | I32LtS | I32LtU | I32LeS | I32LeU | I32GtS | I32GtU | I32GeS | I32GeU
        | I64Eq | I64Ne | I64LtS | I64LtU | I64LeS | I64LeU | I64GtS | I64GtU | I64GeS | I64GeU
        | F32Eq | F32Ne | F32Lt | F32Le | F32Gt | F32Ge | F64Eq | F64Ne | F64Lt | F64Le | F64Gt
        | F64Ge => LeType::I32,
    }
}

struct Renderer<'m> {
    m: &'m LeModule,
    import_syms: BTreeMap<u32, String>,
    /// Defined/import func id → (params, results), for call-arg coercion.
    fn_sigs: BTreeMap<u32, (Vec<LeType>, Vec<LeType>)>,
}

impl<'m> Renderer<'m> {
    /// Parameter types of a runtime entry point (the fixed per-lane runtime
    /// contracts; see `rust_runtime.rs`).
    fn runtime_params(name: &str) -> Option<Vec<LeType>> {
        use LeType::*;
        Some(match name {
            "js_nano_arena_ensure" => vec![I64],
            "js_coregc_alloc" => vec![I32, I32],
            "js_coregc_root_enter" => vec![I32],
            "js_coregc_root_store" => vec![I32, Ref],
            "js_coregc_root_pop" => vec![],
            "js_coregc_array_copy" => vec![Ref, I32, Ref, I32, I32, I32],
            _ => return None,
        })
    }
}

impl<'m> Renderer<'m> {
    fn diag(&self, message: String) -> LeDiag {
        LeDiag {
            function: None,
            block: None,
            inst: None,
            message,
        }
    }

    /// Coerce a rendered expression to the expected type at an assignment /
    /// call / store / return boundary. Only word reinterpretations are
    /// allowed (Ref ↔ I64/Slot); anything else indicates a skeleton typing
    /// bug and is left for rustc to reject.
    fn coerce(&self, expected: LeType, actual: LeType, rendered: String) -> String {
        if expected == actual {
            return rendered;
        }
        // Integer-ish pairs reinterpret/truncate/sign-extend via `as`
        // (word semantics); float boundaries never occur in well-formed IR
        // and are left for rustc to reject as skeleton bugs.
        let intish = |t: LeType| {
            matches!(
                t,
                LeType::I8 | LeType::I16 | LeType::I32 | LeType::I64 | LeType::Slot | LeType::Ref
            )
        };
        if intish(expected) && intish(actual) {
            return format!("(({rendered}) as {})", rty(expected));
        }
        rendered
    }

    /// Input type of a unop (for operand coercion).
    fn unop_input(op: LeUnOp) -> LeType {
        use LeUnOp::*;
        match op {
            I32Eqz | I32WrapI64 => LeType::I64, // wrap reads i64; eqz reads i32 (see below)
            I64ExtendI32S | I64ExtendI32U | I32Extend8S | I32Extend8U | I32Extend16S
            | I32Extend16U => LeType::I32,
            I32TruncF64S | I32TruncF64U | I64TruncF64S | I64TruncF64U | I32TruncSatF64S
            | I32TruncSatF64U | I64TruncSatF64S | I64TruncSatF64U | F32DemoteF64 | F64Abs
            | F64Ceil | F64Floor | F64Sqrt | F64Trunc | F64Nearest | F64Neg | I64ReinterpretF64 => {
                LeType::F64
            }
            F64PromoteF32 | F32Abs | I32ReinterpretF32 => LeType::F32,
            F64ConvertI32S | F64ConvertI32U | F32ReinterpretI32 => LeType::I32,
            F64ConvertI64S | F64ConvertI64U | F64ReinterpretI64 => LeType::I64,
        }
    }

    /// Input type of a binop's operands (for operand coercion).
    fn binop_input(op: LeBinOp) -> LeType {
        use LeBinOp::*;
        match op {
            I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
            | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Eq | I32Ne | I32LtS | I32LtU | I32LeS
            | I32LeU | I32GtS | I32GtU | I32GeS | I32GeU => LeType::I32,
            I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
            | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Eq | I64Ne | I64LtS | I64LtU | I64LeS
            | I64LeU | I64GtS | I64GtU | I64GeS | I64GeU => LeType::I64,
            F32Add | F32Sub | F32Mul | F32Div | F32Eq | F32Ne | F32Lt | F32Le | F32Gt | F32Ge => {
                LeType::F32
            }
            F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Eq | F64Ne | F64Lt | F64Le
            | F64Gt | F64Ge => LeType::F64,
        }
    }

    fn expr_ty(&self, f: &LeFunc, e: &LeExpr) -> LeType {
        match e {
            LeExpr::Local(l) => f.locals[l.0 as usize].1,
            LeExpr::ConstI32(_) => LeType::I32,
            LeExpr::ConstI64(_) => LeType::I64,
            LeExpr::ConstF32(_) => LeType::F32,
            LeExpr::ConstF64(_) => LeType::F64,
            LeExpr::Unary(op, _) => unop_result(*op),
            LeExpr::Binary(op, _, _) => binop_result(*op),
            LeExpr::Select(_, t, _) => self.expr_ty(f, t),
            LeExpr::GlobalGet(g) => self.m.globals[*g as usize].0,
            LeExpr::TableElem { .. } => LeType::Ref,
            LeExpr::ArenaLoad { ty, .. } => *ty,
            LeExpr::Field { layout, field, .. } => {
                self.m.layouts[*layout as usize].fields[*field as usize]
            }
            LeExpr::Index { layout, .. } => self.m.layouts[*layout as usize]
                .array_element
                .expect("index into array layout"),
            LeExpr::ToSlot(..) => LeType::Slot,
            LeExpr::FromSlot(ty, _) => *ty,
        }
    }

    fn fn_name(&self, id: u32) -> String {
        match self.import_syms.get(&id) {
            Some(sym) => sym.clone(),
            None => format!("js_f{id}"),
        }
    }

    fn unop(&self, op: LeUnOp, x: &str) -> String {
        match op {
            LeUnOp::I32Eqz => format!("((({x}) == 0) as i32)"),
            LeUnOp::I32WrapI64 => format!("(({x}) as i32)"),
            LeUnOp::I64ExtendI32S => format!("(({x}) as i64)"),
            LeUnOp::I64ExtendI32U => format!("((({x}) as u32) as i64)"),
            LeUnOp::I32Extend8S => format!("((({x}) as i8) as i32)"),
            LeUnOp::I32Extend8U => format!("((({x}) as u8) as i32)"),
            LeUnOp::I32Extend16S => format!("((({x}) as i16) as i32)"),
            LeUnOp::I32Extend16U => format!("((({x}) as u16) as i32)"),
            // Trapping conversions: in-range by construction (skeleton
            // guard); saturating TruncSat: Rust `as` saturates natively.
            LeUnOp::I32TruncF64S | LeUnOp::I32TruncSatF64S => format!("(({x}) as i32)"),
            LeUnOp::I32TruncF64U | LeUnOp::I32TruncSatF64U => {
                format!("((({x}) as u32) as i32)")
            }
            LeUnOp::I64TruncF64S | LeUnOp::I64TruncSatF64S => format!("(({x}) as i64)"),
            LeUnOp::I64TruncF64U | LeUnOp::I64TruncSatF64U => {
                format!("((({x}) as u64) as i64)")
            }
            LeUnOp::F32DemoteF64 => format!("(({x}) as f32)"),
            LeUnOp::F64PromoteF32 => format!("(({x}) as f64)"),
            LeUnOp::F32Abs | LeUnOp::F64Abs => format!("({x}).abs()"),
            LeUnOp::F64Ceil => format!("({x}).ceil()"),
            LeUnOp::F64Floor => format!("({x}).floor()"),
            LeUnOp::F64Sqrt => format!("({x}).sqrt()"),
            LeUnOp::F64Trunc => format!("({x}).trunc()"),
            LeUnOp::F64Nearest => format!("({x}).round_ties_even()"),
            LeUnOp::F64Neg => format!("(-({x}))"),
            LeUnOp::F64ConvertI32S => format!("((({x}) as i32) as f64)"),
            LeUnOp::F64ConvertI32U => format!("((({x}) as u32) as f64)"),
            LeUnOp::F64ConvertI64S => format!("(({x}) as f64)"),
            LeUnOp::F64ConvertI64U => format!("((({x}) as u64) as f64)"),
            LeUnOp::F64ReinterpretI64 => format!("f64::from_bits((({x}) as u64))"),
            LeUnOp::I64ReinterpretF64 => format!("((({x}).to_bits()) as i64)"),
            LeUnOp::F32ReinterpretI32 => format!("f32::from_bits((({x}) as u32))"),
            LeUnOp::I32ReinterpretF32 => format!("((({x}).to_bits()) as i32)"),
        }
    }

    fn binop(&self, op: LeBinOp, a: &str, b: &str) -> String {
        use LeBinOp::*;
        match op {
            I32Add | I64Add => format!("({a}).wrapping_add({b})"),
            I32Sub | I64Sub => format!("({a}).wrapping_sub({b})"),
            I32Mul | I64Mul => format!("({a}).wrapping_mul({b})"),
            I32DivS => format!("js_div_s32({a}, {b})"),
            I32RemS => format!("js_rem_s32({a}, {b})"),
            I32DivU => format!("js_div_u32((({a}) as u32), (({b}) as u32))"),
            I32RemU => format!("js_rem_u32((({a}) as u32), (({b}) as u32))"),
            I64DivS => format!("js_div_s64({a}, {b})"),
            I64RemS => format!("js_rem_s64({a}, {b})"),
            I64DivU => format!("js_div_u64((({a}) as u64), (({b}) as u64))"),
            I64RemU => format!("js_rem_u64((({a}) as u64), (({b}) as u64))"),
            I32And | I64And => format!("(({a}) & ({b}))"),
            I32Or | I64Or => format!("(({a}) | ({b}))"),
            I32Xor | I64Xor => format!("(({a}) ^ ({b}))"),
            I32Shl | I64Shl => format!("({a}).wrapping_shl((({b}) as u32))"),
            I32ShrS | I64ShrS => format!("({a}).wrapping_shr((({b}) as u32))"),
            I32ShrU => format!("(((({a}) as u32).wrapping_shr((({b}) as u32))) as i32)"),
            I64ShrU => format!("(((({a}) as u64).wrapping_shr((({b}) as u32))) as i64)"),
            I32Eq | I64Eq => format!("((({a}) == ({b})) as i32)"),
            I32Ne | I64Ne => format!("((({a}) != ({b})) as i32)"),
            I32LtS | I64LtS => format!("((({a}) < ({b})) as i32)"),
            I32LtU => format!("(((( {a}) as u32) < (({b}) as u32)) as i32)"),
            I64LtU => format!("(((( {a}) as u64) < (({b}) as u64)) as i32)"),
            I32LeS | I64LeS => format!("((({a}) <= ({b})) as i32)"),
            I32LeU => format!("(((( {a}) as u32) <= (({b}) as u32)) as i32)"),
            I64LeU => format!("(((( {a}) as u64) <= (({b}) as u64)) as i32)"),
            I32GtS | I64GtS => format!("((({a}) > ({b})) as i32)"),
            I32GtU => format!("(((( {a}) as u32) > (({b}) as u32)) as i32)"),
            I64GtU => format!("(((( {a}) as u64) > (({b}) as u64)) as i32)"),
            I32GeS | I64GeS => format!("((({a}) >= ({b})) as i32)"),
            I32GeU => format!("(((( {a}) as u32) >= (({b}) as u32)) as i32)"),
            I64GeU => format!("(((( {a}) as u64) >= (({b}) as u64)) as i32)"),
            F32Add | F64Add => format!("(({a}) + ({b}))"),
            F32Sub | F64Sub => format!("(({a}) - ({b}))"),
            F32Mul | F64Mul => format!("(({a}) * ({b}))"),
            F32Div | F64Div => format!("(({a}) / ({b}))"),
            F64Min => format!("js_wmin({a}, {b})"),
            F64Max => format!("js_wmax({a}, {b})"),
            F32Eq | F64Eq => format!("((({a}) == ({b})) as i32)"),
            F32Ne | F64Ne => format!("((({a}) != ({b})) as i32)"),
            F32Lt | F64Lt => format!("((({a}) < ({b})) as i32)"),
            F32Le | F64Le => format!("((({a}) <= ({b})) as i32)"),
            F32Gt | F64Gt => format!("((({a}) > ({b})) as i32)"),
            F32Ge | F64Ge => format!("((({a}) >= ({b})) as i32)"),
        }
    }

    fn field_access(&self, layout: u32, field: u32, base: &str, mutable: bool) -> String {
        const HEADER_FIELDS: [&str; 4] = ["type_id", "flags", "len", "pad"];
        let ptr = if mutable { "*mut" } else { "*const" };
        if layout == 0 {
            format!("(*(({base}) as {ptr} JsHd)).{}", HEADER_FIELDS[field as usize])
        } else {
            format!("(*(({base}) as {ptr} JsL{layout})).f{field}")
        }
    }

    fn index_access(&self, layout: u32, base: &str, index: &str, mutable: bool) -> String {
        let elem = self.m.layouts[layout as usize]
            .array_element
            .expect("array layout");
        let ptr = if mutable { "*mut" } else { "*const" };
        format!(
            "*(((({base}) as {ptr} u8).add(js_data_off::<{elem}>()) as {ptr} {elem}).add(({index}) as usize))",
            elem = rty(elem),
        )
    }

    fn expr(&self, f: &LeFunc, e: &LeExpr) -> String {
        match e {
            LeExpr::Local(l) => format!("v{}", l.0),
            LeExpr::ConstI32(v) => format!("(({v}u32) as i32)"),
            LeExpr::ConstI64(v) => format!("(({v}u64) as i64)"),
            LeExpr::ConstF32(bits) => format!("f32::from_bits(0x{bits:08x}u32)"),
            LeExpr::ConstF64(bits) => format!("f64::from_bits(0x{bits:016x}u64)"),
            LeExpr::Unary(op, x) => {
                // I32Eqz is the odd one out (reads an i32, not an i64).
                let input = if matches!(op, LeUnOp::I32Eqz) {
                    LeType::I32
                } else {
                    Self::unop_input(*op)
                };
                let operand = self.coerce(input, self.expr_ty(f, x), self.expr(f, x));
                self.unop(*op, &operand)
            }
            LeExpr::Binary(op, a, b) => {
                let input = Self::binop_input(*op);
                let ra = self.coerce(input, self.expr_ty(f, a), self.expr(f, a));
                let rb = self.coerce(input, self.expr_ty(f, b), self.expr(f, b));
                self.binop(*op, &ra, &rb)
            }
            LeExpr::Select(c, t, fl) => {
                let c = self.coerce(LeType::I32, self.expr_ty(f, c), self.expr(f, c));
                let ty = self.expr_ty(f, t);
                format!(
                    "(if ({c}) != 0 {{ ({}) }} else {{ ({}) }})",
                    self.coerce(ty, ty, self.expr(f, t)),
                    self.coerce(ty, self.expr_ty(f, fl), self.expr(f, fl))
                )
            }
            LeExpr::GlobalGet(g) => format!("unsafe {{ JS_G{g} }}"),
            LeExpr::TableElem { table, index } => {
                format!("js_t{table}((({i}) as usize))", i = self.expr(f, index))
            }
            LeExpr::ArenaLoad { ty, base, offset } => {
                let suffix = match ty {
                    LeType::I8 => "i8",
                    LeType::I16 => "i16",
                    LeType::I32 => "i32",
                    LeType::I64 | LeType::Slot => "i64",
                    LeType::F32 => "f32",
                    LeType::F64 => "f64",
                    LeType::Ref => "ref",
                };
                format!(
                    "js_ld_{suffix}((({base}) as u64).wrapping_add({offset}u64))",
                    base = self.expr(f, base)
                )
            }
            LeExpr::Field {
                layout,
                field,
                base,
            } => format!("unsafe {{ {} }}", self.field_access(*layout, *field, &self.expr(f, base), false)),
            LeExpr::Index {
                layout,
                base,
                index,
            } => format!(
                "unsafe {{ {} }}",
                self.index_access(*layout, &self.expr(f, base), &self.expr(f, index), false)
            ),
            LeExpr::ToSlot(ty, x) => {
                let x = self.expr(f, x);
                match ty {
                    LeType::I8 => format!("((({x}) as u8) as u64)"),
                    LeType::I16 => format!("((({x}) as u16) as u64)"),
                    LeType::I32 => format!("((({x}) as u32) as u64)"),
                    LeType::I64 | LeType::Slot => format!("(({x}) as u64)"),
                    LeType::F32 => format!("((({x}).to_bits()) as u64)"),
                    LeType::F64 => format!("(({x}).to_bits())"),
                    LeType::Ref => format!("(({x}) as u64)"),
                }
            }
            LeExpr::FromSlot(ty, x) => {
                let x = self.expr(f, x);
                match ty {
                    LeType::I8 => format!("((({x}) as u8) as i8)"),
                    LeType::I16 => format!("((({x}) as u16) as i16)"),
                    LeType::I32 => format!("(({x}) as i32)"),
                    LeType::I64 | LeType::Slot => format!("(({x}) as i64)"),
                    LeType::F32 => format!("f32::from_bits((({x}) as u32))"),
                    LeType::F64 => format!("f64::from_bits({x})"),
                    LeType::Ref => format!("(({x}) as JsRef)"),
                }
            }
        }
    }

    /// Render a block-arm tail: a statement list whose final position must
    /// *yield* the next `pc` value (a `Goto` block id). `If` in tail position
    /// recurses with yielding arms; diverging statements (`Return`/`Trap`)
    /// unify `!` with the arm type; anything else is a skeleton violation,
    /// rendered as the statement followed by a diverging trap.
    fn arm_tail(
        &self,
        f: &LeFunc,
        stmts: &[LeStmt],
        depth: usize,
        out: &mut String,
    ) -> Result<(), LeDiag> {
        let pad = "  ".repeat(depth);
        for (i, s) in stmts.iter().enumerate() {
            let last = i + 1 == stmts.len();
            if !last {
                if matches!(s, LeStmt::Goto(_)) {
                    return Err(self.diag(
                        "Rust backend: Goto in a non-tail statement position".into(),
                    ));
                }
                self.stmt(f, s, depth, out)?;
                continue;
            }
            match s {
                LeStmt::Goto(id) => writeln!(out, "{pad}{}u32", id.0).unwrap(),
                LeStmt::If { cond, then, else_ } => {
                    writeln!(out, "{pad}if ({}) != 0 {{", self.expr(f, cond)).unwrap();
                    self.arm_tail(f, then, depth + 1, out)?;
                    writeln!(out, "{pad}}} else {{").unwrap();
                    if else_.is_empty() {
                        writeln!(out, "{pad}  js_trap()").unwrap();
                    } else {
                        self.arm_tail(f, else_, depth + 1, out)?;
                    }
                    writeln!(out, "{pad}}}").unwrap();
                }
                LeStmt::Return(_) | LeStmt::Trap | LeStmt::Unreachable => {
                    self.stmt(f, s, depth, out)?;
                }
                _ => {
                    self.stmt(f, s, depth, out)?;
                    writeln!(out, "{pad}js_trap()").unwrap();
                }
            }
        }
        if stmts.is_empty() {
            writeln!(out, "{pad}js_trap()").unwrap();
        }
        Ok(())
    }

    fn stmt(
        &self,
        f: &LeFunc,
        s: &LeStmt,
        depth: usize,
        out: &mut String,
    ) -> Result<(), LeDiag> {
        let pad = "  ".repeat(depth);
        match s {
            LeStmt::Assign(l, e) => {
                let rendered = self.expr(f, e);
                let coerced = self.coerce(f.locals[l.0 as usize].1, self.expr_ty(f, e), rendered);
                writeln!(out, "{pad}v{} = {coerced};", l.0).unwrap();
            }
            LeStmt::Call {
                callee,
                args,
                results,
            } => {
                if results.len() > 1 {
                    return Err(self.diag(format!(
                        "Rust backend: multi-result call ({} results) is not supported",
                        results.len()
                    )));
                }
                let runtime_params_storage;
                let param_tys: Option<&[LeType]> = match callee {
                    LeCallee::Direct(id) => self.fn_sigs.get(&id.0).map(|(p, _)| p.as_slice()),
                    LeCallee::Runtime(name) => {
                        runtime_params_storage = Self::runtime_params(name);
                        runtime_params_storage.as_deref()
                    }
                    LeCallee::Ptr(_) => None,
                };
                let arg_strs: Vec<String> = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let rendered = self.expr(f, a);
                        match param_tys.and_then(|p| p.get(i)) {
                            Some(&expected) => {
                                self.coerce(expected, self.expr_ty(f, a), rendered)
                            }
                            None => rendered,
                        }
                    })
                    .collect();
                let call = match callee {
                    LeCallee::Direct(id) if self.import_syms.contains_key(&id.0) => {
                        format!(
                            "unsafe {{ {}({}) }}",
                            self.fn_name(id.0),
                            arg_strs.join(", ")
                        )
                    }
                    LeCallee::Direct(id) => {
                        format!("{}({})", self.fn_name(id.0), arg_strs.join(", "))
                    }
                    LeCallee::Runtime(name) => format!("{name}({})", arg_strs.join(", ")),
                    LeCallee::Ptr(target) => {
                        let ret = match results.first() {
                            None => "()".to_string(),
                            Some(l) => rty(f.locals[l.0 as usize].1).to_string(),
                        };
                        let params: Vec<&str> =
                            args.iter().map(|a| rty(self.expr_ty(f, a))).collect();
                        format!(
                            "unsafe {{ core::mem::transmute::<usize, fn({}) -> {ret}>(({target})) }}({args})",
                            params.join(", "),
                            target = self.expr(f, target),
                            args = arg_strs.join(", ")
                        )
                    }
                };
                match results.first() {
                    None => writeln!(out, "{pad}{call};").unwrap(),
                    Some(l) => writeln!(out, "{pad}v{} = {call};", l.0).unwrap(),
                }
            }
            LeStmt::ArenaStore {
                ty,
                base,
                offset,
                value,
            } => {
                let suffix = match ty {
                    LeType::I8 => "i8",
                    LeType::I16 => "i16",
                    LeType::I32 => "i32",
                    LeType::I64 | LeType::Slot => "i64",
                    LeType::F32 => "f32",
                    LeType::F64 => "f64",
                    LeType::Ref => "ref",
                };
                let rendered = self.expr(f, value);
                let coerced = self.coerce(*ty, self.expr_ty(f, value), rendered);
                writeln!(
                    out,
                    "{pad}js_st_{suffix}((({base}) as u64).wrapping_add({offset}u64), {coerced});",
                    base = self.expr(f, base),
                )
                .unwrap();
            }
            LeStmt::SetField {
                layout,
                field,
                base,
                value,
            } => {
                let field_ty = if *layout == 0 {
                    LeType::I32 // header fields
                } else {
                    self.m.layouts[*layout as usize].fields[*field as usize]
                };
                let rendered = self.expr(f, value);
                let coerced = self.coerce(field_ty, self.expr_ty(f, value), rendered);
                writeln!(
                    out,
                    "{pad}unsafe {{ {} = {}; }}",
                    self.field_access(*layout, *field, &self.expr(f, base), true),
                    coerced
                )
                .unwrap();
            }
            LeStmt::SetIndex {
                layout,
                base,
                index,
                value,
            } => {
                let elem_ty = self.m.layouts[*layout as usize]
                    .array_element
                    .expect("array layout");
                let rendered = self.expr(f, value);
                let coerced = self.coerce(elem_ty, self.expr_ty(f, value), rendered);
                writeln!(
                    out,
                    "{pad}unsafe {{ {} = {}; }}",
                    self.index_access(*layout, &self.expr(f, base), &self.expr(f, index), true),
                    coerced
                )
                .unwrap();
            }
            LeStmt::GlobalSet(g, e) => {
                let rendered = self.expr(f, e);
                let coerced = self.coerce(
                    self.m.globals[*g as usize].0,
                    self.expr_ty(f, e),
                    rendered,
                );
                writeln!(out, "{pad}unsafe {{ JS_G{g} = {coerced}; }}").unwrap();
            }
            LeStmt::If { cond, then, else_ } => {
                // `Goto` in arm-tail position yields the arm value when this
                // If is itself in arm-tail position; the caller arranges the
                // match, so render the arms' gotos as bare values.
                writeln!(out, "{pad}if ({}) != 0 {{", self.expr(f, cond)).unwrap();
                for s in then {
                    self.stmt(f, s, depth + 1, out)?;
                }
                if else_.is_empty() {
                    writeln!(out, "{pad}}}").unwrap();
                } else {
                    writeln!(out, "{pad}}} else {{").unwrap();
                    for s in else_ {
                        self.stmt(f, s, depth + 1, out)?;
                    }
                    writeln!(out, "{pad}}}").unwrap();
                }
            }
            LeStmt::Loop(body) => {
                writeln!(out, "{pad}loop {{").unwrap();
                for s in body {
                    self.stmt(f, s, depth + 1, out)?;
                }
                writeln!(out, "{pad}}}").unwrap();
            }
            LeStmt::Break => writeln!(out, "{pad}break;").unwrap(),
                LeStmt::Return(values) => match values.len() {
                0 => writeln!(out, "{pad}return;").unwrap(),
                1 => {
                    let rendered = self.expr(f, &values[0]);
                    let coerced = self.coerce(
                        f.results[0],
                        self.expr_ty(f, &values[0]),
                        rendered,
                    );
                    writeln!(out, "{pad}return {coerced};").unwrap()
                }
                n => {
                    return Err(self.diag(format!(
                        "Rust backend: multi-result return ({n} results) is not supported"
                    )));
                }
            },
            LeStmt::Trap => writeln!(out, "{pad}js_trap();").unwrap(),
            LeStmt::Unreachable => writeln!(out, "{pad}js_trap(); // unreachable").unwrap(),
            LeStmt::Goto(id) => {
                return Err(self.diag(format!(
                    "Rust backend: stray Goto(bl{}) outside a block tail",
                    id.0
                )));
            }
            LeStmt::Label(_) => {} // labels are structural; the state machine keys arms
            LeStmt::RootFrameEnter { slots } => {
                writeln!(out, "{pad}js_coregc_root_enter({slots});").unwrap();
            }
            LeStmt::RootStore { slot, value } => {
                writeln!(out, "{pad}js_coregc_root_store({slot}, v{});", value.0).unwrap();
            }
            LeStmt::RootFramePop => writeln!(out, "{pad}js_coregc_root_pop();").unwrap(),
        }
        Ok(())
    }

    fn func(&self, id: u32, f: &LeFunc, out: &mut String) -> Result<(), LeDiag> {
        if self.import_syms.contains_key(&id) || matches!(f.body, super::LeBody::Runtime) {
            return Ok(());
        }
        if f.results.len() > 1 {
            return Err(self.diag(format!(
                "Rust backend: function {} (js_f{id}) has {} results; multi-result is not supported",
                f.name,
                f.results.len()
            )));
        }
        let ret = f
            .results
            .first()
            .map(|t| rty(*t).to_string())
            .unwrap_or_else(|| "()".into());
        let params: Vec<String> = f
            .params
            .iter()
            .map(|(l, t)| format!("mut v{}: {}", l.0, rty(*t)))
            .collect();
        writeln!(out, "fn js_f{id}({}) -> {ret} {{", params.join(", ")).unwrap();
        let param_ids: BTreeSet<u32> = f.params.iter().map(|(l, _)| l.0).collect();
        for (l, t) in &f.locals {
            if !param_ids.contains(&l.0) {
                let zero = match t {
                    LeType::F32 | LeType::F64 => "0.0",
                    _ => "0",
                };
                writeln!(out, "  let mut v{}: {} = {zero};", l.0, rty(*t)).unwrap();
            }
        }
        match &f.body {
            super::LeBody::Blocks(blocks) => {
                let entry = blocks.first().map(|b| b.id.0).unwrap_or(0);
                writeln!(out, "  let mut pc: u32 = {entry};").unwrap();
                writeln!(out, "  loop {{").unwrap();
                writeln!(out, "    pc = match pc {{").unwrap();
                for b in blocks {
                    writeln!(out, "      {}u32 => {{", b.id.0).unwrap();
                    self.arm_tail(f, &b.body, 4, out)?;
                    writeln!(out, "      }}").unwrap();
                }
                writeln!(out, "      _ => js_trap(),").unwrap();
                writeln!(out, "    }};").unwrap();
                writeln!(out, "  }}").unwrap();
            }
            super::LeBody::Structured(stmts) => {
                for s in stmts {
                    self.stmt(f, s, 1, out)?;
                }
            }
            super::LeBody::Runtime => unreachable!("handled above"),
        }
        // Diverging fallthrough guard.
        writeln!(out, "  js_trap()").unwrap();
        writeln!(out, "}}").unwrap();
        Ok(())
    }

    fn validate(&self) -> Result<(), LeDiag> {
        let lane = self.m.lane.as_str();
        if lane != "nano" && lane != "coregc" {
            return Err(self.diag(format!("Rust backend: unknown lane {lane:?}")));
        }
        for f in &self.m.funcs {
            let mut stack: Vec<&LeExpr> = Vec::new();
            let mut stmts: Vec<&LeStmt> = Vec::new();
            match &f.body {
                super::LeBody::Blocks(blocks) => {
                    for b in blocks {
                        stmts.extend(b.body.iter());
                    }
                }
                super::LeBody::Structured(body) => stmts.extend(body.iter()),
                super::LeBody::Runtime => {}
            }
            while let Some(s) = stmts.pop() {
                let stmt_lane_ok = match s {
                    LeStmt::ArenaStore { .. } => lane == "nano",
                    LeStmt::SetField { .. } | LeStmt::SetIndex { .. } => lane == "coregc",
                    LeStmt::RootFrameEnter { .. }
                    | LeStmt::RootStore { .. }
                    | LeStmt::RootFramePop => lane == "coregc",
                    _ => true,
                };
                if !stmt_lane_ok {
                    return Err(self.diag(format!(
                        "Rust backend: statement {:?} is not valid in the {lane} lane",
                        std::mem::discriminant(s)
                    )));
                }
                super::push_stmt_exprs(s, &mut stack);
                match s {
                    LeStmt::If { then, else_, .. } => {
                        stmts.extend(then.iter());
                        stmts.extend(else_.iter());
                    }
                    LeStmt::Loop(body) => stmts.extend(body.iter()),
                    _ => {}
                }
            }
            while let Some(e) = stack.pop() {
                let expr_lane_ok = match e {
                    LeExpr::ArenaLoad { .. } => lane == "nano",
                    LeExpr::Field { .. } | LeExpr::Index { .. } => lane == "coregc",
                    _ => true,
                };
                if !expr_lane_ok {
                    return Err(self.diag(format!(
                        "Rust backend: expression {:?} is not valid in the {lane} lane",
                        std::mem::discriminant(e)
                    )));
                }
                super::push_expr_children(e, &mut stack);
            }
        }
        Ok(())
    }
}

/// Render the module glue (everything after the fixed prelude/runtime).
fn render_module(m: &LeModule) -> Result<String, LeDiag> {
    let mut import_syms = BTreeMap::new();
    let mut taken = BTreeSet::new();
    for imp in &m.imports {
        let sym = format!("js_imp_{}_{}", rust_ident(&imp.module), rust_ident(&imp.name));
        if !taken.insert(sym.clone()) {
            return Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("Rust backend: import symbol collision on {sym}"),
            });
        }
        import_syms.insert(imp.func.0, sym);
    }
    let mut fn_sigs = BTreeMap::new();
    for (id, f) in m.funcs.iter().enumerate() {
        fn_sigs.insert(
            id as u32,
            (
                f.params.iter().map(|(_, t)| *t).collect::<Vec<_>>(),
                f.results.clone(),
            ),
        );
    }
    let r = Renderer {
        m,
        import_syms,
        fn_sigs,
    };
    r.validate()?;

    let mut out = String::new();
    writeln!(out, "// ---- module glue (lane: {}) ----", m.lane).unwrap();

    // Layouts (coregc): layout 0 is the header (JsHd lives in the runtime).
    if m.lane == "coregc" {
        for (i, layout) in m.layouts.iter().enumerate() {
            if i == 0 {
                continue;
            }
            if layout.array_element.is_none() {
                if layout.fields.is_empty() {
                    writeln!(out, "#[repr(C)]\nstruct JsL{i} {{ h: JsHd }}").unwrap();
                } else {
                    let fields: Vec<String> = layout
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(k, t)| format!("f{k}: {},", rty(*t)))
                        .collect();
                    writeln!(
                        out,
                        "#[repr(C)]\nstruct JsL{i} {{ h: JsHd, {} }}",
                        fields.join(" ")
                    )
                    .unwrap();
                }
            }
        }
        // The collector's layout table.
        writeln!(
            out,
            "fn js_layout_for(type_id: i32) -> Option<&'static JsLayoutInfo> {{"
        )
        .unwrap();
        for (i, layout) in m.layouts.iter().enumerate() {
            if i == 0 {
                continue;
            }
            let info = match &layout.array_element {
                Some(elem) => format!(
                    "JsLayoutInfo {{ type_id: {tid}, is_array: true, elem_size: core::mem::size_of::<{elem}>() as u32, elem_is_ref: {elem_is_ref}, ref_offsets: &[], size: 0, data_off: js_data_off::<{elem}>() as u32 }}",
                    tid = layout.type_id,
                    elem = rty(*elem),
                    elem_is_ref = (*elem == LeType::Ref),
                ),
                None => {
                    let ref_offsets: Vec<String> = layout
                        .fields
                        .iter()
                        .enumerate()
                        .filter(|(_, t)| **t == LeType::Ref)
                        .map(|(k, _)| format!("core::mem::offset_of!(JsL{i}, f{k}) as u32"))
                        .collect();
                    format!(
                        "JsLayoutInfo {{ type_id: {tid}, is_array: false, elem_size: 0, elem_is_ref: false, ref_offsets: &[{ro}], size: core::mem::size_of::<JsL{i}>() as u32, data_off: 0 }}",
                        tid = layout.type_id,
                        ro = ref_offsets.join(", "),
                    )
                }
            };
            writeln!(
                out,
                "  static LI_{i}: JsLayoutInfo = {info};"
            )
            .unwrap();
        }
        writeln!(out, "  match type_id {{").unwrap();
        for (i, layout) in m.layouts.iter().enumerate() {
            if i == 0 {
                continue;
            }
            writeln!(out, "    {} => Some(&LI_{i}),", layout.type_id).unwrap();
        }
        writeln!(out, "    _ => None,\n  }}\n}}").unwrap();
    }

    // Statics.
    for s in &m.statics {
        writeln!(out, "// static {:?}", s.name).unwrap();
        write!(out, "static JS_S_{}: [u8; {}] = [", rust_ident(&s.name).to_uppercase(), s.bytes.len()).unwrap();
        for b in &s.bytes {
            write!(out, "{b},").unwrap();
        }
        writeln!(out, "];").unwrap();
    }

    // Globals.
    for (i, (ty, init)) in m.globals.iter().enumerate() {
        let bits = init.unwrap_or(0);
        match ty {
            LeType::F32 => writeln!(
                out,
                "static mut JS_G{i}: f32 = f32::from_bits(0x{:08x}u32);",
                bits as u32
            )
            .unwrap(),
            LeType::F64 => writeln!(
                out,
                "static mut JS_G{i}: f64 = f64::from_bits(0x{bits:016x}u64);"
            )
            .unwrap(),
            LeType::Ref => {
                if bits != 0 {
                    return Err(r.diag(format!(
                        "Rust backend: global JS_G{i} has a nonzero reference init"
                    )));
                }
                writeln!(out, "static mut JS_G{i}: JsRef = 0;").unwrap();
            }
            LeType::I32 => {
                writeln!(out, "static mut JS_G{i}: i32 = (0x{:x}u32) as i32;", bits as u32).unwrap()
            }
            t => writeln!(out, "static mut JS_G{i}: {} = (0x{bits:x}u64) as {};", rty(*t), rty(*t))
                .unwrap(),
        }
    }

    // Imports.
    if !m.imports.is_empty() {
        writeln!(out, "extern \"C\" {{").unwrap();
        for imp in &m.imports {
            let ret = imp
                .results
                .first()
                .map(|t| format!(" -> {}", rty(*t)))
                .unwrap_or_default();
            let params: Vec<String> = imp
                .params
                .iter()
                .enumerate()
                .map(|(k, t)| format!("a{k}: {}", rty(*t)))
                .collect();
            writeln!(out, "  fn {}({}){};", r.import_syms[&imp.func.0], params.join(", "), ret)
                .unwrap();
        }
        writeln!(out, "}}").unwrap();
    }

    // Tables as match-dispatch fns (fn→usize casts are not const).
    for (i, table) in m.tables.iter().enumerate() {
        writeln!(out, "// table {:?}", table.name).unwrap();
        writeln!(out, "fn js_t{i}(i: usize) -> JsFnPtr {{").unwrap();
        writeln!(out, "  match i {{").unwrap();
        for (slot, f) in table.elems.iter().enumerate() {
            writeln!(out, "    {slot} => {} as usize,", r.fn_name(f.0)).unwrap();
        }
        writeln!(out, "    _ => js_trap(),\n  }}\n}}").unwrap();
    }

    // Function definitions.
    for (id, f) in m.funcs.iter().enumerate() {
        r.func(id as u32, f, &mut out)?;
    }

    // Export wrappers (boundary ABI; reset cross-call runtime state).
    let mut export_names = BTreeSet::new();
    for export in &m.exports {
        let sym = format!("js_export_{}", rust_ident(&export.name));
        if !export_names.insert(sym.clone()) {
            return Err(r.diag(format!("Rust backend: export name collision on {sym}")));
        }
        let f = &m.funcs[export.func.0 as usize];
        let ret = f
            .results
            .first()
            .map(|t| format!(" -> {}", rty(*t)))
            .unwrap_or_default();
        let params: Vec<String> = f
            .params
            .iter()
            .enumerate()
            .map(|(k, (_, t))| format!("a{k}: {}", rty(*t)))
            .collect();
        let args: Vec<String> = f.params.iter().enumerate().map(|(k, _)| format!("a{k}")).collect();
        writeln!(
            out,
            "#[no_mangle]\npub extern \"C\" fn {sym}({}){ret} {{",
            params.join(", ")
        )
        .unwrap();
        writeln!(out, "  js_boundary_reset();").unwrap();
        if f.results.is_empty() {
            writeln!(out, "  js_f{}({});", export.func.0, args.join(", ")).unwrap();
            writeln!(out, "  js_boundary_reset();").unwrap();
        } else {
            writeln!(out, "  let r = js_f{}({});", export.func.0, args.join(", ")).unwrap();
            writeln!(out, "  js_boundary_reset();").unwrap();
            writeln!(out, "  r").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }

    // Artifact init (runs the plan start function, if any).
    writeln!(out, "pub fn js_start() {{").unwrap();
    writeln!(out, "  js_boundary_reset();").unwrap();
    if let Some(start) = m.start {
        writeln!(out, "  js_f{}();", start.0).unwrap();
        writeln!(out, "  js_boundary_reset();").unwrap();
    }
    writeln!(out, "}}").unwrap();

    Ok(out)
}

/// Render a complete stable-Rust crate: prelude + lane runtime + module.
pub fn render_rust_file(m: &LeModule) -> Result<String, LeDiag> {
    let mut out = String::from(RUST_PRELUDE);
    match m.lane.as_str() {
        "nano" => out.push_str(RUST_RUNTIME_NANO),
        "coregc" => out.push_str(RUST_RUNTIME_COREGC),
        other => {
            return Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("Rust backend: unknown lane {other:?}"),
            });
        }
    }
    out.push_str(&render_module(m)?);
    Ok(out)
}
