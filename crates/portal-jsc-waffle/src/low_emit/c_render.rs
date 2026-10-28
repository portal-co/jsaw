//! The C renderer (DreamComp #38): `LeModule` → C99 translation unit.
//!
//! See `docs/plan-c-backend-38.md` for the rendering decisions. The renderer
//! consumes only the target-neutral IR plus the fixed runtime texts in
//! [`super::c_runtime`]; every representation concern has already been
//! lowered by the skeleton.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

use super::c_runtime::{C_PRELUDE, C_RUNTIME_COREGC, C_RUNTIME_NANO};
use super::{
    LeBinOp, LeBlock, LeCallee, LeDiag, LeExpr, LeFunc, LeLocal, LeModule, LeStmt, LeType, LeUnOp,
};

/// Sanitize an arbitrary name into a C identifier (deterministic).
pub fn c_ident(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() || out.as_bytes()[0].is_ascii_digit() {
        out.insert(0, '_');
    }
    out
}

fn c_ty(ty: LeType) -> &'static str {
    match ty {
        LeType::I8 => "int8_t",
        LeType::I16 => "int16_t",
        LeType::I32 => "int32_t",
        LeType::I64 => "int64_t",
        LeType::F32 => "float",
        LeType::F64 => "double",
        LeType::Ref => "js_ref",
        LeType::Slot => "uint64_t",
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
    /// Import func id → C symbol.
    import_syms: BTreeMap<u32, String>,
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
            LeUnOp::I32Eqz => format!("(int32_t)({x} == 0)"),
            LeUnOp::I32WrapI64 => format!("(int32_t)(uint32_t)({x})"),
            LeUnOp::I64ExtendI32S => format!("(int64_t)({x})"),
            LeUnOp::I64ExtendI32U => format!("(int64_t)(uint32_t)({x})"),
            LeUnOp::I32Extend8S => format!("(int32_t)(int8_t)({x})"),
            LeUnOp::I32Extend8U => format!("(int32_t)(uint8_t)({x})"),
            LeUnOp::I32Extend16S => format!("(int32_t)(int16_t)({x})"),
            LeUnOp::I32Extend16U => format!("(int32_t)(uint16_t)({x})"),
            // Trapping conversions: in-range by construction (skeleton guard).
            LeUnOp::I32TruncF64S => format!("(int32_t)({x})"),
            LeUnOp::I32TruncF64U => format!("(int32_t)(uint32_t)({x})"),
            LeUnOp::I64TruncF64S => format!("(int64_t)({x})"),
            LeUnOp::I64TruncF64U => format!("(int64_t)(uint64_t)({x})"),
            LeUnOp::I32TruncSatF64S => format!("js_trunc_sat_s32({x})"),
            LeUnOp::I32TruncSatF64U => format!("js_trunc_sat_u32({x})"),
            LeUnOp::I64TruncSatF64S => format!("js_trunc_sat_s64({x})"),
            LeUnOp::I64TruncSatF64U => format!("js_trunc_sat_u64({x})"),
            LeUnOp::F32DemoteF64 => format!("(float)({x})"),
            LeUnOp::F64PromoteF32 => format!("(double)({x})"),
            LeUnOp::F32Abs => format!("fabsf({x})"),
            LeUnOp::F64Abs => format!("fabs({x})"),
            LeUnOp::F64Ceil => format!("ceil({x})"),
            LeUnOp::F64Floor => format!("floor({x})"),
            LeUnOp::F64Sqrt => format!("sqrt({x})"),
            LeUnOp::F64Trunc => format!("trunc({x})"),
            LeUnOp::F64Nearest => format!("nearbyint({x})"),
            LeUnOp::F64Neg => format!("(-({x}))"),
            LeUnOp::F64ConvertI32S => format!("(double)(int32_t)({x})"),
            LeUnOp::F64ConvertI32U => format!("(double)(uint32_t)({x})"),
            LeUnOp::F64ConvertI64S => format!("(double)({x})"),
            LeUnOp::F64ConvertI64U => format!("(double)(uint64_t)({x})"),
            LeUnOp::F64ReinterpretI64 => format!("js_f64bits((uint64_t)({x}))"),
            LeUnOp::I64ReinterpretF64 => format!("(int64_t)js_bits_f64({x})"),
            LeUnOp::F32ReinterpretI32 => format!("js_f32bits((uint32_t)({x}))"),
            LeUnOp::I32ReinterpretF32 => format!("(int32_t)js_bits_f32({x})"),
        }
    }

    fn binop(&self, op: LeBinOp, a: &str, b: &str) -> String {
        use LeBinOp::*;
        match op {
            I32Add => format!("(int32_t)((uint32_t){a} + (uint32_t){b})"),
            I32Sub => format!("(int32_t)((uint32_t){a} - (uint32_t){b})"),
            I32Mul => format!("(int32_t)((uint32_t){a} * (uint32_t){b})"),
            I32DivS => format!("js_div_s32({a}, {b})"),
            I32RemS => format!("js_rem_s32({a}, {b})"),
            I32DivU => format!("js_div_u32((uint32_t){a}, (uint32_t){b})"),
            I32RemU => format!("js_rem_u32((uint32_t){a}, (uint32_t){b})"),
            I32And => format!("(int32_t)((uint32_t){a} & (uint32_t){b})"),
            I32Or => format!("(int32_t)((uint32_t){a} | (uint32_t){b})"),
            I32Xor => format!("(int32_t)((uint32_t){a} ^ (uint32_t){b})"),
            I32Shl => format!("(int32_t)((uint32_t){a} << ((uint32_t){b} & 31))"),
            I32ShrS => format!("(int32_t)((int32_t){a} >> ((uint32_t){b} & 31))"),
            I32ShrU => format!("(int32_t)((uint32_t){a} >> ((uint32_t){b} & 31))"),
            I32Eq => format!("(int32_t)({a} == {b})"),
            I32Ne => format!("(int32_t)({a} != {b})"),
            I32LtS => format!("(int32_t)({a} < {b})"),
            I32LtU => format!("(int32_t)((uint32_t){a} < (uint32_t){b})"),
            I32LeS => format!("(int32_t)({a} <= {b})"),
            I32LeU => format!("(int32_t)((uint32_t){a} <= (uint32_t){b})"),
            I32GtS => format!("(int32_t)({a} > {b})"),
            I32GtU => format!("(int32_t)((uint32_t){a} > (uint32_t){b})"),
            I32GeS => format!("(int32_t)({a} >= {b})"),
            I32GeU => format!("(int32_t)((uint32_t){a} >= (uint32_t){b})"),
            I64Add => format!("(int64_t)((uint64_t){a} + (uint64_t){b})"),
            I64Sub => format!("(int64_t)((uint64_t){a} - (uint64_t){b})"),
            I64Mul => format!("(int64_t)((uint64_t){a} * (uint64_t){b})"),
            I64DivS => format!("js_div_s64({a}, {b})"),
            I64RemS => format!("js_rem_s64({a}, {b})"),
            I64DivU => format!("js_div_u64((uint64_t){a}, (uint64_t){b})"),
            I64RemU => format!("js_rem_u64((uint64_t){a}, (uint64_t){b})"),
            I64And => format!("(int64_t)((uint64_t){a} & (uint64_t){b})"),
            I64Or => format!("(int64_t)((uint64_t){a} | (uint64_t){b})"),
            I64Xor => format!("(int64_t)((uint64_t){a} ^ (uint64_t){b})"),
            I64Shl => format!("(int64_t)((uint64_t){a} << ((uint64_t){b} & 63))"),
            I64ShrS => format!("(int64_t)((int64_t){a} >> ((uint64_t){b} & 63))"),
            I64ShrU => format!("(int64_t)((uint64_t){a} >> ((uint64_t){b} & 63))"),
            I64Eq => format!("(int32_t)({a} == {b})"),
            I64Ne => format!("(int32_t)({a} != {b})"),
            I64LtS => format!("(int32_t)({a} < {b})"),
            I64LtU => format!("(int32_t)((uint64_t){a} < (uint64_t){b})"),
            I64LeS => format!("(int32_t)({a} <= {b})"),
            I64LeU => format!("(int32_t)((uint64_t){a} <= (uint64_t){b})"),
            I64GtS => format!("(int32_t)({a} > {b})"),
            I64GtU => format!("(int32_t)((uint64_t){a} > (uint64_t){b})"),
            I64GeS => format!("(int32_t)({a} >= {b})"),
            I64GeU => format!("(int32_t)((uint64_t){a} >= (uint64_t){b})"),
            F32Add => format!("js_f32add({a}, {b})"),
            F32Sub => format!("js_f32sub({a}, {b})"),
            F32Mul => format!("js_f32mul({a}, {b})"),
            F32Div => format!("js_f32div({a}, {b})"),
            F32Eq => format!("(int32_t)({a} == {b})"),
            F32Ne => format!("(int32_t)({a} != {b})"),
            F32Lt => format!("(int32_t)({a} < {b})"),
            F32Le => format!("(int32_t)({a} <= {b})"),
            F32Gt => format!("(int32_t)({a} > {b})"),
            F32Ge => format!("(int32_t)({a} >= {b})"),
            F64Add => format!("({a} + {b})"),
            F64Sub => format!("({a} - {b})"),
            F64Mul => format!("({a} * {b})"),
            F64Div => format!("({a} / {b})"),
            F64Min => format!("js_wmin({a}, {b})"),
            F64Max => format!("js_wmax({a}, {b})"),
            F64Eq => format!("(int32_t)({a} == {b})"),
            F64Ne => format!("(int32_t)({a} != {b})"),
            F64Lt => format!("(int32_t)({a} < {b})"),
            F64Le => format!("(int32_t)({a} <= {b})"),
            F64Gt => format!("(int32_t)({a} > {b})"),
            F64Ge => format!("(int32_t)({a} >= {b})"),
        }
    }

    fn field_access(&self, layout: u32, field: u32, base: &str) -> String {
        const HEADER_FIELDS: [&str; 4] = ["type_id", "flags", "len", "pad"];
        if layout == 0 {
            format!(
                "((js_hd *)(uintptr_t)({base}))->{}",
                HEADER_FIELDS[field as usize]
            )
        } else {
            format!("((struct js_L{layout} *)(uintptr_t)({base}))->f{field}")
        }
    }

    fn index_access(&self, layout: u32, base: &str, index: &str) -> String {
        format!("((struct js_L{layout} *)(uintptr_t)({base}))->data[(uint32_t)({index})]")
    }

    fn expr(&self, f: &LeFunc, e: &LeExpr) -> String {
        match e {
            LeExpr::Local(l) => format!("v{}", l.0),
            LeExpr::ConstI32(v) => format!("(int32_t){v}u"),
            LeExpr::ConstI64(v) => format!("(int64_t){v}ull"),
            LeExpr::ConstF32(bits) => format!("js_f32bits(0x{bits:08x}u)"),
            LeExpr::ConstF64(bits) => format!("js_f64bits(UINT64_C(0x{bits:016x}))"),
            LeExpr::Unary(op, x) => self.unop(*op, &self.expr(f, x)),
            LeExpr::Binary(op, a, b) => self.binop(*op, &self.expr(f, a), &self.expr(f, b)),
            LeExpr::Select(c, t, fl) => format!(
                "(({c}) ? ({t}) : ({fl}))",
                c = self.expr(f, c),
                t = self.expr(f, t),
                fl = self.expr(f, fl)
            ),
            LeExpr::GlobalGet(g) => format!("js_g{g}"),
            LeExpr::TableElem { table, index } => {
                format!("js_t{table}[{i}]", i = self.expr(f, index))
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
                    "js_ld_{suffix}((uint64_t)({base}) + {offset}u)",
                    base = self.expr(f, base)
                )
            }
            LeExpr::Field {
                layout,
                field,
                base,
            } => self.field_access(*layout, *field, &self.expr(f, base)),
            LeExpr::Index {
                layout,
                base,
                index,
            } => self.index_access(*layout, &self.expr(f, base), &self.expr(f, index)),
            LeExpr::ToSlot(ty, x) => {
                let x = self.expr(f, x);
                match ty {
                    LeType::I8 => format!("(uint64_t)(uint8_t)({x})"),
                    LeType::I16 => format!("(uint64_t)(uint16_t)({x})"),
                    LeType::I32 => format!("(uint64_t)(uint32_t)({x})"),
                    LeType::I64 | LeType::Slot => format!("(uint64_t)({x})"),
                    LeType::F32 => format!("(uint64_t)js_bits_f32({x})"),
                    LeType::F64 => format!("js_bits_f64({x})"),
                    LeType::Ref => format!("(uint64_t)({x})"),
                }
            }
            LeExpr::FromSlot(ty, x) => {
                let x = self.expr(f, x);
                match ty {
                    LeType::I8 => format!("(int8_t)(uint8_t)({x})"),
                    LeType::I16 => format!("(int16_t)(uint16_t)({x})"),
                    LeType::I32 => format!("(int32_t)(uint32_t)({x})"),
                    LeType::I64 | LeType::Slot => format!("(int64_t)({x})"),
                    LeType::F32 => format!("js_f32bits((uint32_t)({x}))"),
                    LeType::F64 => format!("js_f64bits({x})"),
                    LeType::Ref => format!("(js_ref)(uintptr_t)({x})"),
                }
            }
        }
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
                writeln!(out, "{pad}v{} = {};", l.0, self.expr(f, e)).unwrap();
            }
            LeStmt::Call {
                callee,
                args,
                results,
            } => {
                if results.len() > 1 {
                    return Err(self.diag(format!(
                        "C backend: multi-result call ({} results) is not supported",
                        results.len()
                    )));
                }
                let arg_strs: Vec<String> = args.iter().map(|a| self.expr(f, a)).collect();
                let call = match callee {
                    LeCallee::Direct(id) => {
                        format!("{}({})", self.fn_name(id.0), arg_strs.join(", "))
                    }
                    LeCallee::Runtime(name) => format!("{name}({})", arg_strs.join(", ")),
                    LeCallee::Ptr(target) => {
                        let ret = match results.first() {
                            None => "void",
                            Some(l) => c_ty(f.locals[l.0 as usize].1),
                        };
                        let params: Vec<&str> = args
                            .iter()
                            .map(|a| c_ty(self.expr_ty(f, a)))
                            .collect();
                        format!(
                            "(({ret} (*)({params}))({target}))({args})",
                            params = params.join(", "),
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
                writeln!(
                    out,
                    "{pad}js_st_{suffix}((uint64_t)({base}) + {offset}u, {value});",
                    base = self.expr(f, base),
                    value = self.expr(f, value)
                )
                .unwrap();
            }
            LeStmt::SetField {
                layout,
                field,
                base,
                value,
            } => {
                writeln!(
                    out,
                    "{pad}{} = {};",
                    self.field_access(*layout, *field, &self.expr(f, base)),
                    self.expr(f, value)
                )
                .unwrap();
            }
            LeStmt::SetIndex {
                layout,
                base,
                index,
                value,
            } => {
                writeln!(
                    out,
                    "{pad}{} = {};",
                    self.index_access(*layout, &self.expr(f, base), &self.expr(f, index)),
                    self.expr(f, value)
                )
                .unwrap();
            }
            LeStmt::GlobalSet(g, e) => {
                writeln!(out, "{pad}js_g{g} = {};", self.expr(f, e)).unwrap();
            }
            LeStmt::If { cond, then, else_ } => {
                writeln!(out, "{pad}if ({}) {{", self.expr(f, cond)).unwrap();
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
                writeln!(out, "{pad}for (;;) {{").unwrap();
                for s in body {
                    self.stmt(f, s, depth + 1, out)?;
                }
                writeln!(out, "{pad}}}").unwrap();
            }
            LeStmt::Break => writeln!(out, "{pad}break;").unwrap(),
            LeStmt::Return(values) => match values.len() {
                0 => writeln!(out, "{pad}return;").unwrap(),
                1 => writeln!(out, "{pad}return {};", self.expr(f, &values[0])).unwrap(),
                n => {
                    return Err(self.diag(format!(
                        "C backend: multi-result return ({n} results) is not supported"
                    )));
                }
            },
            LeStmt::Trap => writeln!(out, "{pad}js_trap();").unwrap(),
            LeStmt::Unreachable => writeln!(out, "{pad}js_trap(); /* unreachable */").unwrap(),
            LeStmt::Goto(id) => writeln!(out, "{pad}goto bl{};", id.0).unwrap(),
            LeStmt::Label(id) => writeln!(out, "bl{}: ;", id.0).unwrap(),
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

    fn block(&self, f: &LeFunc, b: &LeBlock, out: &mut String) -> Result<(), LeDiag> {
        // The skeleton begins every block body with its own `Label` stmt.
        for s in &b.body {
            self.stmt(f, s, 1, out)?;
        }
        Ok(())
    }

    fn func(&self, id: u32, f: &LeFunc, out: &mut String) -> Result<(), LeDiag> {
        if self.import_syms.contains_key(&id) {
            return Ok(()); // extern prototype only
        }
        if matches!(f.body, super::LeBody::Runtime) {
            return Ok(()); // defined by the fixed runtime text
        }
        if f.results.len() > 1 {
            return Err(self.diag(format!(
                "C backend: function {} (js_f{id}) has {} results; multi-result is not supported",
                f.name,
                f.results.len()
            )));
        }
        let ret = f
            .results
            .first()
            .map(|t| c_ty(*t))
            .unwrap_or("void");
        let params: Vec<String> = f
            .params
            .iter()
            .map(|(l, t)| format!("{} v{}", c_ty(*t), l.0))
            .collect();
        writeln!(out, "static {ret} js_f{id}({params}) {{", params = params.join(", ")).unwrap();
        let param_ids: BTreeSet<u32> = f.params.iter().map(|(l, _)| l.0).collect();
        for (l, t) in &f.locals {
            if !param_ids.contains(&l.0) {
                writeln!(out, "  {} v{} = 0;", c_ty(*t), l.0).unwrap();
            }
        }
        match &f.body {
            super::LeBody::Blocks(blocks) => {
                for b in blocks {
                    self.block(f, b, out)?;
                }
            }
            super::LeBody::Structured(stmts) => {
                for s in stmts {
                    self.stmt(f, s, 1, out)?;
                }
            }
            super::LeBody::Runtime => unreachable!("handled above"),
        }
        // Fallthrough guard: every path is total even if the CFG is unusual.
        writeln!(out, "  js_trap();").unwrap();
        if let Some(t) = f.results.first() {
            writeln!(out, "  return ({})0;", c_ty(*t)).unwrap();
        }
        writeln!(out, "}}").unwrap();
        Ok(())
    }

    /// Lane-consistency and capability validation (fail closed, #38
    /// acceptance #4).
    fn validate(&self) -> Result<(), LeDiag> {
        let lane = self.m.lane.as_str();
        if lane != "nano" && lane != "coregc" {
            return Err(self.diag(format!("C backend: unknown lane {lane:?}")));
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
                        "C backend: statement {:?} is not valid in the {lane} lane",
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
                        "C backend: expression {:?} is not valid in the {lane} lane",
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
        let sym = format!("js_imp_{}_{}", c_ident(&imp.module), c_ident(&imp.name));
        if !taken.insert(sym.clone()) {
            return Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("C backend: import symbol collision on {sym}"),
            });
        }
        import_syms.insert(imp.func.0, sym);
    }
    let r = Renderer { m, import_syms };
    r.validate()?;

    let mut out = String::new();
    writeln!(out, "/* ---- module glue (lane: {}) ---- */", m.lane).unwrap();

    // Layouts (coregc): layout 0 is the header (js_hd lives in the runtime).
    if m.lane == "coregc" {
        for (i, layout) in m.layouts.iter().enumerate() {
            if i == 0 {
                continue; // js_hd
            }
            match &layout.array_element {
                Some(elem) => writeln!(
                    out,
                    "struct js_L{i} {{ js_hd h; {} data[]; }};",
                    c_ty(*elem)
                )
                .unwrap(),
                None => {
                    if layout.fields.is_empty() {
                        writeln!(out, "struct js_L{i} {{ js_hd h; }};").unwrap();
                    } else {
                        let fields: Vec<String> = layout
                            .fields
                            .iter()
                            .enumerate()
                            .map(|(k, t)| format!("{} f{k};", c_ty(*t)))
                            .collect();
                        writeln!(out, "struct js_L{i} {{ js_hd h; {} }};", fields.join(" "))
                            .unwrap();
                    }
                }
            }
        }
        // The collector's layout table.
        writeln!(
            out,
            "const js_layout_info *js_layout_for(int32_t type_id) {{"
        )
        .unwrap();
        for (i, layout) in m.layouts.iter().enumerate() {
            if i == 0 {
                continue;
            }
            let ref_offsets: Vec<String> = layout
                .fields
                .iter()
                .enumerate()
                .filter(|(_, t)| **t == LeType::Ref)
                .map(|(k, _)| format!("(uint32_t)offsetof(struct js_L{i}, f{k})"))
                .collect();
            if !ref_offsets.is_empty() {
                writeln!(
                    out,
                    "  static const uint32_t ro_{i}[] = {{ {} }};",
                    ref_offsets.join(", ")
                )
                .unwrap();
            }
        }
        writeln!(out, "  switch (type_id) {{").unwrap();
        for (i, layout) in m.layouts.iter().enumerate() {
            if i == 0 {
                continue;
            }
            let ref_offsets: Vec<String> = layout
                .fields
                .iter()
                .enumerate()
                .filter(|(_, t)| **t == LeType::Ref)
                .map(|(k, _)| format!("(uint32_t)offsetof(struct js_L{i}, f{k})"))
                .collect();
            let ro = if ref_offsets.is_empty() {
                "0".to_string()
            } else {
                format!("ro_{i}")
            };
            let (is_array, elem_size, elem_is_ref, size, data_off) = match &layout.array_element {
                Some(elem) => (
                    1,
                    format!("(uint32_t)sizeof({})", c_ty(*elem)),
                    u32::from(*elem == LeType::Ref),
                    "0".to_string(),
                    format!("(uint32_t)offsetof(struct js_L{i}, data)"),
                ),
                None => (
                    0,
                    "0".to_string(),
                    0,
                    format!("(uint32_t)sizeof(struct js_L{i})"),
                    "0".to_string(),
                ),
            };
            writeln!(
                out,
                "  case {tid}: {{ static const js_layout_info li = {{ {tid}, {is_array}, {elem_size}, {elem_is_ref}, {n_ref}, {ro}, {size}, {data_off} }}; return &li; }}",
                tid = layout.type_id,
                n_ref = ref_offsets.len(),
            )
            .unwrap();
        }
        writeln!(out, "  default: return 0;").unwrap();
        writeln!(out, "  }}").unwrap();
        writeln!(out, "}}").unwrap();
    }

    // Statics.
    for s in &m.statics {
        writeln!(out, "/* static {:?} */", s.name).unwrap();
        write!(out, "const unsigned char js_s_{}[] = {{", c_ident(&s.name)).unwrap();
        for (i, b) in s.bytes.iter().enumerate() {
            if i % 16 == 0 {
                write!(out, "\n  ").unwrap();
            }
            write!(out, "{b},").unwrap();
        }
        writeln!(out, "\n}};").unwrap();
    }

    // Globals.
    for (i, (ty, init)) in m.globals.iter().enumerate() {
        let bits = init.unwrap_or(0);
        match ty {
            LeType::F32 => {
                writeln!(
                    out,
                    "static union {{ uint32_t b; float f; }} js_g{i}_u = {{ 0x{:08x}u }};",
                    bits as u32
                )
                .unwrap();
                writeln!(out, "#define js_g{i} (js_g{i}_u.f)").unwrap();
            }
            LeType::F64 => {
                writeln!(
                    out,
                    "static union {{ uint64_t b; double d; }} js_g{i}_u = {{ UINT64_C(0x{bits:016x}) }};"
                )
                .unwrap();
                writeln!(out, "#define js_g{i} (js_g{i}_u.d)").unwrap();
            }
            LeType::Ref => {
                if bits != 0 {
                    return Err(r.diag(format!(
                        "C backend: global js_g{i} has a nonzero reference init"
                    )));
                }
                writeln!(out, "static js_ref js_g{i} = 0;").unwrap();
            }
            LeType::I64 | LeType::Slot => {
                writeln!(out, "static int64_t js_g{i} = (int64_t)UINT64_C(0x{bits:016x});").unwrap();
            }
            t => writeln!(out, "static {} js_g{i} = ({})0x{:x}u;", c_ty(*t), c_ty(*t), bits as u32)
                .unwrap(),
        }
    }

    // Import prototypes.
    for imp in &m.imports {
        let ret = imp
            .results
            .first()
            .map(|t| c_ty(*t))
            .unwrap_or("void");
        let params: Vec<&str> = imp.params.iter().map(|t| c_ty(*t)).collect();
        writeln!(
            out,
            "extern {ret} {}({});",
            r.import_syms[&imp.func.0],
            if params.is_empty() {
                "void".into()
            } else {
                params.join(", ")
            }
        )
        .unwrap();
    }

    // Function prototypes (needed before the tables).
    for (id, f) in m.funcs.iter().enumerate() {
        if r.import_syms.contains_key(&(id as u32)) || matches!(f.body, super::LeBody::Runtime) {
            continue;
        }
        let ret = f.results.first().map(|t| c_ty(*t)).unwrap_or("void");
        let params: Vec<String> = f
            .params
            .iter()
            .map(|(l, t)| format!("{} v{}", c_ty(*t), l.0))
            .collect();
        writeln!(out, "static {ret} js_f{id}({});", params.join(", ")).unwrap();
    }

    // Tables.
    for (i, table) in m.tables.iter().enumerate() {
        let elems: Vec<String> = table
            .elems
            .iter()
            .map(|f| {
                if r.import_syms.contains_key(&f.0) {
                    format!("(js_fnptr_t){}", r.import_syms[&f.0])
                } else {
                    format!("(js_fnptr_t)js_f{}", f.0)
                }
            })
            .collect();
        writeln!(out, "/* table {:?} */", table.name).unwrap();
        writeln!(
            out,
            "static js_fnptr_t const js_t{i}[{}] = {{ {} }};",
            elems.len(),
            elems.join(", ")
        )
        .unwrap();
    }

    // Function definitions.
    for (id, f) in m.funcs.iter().enumerate() {
        r.func(id as u32, f, &mut out)?;
    }

    // Export wrappers (boundary ABI; reset cross-call runtime state).
    let mut export_names = BTreeSet::new();
    for export in &m.exports {
        let sym = format!("js_export_{}", c_ident(&export.name));
        if !export_names.insert(sym.clone()) {
            return Err(r.diag(format!("C backend: export name collision on {sym}")));
        }
        let f = &m.funcs[export.func.0 as usize];
        let ret = f.results.first().map(|t| c_ty(*t)).unwrap_or("void");
        let params: Vec<String> = f
            .params
            .iter()
            .enumerate()
            .map(|(k, (_, t))| format!("{} a{k}", c_ty(*t)))
            .collect();
        let args: Vec<String> = f.params.iter().enumerate().map(|(k, _)| format!("a{k}")).collect();
        writeln!(out, "{ret} {sym}({params}) {{", params = params.join(", ")).unwrap();
        writeln!(out, "  js_boundary_reset();").unwrap();
        if f.results.is_empty() {
            writeln!(out, "  js_f{}({});", export.func.0, args.join(", ")).unwrap();
            writeln!(out, "  js_boundary_reset();").unwrap();
        } else {
            writeln!(
                out,
                "  {ret} r = js_f{}({});",
                export.func.0,
                args.join(", ")
            )
            .unwrap();
            writeln!(out, "  js_boundary_reset();").unwrap();
            writeln!(out, "  return r;").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }

    // Artifact init (runs the plan start function, if any).
    writeln!(out, "void js_start(void) {{").unwrap();
    writeln!(out, "  js_boundary_reset();").unwrap();
    if let Some(start) = m.start {
        writeln!(out, "  js_f{}();", start.0).unwrap();
        writeln!(out, "  js_boundary_reset();").unwrap();
    }
    writeln!(out, "}}").unwrap();

    Ok(out)
}

/// Render a complete C99 translation unit: prelude + lane runtime + module.
pub fn render_c_file(m: &LeModule) -> Result<String, LeDiag> {
    let mut out = String::from(C_PRELUDE);
    match m.lane.as_str() {
        "nano" => out.push_str(C_RUNTIME_NANO),
        "coregc" => out.push_str(C_RUNTIME_COREGC),
        other => {
            return Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("C backend: unknown lane {other:?}"),
            });
        }
    }
    out.push_str(&render_module(m)?);
    Ok(out)
}
