//! Debug text renderer for [`LeModule`] — the #36 smoke consumer.
//!
//! Renders the target-neutral IR in a stable, fully deterministic text
//! format so tests can prove that both representation implementations cover
//! the corpus operator surface and that emission is deterministic (emit
//! twice, compare). This is NOT a target backend; it performs no validation
//! beyond what emission already did.

use super::{
    LeBinOp, LeBlock, LeBody, LeCallee, LeExpr, LeFunc, LeModule, LeStmt, LeUnOp,
};
use std::fmt::Write as _;

/// Render the module as stable debug text.
pub fn render_debug(module: &LeModule) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "module {} lane={}", module.schema, module.lane);
    for (i, layout) in module.layouts.iter().enumerate() {
        let _ = writeln!(
            out,
            "layout {i} type_id={} fields=[{}] array_element={}",
            layout.type_id,
            layout
                .fields
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(","),
            layout
                .array_element
                .map(|t| t.to_string())
                .unwrap_or_else(|| "-".into())
        );
    }
    for s in &module.statics {
        let _ = writeln!(out, "static {} ({} bytes)", s.name, s.bytes.len());
    }
    for (i, table) in module.tables.iter().enumerate() {
        let _ = writeln!(
            out,
            "table {i} {}: [{}]",
            table.name,
            table
                .elems
                .iter()
                .map(|f| f.0.to_string())
                .collect::<Vec<_>>()
                .join(",")
        );
    }
    for g in &module.globals {
        let _ = writeln!(out, "global {} = {:?}", g.0, g.1);
    }
    for import in &module.imports {
        let _ = writeln!(
            out,
            "import {}.{} = f{} ({:?}) -> {:?}",
            import.module, import.name, import.func.0, import.params, import.results
        );
    }
    for export in &module.exports {
        let _ = writeln!(out, "export {} = f{}", export.name, export.func.0);
    }
    if let Some(start) = module.start {
        let _ = writeln!(out, "start f{}", start.0);
    }
    for (id, func) in module.funcs.iter().enumerate() {
        render_func(&mut out, id, func);
    }
    out
}

fn render_func(out: &mut String, id: usize, func: &LeFunc) {
    let _ = writeln!(
        out,
        "fn f{id} {}({}) -> ({})",
        func.name,
        func.params
            .iter()
            .map(|(l, t)| format!("v{}:{}", l.0, t))
            .collect::<Vec<_>>()
            .join(","),
        func.results
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(",")
    );
    for (l, t) in &func.locals {
        let _ = writeln!(out, "  local v{}: {}", l.0, t);
    }
    match &func.body {
        LeBody::Blocks(blocks) => {
            for block in blocks {
                render_block(out, block);
            }
        }
        LeBody::Structured(stmts) => {
            for stmt in stmts {
                render_stmt(out, stmt, 1);
            }
        }
        LeBody::Runtime => {
            let _ = writeln!(out, "  runtime");
        }
    }
}

fn render_block(out: &mut String, block: &LeBlock) {
    let _ = writeln!(
        out,
        "  block b{}({})",
        block.id.0,
        block
            .params
            .iter()
            .map(|l| format!("v{}", l.0))
            .collect::<Vec<_>>()
            .join(",")
    );
    for stmt in &block.body {
        render_stmt(out, stmt, 2);
    }
}

fn render_stmt(out: &mut String, stmt: &LeStmt, indent: usize) {
    let pad = "  ".repeat(indent);
    match stmt {
        LeStmt::Assign(l, e) => {
            let _ = writeln!(out, "{pad}v{} = {}", l.0, expr(e));
        }
        LeStmt::Call {
            callee,
            args,
            results,
        } => {
            let _ = writeln!(
                out,
                "{pad}[{}] = call {}({})",
                results
                    .iter()
                    .map(|l| format!("v{}", l.0))
                    .collect::<Vec<_>>()
                    .join(","),
                render_callee(callee),
                args.iter().map(expr).collect::<Vec<_>>().join(",")
            );
        }
        LeStmt::ArenaStore {
            ty,
            base,
            offset,
            value,
        } => {
            let _ = writeln!(
                out,
                "{pad}astore {ty} [{} + {offset}] = {}",
                expr(base),
                expr(value)
            );
        }
        LeStmt::SetField {
            layout,
            field,
            base,
            value,
        } => {
            let _ = writeln!(
                out,
                "{pad}setf l{layout}.{field} {} = {}",
                expr(base),
                expr(value)
            );
        }
        LeStmt::SetIndex {
            layout,
            base,
            index,
            value,
        } => {
            let _ = writeln!(
                out,
                "{pad}seti l{layout} {}[{}] = {}",
                expr(base),
                expr(index),
                expr(value)
            );
        }
        LeStmt::GlobalSet(g, e) => {
            let _ = writeln!(out, "{pad}global {g} = {}", expr(e));
        }
        LeStmt::If { cond, then, else_ } => {
            let _ = writeln!(out, "{pad}if {}", expr(cond));
            for s in then {
                render_stmt(out, s, indent + 1);
            }
            if !else_.is_empty() {
                let _ = writeln!(out, "{pad}else");
                for s in else_ {
                    render_stmt(out, s, indent + 1);
                }
            }
        }
        LeStmt::Loop(body) => {
            let _ = writeln!(out, "{pad}loop");
            for s in body {
                render_stmt(out, s, indent + 1);
            }
        }
        LeStmt::Break => {
            let _ = writeln!(out, "{pad}break");
        }
        LeStmt::Return(values) => {
            let _ = writeln!(
                out,
                "{pad}return {}",
                values.iter().map(expr).collect::<Vec<_>>().join(",")
            );
        }
        LeStmt::Trap => {
            let _ = writeln!(out, "{pad}trap");
        }
        LeStmt::Unreachable => {
            let _ = writeln!(out, "{pad}unreachable");
        }
        LeStmt::Goto(b) => {
            let _ = writeln!(out, "{pad}goto b{}", b.0);
        }
        LeStmt::Label(b) => {
            let _ = writeln!(out, "{pad}label b{}", b.0);
        }
        LeStmt::RootFrameEnter { slots } => {
            let _ = writeln!(out, "{pad}root_enter {slots}");
        }
        LeStmt::RootStore { slot, value } => {
            let _ = writeln!(out, "{pad}root_store {slot} = v{}", value.0);
        }
        LeStmt::RootFramePop => {
            let _ = writeln!(out, "{pad}root_pop");
        }
    }
}

fn render_callee(c: &LeCallee) -> String {
    match c {
        LeCallee::Direct(f) => format!("f{}", f.0),
        LeCallee::Ptr(e) => format!("*{}", expr(e)),
        LeCallee::Runtime(name) => format!("@{name}"),
    }
}

fn expr(e: &LeExpr) -> String {
    match e {
        LeExpr::Local(l) => format!("v{}", l.0),
        LeExpr::ConstI32(v) => format!("{v}i32"),
        LeExpr::ConstI64(v) => format!("{v}i64"),
        LeExpr::ConstF32(v) => format!("{:08x}f32", v),
        LeExpr::ConstF64(v) => format!("{:016x}f64", v),
        LeExpr::Unary(op, a) => format!("{}({})", unop(*op), expr(a)),
        LeExpr::Binary(op, a, b) => format!("{}({}, {})", binop(*op), expr(a), expr(b)),
        LeExpr::Select(c, a, b) => format!("select({}, {}, {})", expr(c), expr(a), expr(b)),
        LeExpr::GlobalGet(g) => format!("global {g}"),
        LeExpr::TableElem { table, index } => format!("table{table}[{}]", expr(index)),
        LeExpr::ArenaLoad { ty, base, offset } => {
            format!("aload {ty} [{} + {offset}]", expr(base))
        }
        LeExpr::Field {
            layout,
            field,
            base,
        } => format!("getf l{layout}.{field} {}", expr(base)),
        LeExpr::Index {
            layout,
            base,
            index,
        } => format!("geti l{layout} {}[{}]", expr(base), expr(index)),
        LeExpr::ToSlot(ty, e) => format!("to_slot<{ty}>({})", expr(e)),
        LeExpr::FromSlot(ty, e) => format!("fromslot<{ty}>({})", expr(e)),
    }
}

fn unop(op: LeUnOp) -> &'static str {
    use LeUnOp::*;
    match op {
        I32Eqz => "i32.eqz",
        I32WrapI64 => "i32.wrap",
        I64ExtendI32S => "i64.ext_s",
        I64ExtendI32U => "i64.ext_u",
        I32Extend8S => "i32.ext8_s",
        I32Extend8U => "i32.ext8_u",
        I32Extend16S => "i32.ext16_s",
        I32Extend16U => "i32.ext16_u",
        I32TruncF64S => "i32.trunc_s",
        I32TruncF64U => "i32.trunc_u",
        I64TruncF64S => "i64.trunc_s",
        I64TruncF64U => "i64.trunc_u",
        I32TruncSatF64S => "i32.truncsat_s",
        I32TruncSatF64U => "i32.truncsat_u",
        I64TruncSatF64S => "i64.truncsat_s",
        I64TruncSatF64U => "i64.truncsat_u",
        F32DemoteF64 => "f32.demote",
        F64PromoteF32 => "f64.promote",
        F32Abs => "f32.abs",
        F64Abs => "f64.abs",
        F64Ceil => "f64.ceil",
        F64Floor => "f64.floor",
        F64Sqrt => "f64.sqrt",
        F64Trunc => "f64.trunc",
        F64Nearest => "f64.nearest",
        F64Neg => "f64.neg",
        F64ConvertI32S => "f64.cvt_i32s",
        F64ConvertI32U => "f64.cvt_i32u",
        F64ConvertI64S => "f64.cvt_i64s",
        F64ConvertI64U => "f64.cvt_i64u",
        F64ReinterpretI64 => "f64.reinterpret",
        I64ReinterpretF64 => "i64.reinterpret",
        F32ReinterpretI32 => "f32.reinterpret",
        I32ReinterpretF32 => "i32.reinterpret",
    }
}

fn binop(op: LeBinOp) -> &'static str {
    use LeBinOp::*;
    match op {
        I32Add => "i32.add",
        I32Sub => "i32.sub",
        I32Mul => "i32.mul",
        I32DivS => "i32.div_s",
        I32DivU => "i32.div_u",
        I32RemS => "i32.rem_s",
        I32RemU => "i32.rem_u",
        I32And => "i32.and",
        I32Or => "i32.or",
        I32Xor => "i32.xor",
        I32Shl => "i32.shl",
        I32ShrS => "i32.shr_s",
        I32ShrU => "i32.shr_u",
        I32Eq => "i32.eq",
        I32Ne => "i32.ne",
        I32LtS => "i32.lt_s",
        I32LtU => "i32.lt_u",
        I32LeS => "i32.le_s",
        I32LeU => "i32.le_u",
        I32GtS => "i32.gt_s",
        I32GtU => "i32.gt_u",
        I32GeS => "i32.ge_s",
        I32GeU => "i32.ge_u",
        I64Add => "i64.add",
        I64Sub => "i64.sub",
        I64Mul => "i64.mul",
        I64DivS => "i64.div_s",
        I64DivU => "i64.div_u",
        I64RemS => "i64.rem_s",
        I64RemU => "i64.rem_u",
        I64And => "i64.and",
        I64Or => "i64.or",
        I64Xor => "i64.xor",
        I64Shl => "i64.shl",
        I64ShrS => "i64.shr_s",
        I64ShrU => "i64.shr_u",
        I64Eq => "i64.eq",
        I64Ne => "i64.ne",
        I64LtS => "i64.lt_s",
        I64LtU => "i64.lt_u",
        I64LeS => "i64.le_s",
        I64LeU => "i64.le_u",
        I64GtS => "i64.gt_s",
        I64GtU => "i64.gt_u",
        I64GeS => "i64.ge_s",
        I64GeU => "i64.ge_u",
        F64Add => "f64.add",
        F64Sub => "f64.sub",
        F64Mul => "f64.mul",
        F64Div => "f64.div",
        F64Min => "f64.min",
        F64Max => "f64.max",
        F64Eq => "f64.eq",
        F64Ne => "f64.ne",
        F64Lt => "f64.lt",
        F64Le => "f64.le",
        F64Gt => "f64.gt",
        F64Ge => "f64.ge",
        F32Add => "f32.add",
        F32Sub => "f32.sub",
        F32Mul => "f32.mul",
        F32Div => "f32.div",
        F32Eq => "f32.eq",
        F32Ne => "f32.ne",
        F32Lt => "f32.lt",
        F32Le => "f32.le",
        F32Gt => "f32.gt",
        F32Ge => "f32.ge",
    }
}

// ---------------------------------------------------------------------------
// Structural validation
// ---------------------------------------------------------------------------

/// Structural well-formedness checks for a [`LeModule`]: labels resolve,
/// locals are in range, calls reference declared functions, and helper
/// bodies carry no CFG markers. Returns a list of violations (empty = ok).
pub fn validate(module: &LeModule) -> Vec<String> {
    let mut errors = Vec::new();
    let func_count = module.funcs.len() as u32;
    for table in &module.tables {
        for (i, elem) in table.elems.iter().enumerate() {
            if elem.0 >= func_count {
                errors.push(format!(
                    "table {} slot {i} references out-of-range function {}",
                    table.name, elem.0
                ));
            }
        }
    }
    for (fid, func) in module.funcs.iter().enumerate() {
        let local_count = func.locals.len() as u32;
        let mut check_expr = |e: &LeExpr, errors: &mut Vec<String>| {
            let mut stack = vec![e];
            while let Some(e) = stack.pop() {
                match e {
                    LeExpr::Local(l) => {
                        if l.0 >= local_count {
                            errors.push(format!("f{fid}: local v{} out of range", l.0));
                        }
                    }
                    LeExpr::Unary(_, a) => stack.push(a),
                    LeExpr::Binary(_, a, b) => {
                        stack.push(a);
                        stack.push(b);
                    }
                    LeExpr::Select(a, b, c) => {
                        stack.push(a);
                        stack.push(b);
                        stack.push(c);
                    }
                    LeExpr::TableElem { index, .. } => stack.push(index),
                    LeExpr::ArenaLoad { base, .. } => stack.push(base),
                    LeExpr::Field { base, .. } => stack.push(base),
                    LeExpr::Index { base, index, .. } => {
                        stack.push(base);
                        stack.push(index);
                    }
                    LeExpr::ToSlot(_, a) | LeExpr::FromSlot(_, a) => stack.push(a),
                    _ => {}
                }
            }
        };
        let mut check_callee = |c: &LeCallee, errors: &mut Vec<String>| match c {
            LeCallee::Direct(f) => {
                if f.0 >= func_count {
                    errors.push(format!("f{fid}: call to out-of-range function {}", f.0));
                }
            }
            LeCallee::Ptr(e) => check_expr(e, errors),
            LeCallee::Runtime(name) => {
                if !module.funcs.iter().any(|f| f.name == *name) {
                    errors.push(format!("f{fid}: call to undeclared runtime fn {name}"));
                }
            }
        };
        let mut visit = |stmts: &[LeStmt], errors: &mut Vec<String>| {
            let mut stack: Vec<&LeStmt> = stmts.iter().collect();
            while let Some(s) = stack.pop() {
                match s {
                    LeStmt::Assign(l, e) => {
                        if l.0 >= local_count {
                            errors.push(format!("f{fid}: assign to out-of-range v{}", l.0));
                        }
                        check_expr(e, errors);
                    }
                    LeStmt::Call {
                        callee: c,
                        args,
                        results,
                    } => {
                        check_callee(c, errors);
                        for a in args {
                            check_expr(a, errors);
                        }
                        for r in results {
                            if r.0 >= local_count {
                                errors.push(format!("f{fid}: call result v{} out of range", r.0));
                            }
                        }
                    }
                    LeStmt::ArenaStore { base, value, .. } => {
                        check_expr(base, errors);
                        check_expr(value, errors);
                    }
                    LeStmt::SetField { base, value, .. } => {
                        check_expr(base, errors);
                        check_expr(value, errors);
                    }
                    LeStmt::SetIndex {
                        base,
                        index,
                        value,
                        ..
                    } => {
                        check_expr(base, errors);
                        check_expr(index, errors);
                        check_expr(value, errors);
                    }
                    LeStmt::GlobalSet(_, e) => check_expr(e, errors),
                    LeStmt::If { cond, then, else_ } => {
                        check_expr(cond, errors);
                        stack.extend(then.iter());
                        stack.extend(else_.iter());
                    }
                    LeStmt::Loop(body) => stack.extend(body.iter()),
                    LeStmt::Return(values) => {
                        for v in values {
                            check_expr(v, errors);
                        }
                    }
                    LeStmt::RootStore { value, .. } => {
                        if value.0 >= local_count {
                            errors.push(format!("f{fid}: root store of out-of-range v{}", value.0));
                        }
                    }
                    _ => {}
                }
            }
        };
        match &func.body {
            LeBody::Blocks(blocks) => {
                let labels: std::collections::BTreeSet<u32> =
                    blocks.iter().map(|b| b.id.0).collect();
                for block in blocks {
                    visit(&block.body, &mut errors);
                    for stmt in &block.body {
                        // Goto targets must resolve (top level only; nested
                        // structured edges use per-arm Gotos).
                        let mut stack: Vec<&LeStmt> = vec![stmt];
                        while let Some(s) = stack.pop() {
                            match s {
                                LeStmt::Goto(target) => {
                                    if !labels.contains(&target.0) {
                                        errors.push(format!(
                                            "f{fid}: goto unresolved label b{}",
                                            target.0
                                        ));
                                    }
                                }
                                LeStmt::If { then, else_, .. } => {
                                    stack.extend(then.iter());
                                    stack.extend(else_.iter());
                                }
                                LeStmt::Loop(body) => stack.extend(body.iter()),
                                _ => {}
                            }
                        }
                    }
                }
            }
            LeBody::Structured(stmts) => visit(stmts, &mut errors),
            LeBody::Runtime => {}
        }
    }
    errors
}
