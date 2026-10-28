//! The MiniBC renderer (DreamComp #39): `LeModule` → MiniBC byte stream on
//! the stock segment format, `StdExtOp` control flow, and the jsaw host-op
//! family (`0x9B`) for representation state (arena / GC heap / globals /
//! FFI). See `docs/plan-minibc-backend-39.md`.
//!
//! Calling convention: the first `window_runs` R4 runs (reg bases
//! `0, 4, …, 4·(W−1)`) form the call window — arguments in on entry, results
//! out on return; locals allocate at bases `≥ 4W`, so the arg shuffle never
//! clobbers a local. A direct call pushes the locals live after the call,
//! shuffles args into the window, and exits with `Call(func_reg)` (callee
//! entry pc as f64 bits, patched by fixup); the continuation segment pops
//! the saves and moves results out of the window. Indirect calls dispatch
//! through a per-call-site `Table` ext-op into arm shims that load the
//! target's entry pc and fall into a shared `Call` segment. Returns shuffle
//! results into the window and exit with a bare `0xFF` (Halt), which pops
//! the caller's return address (or stops at the top level).

use std::collections::{BTreeMap, BTreeSet};

use dreamcompiler_minibc::interp::StdExtOp;
use dreamcompiler_minibc::ext_op::EncodeExtOp;
use dreamcompiler_minibc::{AluRawOp, AluUnOp, Encode, MiniBc, MiniOp, Reg, RegForm};

use super::{
    LeBinOp, LeBlock, LeCallee, LeDiag, LeExpr, LeFunc, LeLocal, LeModule, LeStmt, LeType, LeUnOp,
};

/// jsaw host-op family table indices (ext-op tag `0x9B`; see the plan doc).
pub mod host_idx {
    pub const NANO_ARENA_ENSURE: u32 = 0; // args: end
    pub const NANO_LOAD: u32 = 1; // args: addr, size_log2 → word
    pub const NANO_STORE: u32 = 2; // args: addr, size_log2, value
    pub const COREGC_ALLOC: u32 = 3; // args: type_id, len → handle
    pub const COREGC_FIELD_READ: u32 = 4; // args: base, layout, field → word
    pub const COREGC_FIELD_WRITE: u32 = 5; // args: base, layout, field, value
    pub const COREGC_INDEX_READ: u32 = 6; // args: base, layout, index → word
    pub const COREGC_INDEX_WRITE: u32 = 7; // args: base, layout, index, value
    pub const COREGC_ROOT_ENTER: u32 = 8; // args: slots
    pub const COREGC_ROOT_STORE: u32 = 9; // args: slot, value
    pub const COREGC_ROOT_POP: u32 = 10;
    pub const COREGC_ARRAY_COPY: u32 = 11; // args: dst, di, src, si, len, elem_log2
    pub const GLOBAL_GET: u32 = 12; // args: index → word
    pub const GLOBAL_SET: u32 = 13; // args: index, value
    pub const FFI_CALL: u32 = 14; // args: ordinal, args... → word
}

/// Base register of window R4 run `i`.
fn window_base(i: u32) -> u8 {
    (i * 4) as u8
}

fn size_log2(ty: LeType) -> u64 {
    match ty {
        LeType::I8 => 0,
        LeType::I16 => 1,
        LeType::I32 | LeType::F32 => 2,
        LeType::I64 | LeType::F64 | LeType::Ref | LeType::Slot => 3,
    }
}

// ── Flat representation ─────────────────────────────────────────────────────

/// A flat operation with logical register ids (LeLocal ids; ephemeral temps
/// start at `func.locals.len()`). Window runs and physical registers appear
/// only at emission; ids are u32 throughout the flat form.
#[derive(Clone, Debug)]
enum FlatOp {
    LoadConst { dst: u32, bits: u64 },
    Move { src: u32, dst: u32 },
    /// Raw-ALU three-register op.
    AluRaw { a: u32, b: u32, dst: u32, sub: AluRawOp },
    /// Raw-ALU unary op.
    AluUn { src: u32, dst: u32, sub: AluUnOp },
    /// A stock three-register op (see `Stock3`).
    Stock3 { kind: Stock3, a: u32, b: u32, dst: u32 },
    /// A stock two-register op (see `Stock2`).
    Stock2 { kind: Stock2, src: u32, dst: u32 },
    /// Move logical reg → call-window run.
    WindowStore { run: u32, src: u32 },
    /// Move call-window run → logical reg.
    WindowLoad { run: u32, dst: u32 },
    /// Load a function's entry pc (f64 bits) into a logical reg — the payload
    /// is patched at fixup time.
    FuncAddrLoad { func: u32, dst: u32 },
    Trap,
}

/// Stock three-register ops usable with raw-word semantics.
#[derive(Clone, Copy, Debug)]
enum Stock3 {
    Add64,
    Sub64,
    Mul64,
    And64,
    Or64,
    Xor64,
    Shl64,
    ShrS64,
    ShrU64,
    FAdd,
    FSub,
    FMul,
    FDiv,
    FMin,
}

/// Stock two-register ops.
#[derive(Clone, Copy, Debug)]
enum Stock2 {
    FFloor,
    FTrunc,
}

/// Jump target: a plan block (resolved at emission via the block map) or a
/// synthetic segment index (already final).
#[derive(Clone, Copy, Debug)]
enum FlatTarget {
    Block(u32),
    Seg(usize),
}

#[derive(Clone, Debug)]
enum FlatExit {
    /// Explicit jump to the physically-next segment.
    Next,
    Jump(FlatTarget),
    /// `Cond`: taken → `if_true`; not taken → physically-next segment.
    Cond { cond: u32, if_true: FlatTarget },
    /// Table dispatch on a funcref word to per-slot arm segments.
    Table {
        index: u32,
        arms: Vec<(u32, FlatTarget)>,
        default: FlatTarget,
    },
    /// jsaw host-op family member (the interp falls through to the
    /// physically-next segment afterwards).
    HostOp {
        idx: u32,
        args: Vec<u32>,
        out: Option<u32>,
    },
    /// Direct or import call. For `Direct`, `func_reg` holds the callee
    /// entry pc as f64 bits (LoadConst64 with a fixup). For imports the call
    /// is lowered as a `HostOp` at flatten time, so this arm is only
    /// `Direct`.
    Call { func_reg: u32 },
    /// Return from the current function (window already loaded).
    Ret,
    /// Deterministic trap (preceded by the `Trap` MiniOp).
    Trap,
}

#[derive(Clone, Debug)]
struct FlatSeg {
    ops: Vec<FlatOp>,
    exit: FlatExit,
}

struct FnFlat {
    segs: Vec<FlatSeg>,
    local_tys: Vec<LeType>,
    diags: Vec<LeDiag>,
    /// (seg idx) per call exit whose saves must be computed post-liveness.
    calls: Vec<usize>,
    /// plan block id → segment index (for `FlatTarget::Block` resolution).
    block_seg: BTreeMap<u32, u32>,
}

struct FnCx<'m> {
    m: &'m LeModule,
    local_tys: Vec<LeType>,
    next_temp: u32,
    segs: Vec<FlatSeg>,
    block_seg: BTreeMap<u32, u32>,
    break_lists: Vec<Vec<usize>>,
    calls: Vec<usize>,
    diags: Vec<LeDiag>,
}

impl<'m> FnCx<'m> {
    fn temp(&mut self, ty: LeType) -> u32 {
        let id = self.next_temp;
        self.next_temp += 1;
        self.local_tys.push(ty);
        id
    }

    fn push(&mut self, op: FlatOp) {
        self.segs.last_mut().expect("open segment").ops.push(op);
    }

    fn mov(&mut self, src: u32, dst: u32) {
        if src != dst {
            self.push(FlatOp::Move { src, dst });
        }
    }

    fn const64(&mut self, bits: u64, ty: LeType) -> u32 {
        let t = self.temp(ty);
        self.push(FlatOp::LoadConst { dst: t, bits });
        t
    }

    /// Close the current segment with `exit`; open a fresh current segment
    /// (physically next) and return the closed segment's index.
    fn close(&mut self, exit: FlatExit) -> usize {
        let idx = self.segs.len() - 1;
        self.segs[idx].exit = exit;
        self.segs.push(FlatSeg {
            ops: Vec::new(),
            exit: FlatExit::Next,
        });
        idx
    }

    /// Emit a host op as the current segment's exit (the interp falls
    /// through to the next segment afterwards).
    fn host(&mut self, idx: u32, args: Vec<u32>, out: Option<u32>) {
        self.close(FlatExit::HostOp { idx, args, out });
    }

    fn diag(&mut self, message: String) {
        self.diags.push(LeDiag {
            function: None,
            block: None,
            inst: None,
            message,
        });
    }
}

// ── op tables ───────────────────────────────────────────────────────────────

fn unop_alu(op: LeUnOp) -> Option<AluUnOp> {
    Some(match op {
        LeUnOp::I32Eqz => AluUnOp::Eqz32,
        LeUnOp::I32WrapI64 => AluUnOp::WrapI64ToI32,
        LeUnOp::I64ExtendI32S => AluUnOp::ExtI32SToI64,
        LeUnOp::I64ExtendI32U => AluUnOp::ExtI32UToI64,
        LeUnOp::I32Extend8S => AluUnOp::Ext8SToI32,
        LeUnOp::I32Extend8U => AluUnOp::Ext8UToI32,
        LeUnOp::I32Extend16S => AluUnOp::Ext16SToI32,
        LeUnOp::I32Extend16U => AluUnOp::Ext16UToI32,
        LeUnOp::I32TruncF64S => AluUnOp::TruncF64ToI32S,
        LeUnOp::I32TruncF64U => AluUnOp::TruncF64ToI32U,
        LeUnOp::I64TruncF64S => AluUnOp::TruncF64ToI64S,
        LeUnOp::I64TruncF64U => AluUnOp::TruncF64ToI64U,
        LeUnOp::I32TruncSatF64S => AluUnOp::TruncSatF64ToI32S,
        LeUnOp::I32TruncSatF64U => AluUnOp::TruncSatF64ToI32U,
        LeUnOp::I64TruncSatF64S => AluUnOp::TruncSatF64ToI64S,
        LeUnOp::I64TruncSatF64U => AluUnOp::TruncSatF64ToI64U,
        LeUnOp::F32DemoteF64 => AluUnOp::F32DemoteF64,
        LeUnOp::F64PromoteF32 => AluUnOp::F64PromoteF32,
        LeUnOp::F32Abs => AluUnOp::F32Abs,
        LeUnOp::F64Abs => AluUnOp::F64Abs,
        LeUnOp::F64Ceil => AluUnOp::F64Ceil,
        LeUnOp::F64Sqrt => AluUnOp::F64Sqrt,
        LeUnOp::F64Nearest => AluUnOp::F64Nearest,
        LeUnOp::F64Neg => AluUnOp::F64Neg,
        LeUnOp::F64ConvertI32S => AluUnOp::ConvertI32SToF64,
        LeUnOp::F64ConvertI32U => AluUnOp::ConvertI32UToF64,
        LeUnOp::F64ConvertI64S => AluUnOp::ConvertI64SToF64,
        LeUnOp::F64ConvertI64U => AluUnOp::ConvertI64UToF64,
        LeUnOp::F64Floor
        | LeUnOp::F64Trunc
        | LeUnOp::F64ReinterpretI64
        | LeUnOp::I64ReinterpretF64
        | LeUnOp::F32ReinterpretI32
        | LeUnOp::I32ReinterpretF32 => return None,
    })
}

fn binop_alu(op: LeBinOp) -> Option<AluRawOp> {
    use LeBinOp::*;
    Some(match op {
        I32Add => AluRawOp::Add32,
        I32Sub => AluRawOp::Sub32,
        I32Mul => AluRawOp::Mul32,
        I32DivS => AluRawOp::Div32S,
        I32DivU => AluRawOp::Div32U,
        I32RemS => AluRawOp::Rem32S,
        I32RemU => AluRawOp::Rem32U,
        I32And => AluRawOp::And32,
        I32Or => AluRawOp::Or32,
        I32Xor => AluRawOp::Xor32,
        I32Shl => AluRawOp::Shl32,
        I32ShrS => AluRawOp::Shr32S,
        I32ShrU => AluRawOp::Shr32U,
        I32Eq => AluRawOp::Eq32,
        I32Ne => AluRawOp::Ne32,
        I32LtS => AluRawOp::Lt32S,
        I32LtU => AluRawOp::Lt32U,
        I32LeS => AluRawOp::Le32S,
        I32LeU => AluRawOp::Le32U,
        I32GtS => AluRawOp::Gt32S,
        I32GtU => AluRawOp::Gt32U,
        I32GeS => AluRawOp::Ge32S,
        I32GeU => AluRawOp::Ge32U,
        I64DivS => AluRawOp::Div64S,
        I64DivU => AluRawOp::Div64U,
        I64RemS => AluRawOp::Rem64S,
        I64RemU => AluRawOp::Rem64U,
        I64Eq => AluRawOp::Eq64,
        I64Ne => AluRawOp::Ne64,
        I64LtS => AluRawOp::Lt64S,
        I64LtU => AluRawOp::Lt64U,
        I64LeS => AluRawOp::Le64S,
        I64LeU => AluRawOp::Le64U,
        I64GtS => AluRawOp::Gt64S,
        I64GtU => AluRawOp::Gt64U,
        I64GeS => AluRawOp::Ge64S,
        I64GeU => AluRawOp::Ge64U,
        F32Add => AluRawOp::F32Add,
        F32Sub => AluRawOp::F32Sub,
        F32Mul => AluRawOp::F32Mul,
        F32Div => AluRawOp::F32Div,
        F32Eq => AluRawOp::F32Eq,
        F32Ne => AluRawOp::F32Ne,
        F32Lt => AluRawOp::F32Lt,
        F32Le => AluRawOp::F32Le,
        F32Gt => AluRawOp::F32Gt,
        F32Ge => AluRawOp::F32Ge,
        F64Max => AluRawOp::F64Max,
        F64Eq => AluRawOp::F64Eq,
        F64Ne => AluRawOp::F64Ne,
        F64Lt => AluRawOp::F64Lt,
        F64Le => AluRawOp::F64Le,
        F64Gt => AluRawOp::F64Gt,
        F64Ge => AluRawOp::F64Ge,
        I64Add | I64Sub | I64Mul | I64And | I64Or | I64Xor | I64Shl | I64ShrS | I64ShrU
        | F64Add | F64Sub | F64Mul | F64Div | F64Min => return None,
    })
}

fn unop_result_ty(op: LeUnOp) -> LeType {
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
        _ => LeType::F64,
    }
}

fn binop_result_ty(op: LeBinOp) -> LeType {
    use LeBinOp::*;
    match op {
        I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
        | I32Xor | I32Shl | I32ShrS | I32ShrU => LeType::I32,
        I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
        | I64Xor | I64Shl | I64ShrS | I64ShrU => LeType::I64,
        F32Add | F32Sub | F32Mul | F32Div => LeType::F32,
        F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max => LeType::F64,
        _ => LeType::I32,
    }
}

// ── expression lowering ─────────────────────────────────────────────────────

fn addr_plus(cx: &mut FnCx, base: u32, offset: u32) -> u32 {
    if offset == 0 {
        return base;
    }
    let off = cx.const64(offset as u64, LeType::I64);
    let t = cx.temp(LeType::I64);
    cx.push(FlatOp::Stock3 {
        kind: Stock3::Add64,
        a: base,
        b: off,
        dst: t,
    });
    t
}

fn lower_expr(cx: &mut FnCx, e: &LeExpr) -> u32 {
    match e {
        LeExpr::Local(l) => l.0,
        LeExpr::ConstI32(v) => cx.const64(*v as u64, LeType::I32),
        LeExpr::ConstI64(v) => cx.const64(*v, LeType::I64),
        LeExpr::ConstF32(bits) => cx.const64(*bits as u64, LeType::F32),
        LeExpr::ConstF64(bits) => cx.const64(*bits, LeType::F64),
        LeExpr::Unary(op, x) => {
            let src = lower_expr(cx, x);
            let dst = cx.temp(unop_result_ty(*op));
            if let Some(sub) = unop_alu(*op) {
                cx.push(FlatOp::AluUn { src, dst, sub });
            } else {
                match op {
                    LeUnOp::F64Floor => cx.push(FlatOp::Stock2 { kind: Stock2::FFloor, src, dst }),
                    LeUnOp::F64Trunc => cx.push(FlatOp::Stock2 { kind: Stock2::FTrunc, src, dst }),
                    // Reinterpretations are verbatim R4 moves.
                    LeUnOp::F64ReinterpretI64
                    | LeUnOp::I64ReinterpretF64
                    | LeUnOp::F32ReinterpretI32
                    | LeUnOp::I32ReinterpretF32 => cx.mov(src, dst),
                    other => {
                        cx.diag(format!("minibc: unop {other:?} has no lowering"));
                    }
                }
            }
            dst
        }
        LeExpr::Binary(op, a, b) => {
            let ra = lower_expr(cx, a);
            let rb = lower_expr(cx, b);
            let dst = cx.temp(binop_result_ty(*op));
            if let Some(sub) = binop_alu(*op) {
                cx.push(FlatOp::AluRaw {
                    a: ra,
                    b: rb,
                    dst,
                    sub,
                });
            } else {
                let kind = match op {
                    LeBinOp::I64Add => Stock3::Add64,
                    LeBinOp::I64Sub => Stock3::Sub64,
                    LeBinOp::I64Mul => Stock3::Mul64,
                    LeBinOp::I64And => Stock3::And64,
                    LeBinOp::I64Or => Stock3::Or64,
                    LeBinOp::I64Xor => Stock3::Xor64,
                    LeBinOp::I64Shl => Stock3::Shl64,
                    LeBinOp::I64ShrS => Stock3::ShrS64,
                    LeBinOp::I64ShrU => Stock3::ShrU64,
                    // Stock f64 ops canonicalize NaN payloads — a legal wasm
                    // NaN outcome.
                    LeBinOp::F64Add => Stock3::FAdd,
                    LeBinOp::F64Sub => Stock3::FSub,
                    LeBinOp::F64Mul => Stock3::FMul,
                    LeBinOp::F64Div => Stock3::FDiv,
                    LeBinOp::F64Min => Stock3::FMin,
                    other => {
                        cx.diag(format!("minibc: binop {other:?} has no lowering"));
                        return dst;
                    }
                };
                cx.push(FlatOp::Stock3 {
                    kind,
                    a: ra,
                    b: rb,
                    dst,
                });
            }
            dst
        }
        LeExpr::Select(cond, if_true, if_false) => {
            // Diamond: [pre] Cond(c) falls through to ELSE (physically next),
            // jumps to THEN; both arms move into `out` and jump to DONE.
            let c = lower_expr(cx, cond);
            let out_ty = expr_ty(cx, if_true);
            let out = cx.temp(out_ty);
            let pre = cx.close(FlatExit::Next); // exit rewritten below
            let else_seg = cx.segs.len() - 1;
            let f = lower_expr(cx, if_false);
            cx.mov(f, out);
            cx.close(FlatExit::Next);
            let then_seg = cx.segs.len() - 1;
            let t = lower_expr(cx, if_true);
            cx.mov(t, out);
            cx.close(FlatExit::Next);
            let done_seg = cx.segs.len() - 1;
            cx.segs[pre].exit = FlatExit::Cond {
                cond: c,
                if_true: FlatTarget::Seg(then_seg),
            };
            cx.segs[else_seg].exit = FlatExit::Jump(FlatTarget::Seg(done_seg));
            cx.segs[then_seg].exit = FlatExit::Jump(FlatTarget::Seg(done_seg));
            out
        }
        LeExpr::GlobalGet(g) => {
            let idx = cx.const64(*g as u64, LeType::I32);
            let dst = cx.temp(cx.m.globals[*g as usize].0);
            cx.host(host_idx::GLOBAL_GET, vec![idx], Some(dst));
            dst
        }
        LeExpr::TableElem { .. } => {
            cx.diag("minibc: TableElem outside a call target is not supported".into());
            cx.const64(0, LeType::I64)
        }
        LeExpr::ArenaLoad { ty, base, offset } => {
            let b = lower_expr(cx, base);
            let addr = addr_plus(cx, b, *offset);
            let sz = cx.const64(size_log2(*ty), LeType::I32);
            let dst = cx.temp(*ty);
            cx.host(host_idx::NANO_LOAD, vec![addr, sz], Some(dst));
            dst
        }
        LeExpr::Field {
            layout,
            field,
            base,
        } => {
            let b = lower_expr(cx, base);
            let lay = cx.const64(*layout as u64, LeType::I32);
            let fld = cx.const64(*field as u64, LeType::I32);
            let dst = cx.temp(if *layout == 0 {
                LeType::I32
            } else {
                cx.m.layouts[*layout as usize].fields[*field as usize]
            });
            cx.host(host_idx::COREGC_FIELD_READ, vec![b, lay, fld], Some(dst));
            dst
        }
        LeExpr::Index {
            layout,
            base,
            index,
        } => {
            let b = lower_expr(cx, base);
            let lay = cx.const64(*layout as u64, LeType::I32);
            let i = lower_expr(cx, index);
            let dst = cx.temp(
                cx.m.layouts[*layout as usize]
                    .array_element
                    .expect("array layout"),
            );
            cx.host(host_idx::COREGC_INDEX_READ, vec![b, lay, i], Some(dst));
            dst
        }
        LeExpr::ToSlot(_, x) => {
            // Word erasure: verbatim R4 move (all jsaw words are 64-bit).
            let src = lower_expr(cx, x);
            let dst = cx.temp(LeType::Slot);
            cx.mov(src, dst);
            dst
        }
        LeExpr::FromSlot(ty, x) => {
            let src = lower_expr(cx, x);
            let dst = cx.temp(*ty);
            cx.mov(src, dst);
            dst
        }
    }
}

/// The static type of an expression (locals from the function's table).
fn expr_ty(cx: &FnCx, e: &LeExpr) -> LeType {
    match e {
        LeExpr::Local(l) => cx.local_tys[l.0 as usize],
        LeExpr::ConstI32(_) => LeType::I32,
        LeExpr::ConstI64(_) => LeType::I64,
        LeExpr::ConstF32(_) => LeType::F32,
        LeExpr::ConstF64(_) => LeType::F64,
        LeExpr::Unary(op, _) => unop_result_ty(*op),
        LeExpr::Binary(op, _, _) => binop_result_ty(*op),
        LeExpr::Select(_, t, _) => expr_ty(cx, t),
        LeExpr::GlobalGet(g) => cx.m.globals[*g as usize].0,
        LeExpr::TableElem { .. } => LeType::I64,
        LeExpr::ArenaLoad { ty, .. } => *ty,
        LeExpr::Field { layout, field, .. } => {
            if *layout == 0 {
                LeType::I32
            } else {
                cx.m.layouts[*layout as usize].fields[*field as usize]
            }
        }
        LeExpr::Index { layout, .. } => cx.m.layouts[*layout as usize]
            .array_element
            .expect("array layout"),
        LeExpr::ToSlot(..) => LeType::Slot,
        LeExpr::FromSlot(ty, _) => *ty,
    }
}

// ── statement lowering ──────────────────────────────────────────────────────

fn lower_stmt(cx: &mut FnCx, s: &LeStmt) {
    match s {
        LeStmt::Assign(l, e) => {
            let src = lower_expr(cx, e);
            cx.mov(src, l.0);
        }
        LeStmt::Call {
            callee,
            args,
            results,
        } => {
            let arg_regs: Vec<u32> = args.iter().map(|a| lower_expr(cx, a)).collect();
            let result_regs: Vec<u32> = results.iter().map(|r| r.0).collect();
            lower_call(cx, callee, arg_regs, result_regs);
        }
        LeStmt::ArenaStore {
            ty,
            base,
            offset,
            value,
        } => {
            let b = lower_expr(cx, base);
            let addr = addr_plus(cx, b, *offset);
            let sz = cx.const64(size_log2(*ty), LeType::I32);
            let v = lower_expr(cx, value);
            cx.host(host_idx::NANO_STORE, vec![addr, sz, v], None);
        }
        LeStmt::SetField {
            layout,
            field,
            base,
            value,
        } => {
            let b = lower_expr(cx, base);
            let lay = cx.const64(*layout as u64, LeType::I32);
            let fld = cx.const64(*field as u64, LeType::I32);
            let v = lower_expr(cx, value);
            cx.host(host_idx::COREGC_FIELD_WRITE, vec![b, lay, fld, v], None);
        }
        LeStmt::SetIndex {
            layout,
            base,
            index,
            value,
        } => {
            let b = lower_expr(cx, base);
            let lay = cx.const64(*layout as u64, LeType::I32);
            let i = lower_expr(cx, index);
            let v = lower_expr(cx, value);
            cx.host(host_idx::COREGC_INDEX_WRITE, vec![b, lay, i, v], None);
        }
        LeStmt::GlobalSet(g, e) => {
            let idx = cx.const64(*g as u64, LeType::I32);
            let v = lower_expr(cx, e);
            cx.host(host_idx::GLOBAL_SET, vec![idx, v], None);
        }
        LeStmt::If { cond, then, else_ } => {
            let c = lower_expr(cx, cond);
            let pre = cx.close(FlatExit::Next);
            // The else arm is physically next (Cond's fallthrough).
            let else_seg = cx.segs.len() - 1;
            for s in else_ {
                lower_stmt(cx, s);
            }
            cx.close(FlatExit::Next);
            let then_seg = cx.segs.len() - 1;
            for s in then {
                lower_stmt(cx, s);
            }
            cx.close(FlatExit::Next);
            let done_seg = cx.segs.len() - 1;
            cx.segs[pre].exit = FlatExit::Cond {
                cond: c,
                if_true: FlatTarget::Seg(then_seg),
            };
            cx.segs[else_seg].exit = FlatExit::Jump(FlatTarget::Seg(done_seg));
            cx.segs[then_seg].exit = FlatExit::Jump(FlatTarget::Seg(done_seg));
        }
        LeStmt::Loop(body) => {
            cx.close(FlatExit::Next);
            let top_seg = cx.segs.len() - 1;
            cx.break_lists.push(Vec::new());
            for s in body {
                lower_stmt(cx, s);
            }
            cx.close(FlatExit::Jump(FlatTarget::Seg(top_seg)));
            let after = cx.segs.len() - 1;
            for b in cx.break_lists.pop().unwrap_or_default() {
                cx.segs[b].exit = FlatExit::Jump(FlatTarget::Seg(after));
            }
        }
        LeStmt::Break => {
            if cx.break_lists.is_empty() {
                cx.diag("minibc: Break outside a Loop".into());
            } else {
                let seg = cx.close(FlatExit::Jump(FlatTarget::Seg(0)));
                cx.break_lists.last_mut().expect("checked").push(seg);
            }
        }
        LeStmt::Return(values) => {
            for (i, v) in values.iter().enumerate() {
                let src = lower_expr(cx, v);
                cx.push(FlatOp::WindowStore {
                    run: i as u32,
                    src,
                });
            }
            cx.close(FlatExit::Ret);
        }
        LeStmt::Trap | LeStmt::Unreachable => {
            cx.push(FlatOp::Trap);
            cx.close(FlatExit::Trap);
        }
        LeStmt::Goto(id) => {
            cx.close(FlatExit::Jump(FlatTarget::Block(id.0)));
        }
        LeStmt::Label(_) => {} // structural; block boundaries handle labels
        LeStmt::RootFrameEnter { slots } => {
            let n = cx.const64(*slots as u64, LeType::I32);
            cx.host(host_idx::COREGC_ROOT_ENTER, vec![n], None);
        }
        LeStmt::RootStore { slot, value } => {
            let n = cx.const64(*slot as u64, LeType::I32);
            cx.host(host_idx::COREGC_ROOT_STORE, vec![n, value.0], None);
        }
        LeStmt::RootFramePop => {
            cx.host(host_idx::COREGC_ROOT_POP, vec![], None);
        }
    }
}

// ── calls ───────────────────────────────────────────────────────────────────

fn lower_call(cx: &mut FnCx, callee: &LeCallee, args: Vec<u32>, results: Vec<u32>) {
    if results.len() > 1 {
        cx.diag("minibc: multi-result call".into());
        return;
    }
    match callee {
        LeCallee::Runtime(name) => {
            let idx = match name.as_str() {
                "js_nano_arena_ensure" => host_idx::NANO_ARENA_ENSURE,
                "js_coregc_alloc" => host_idx::COREGC_ALLOC,
                "js_coregc_root_enter" => host_idx::COREGC_ROOT_ENTER,
                "js_coregc_root_store" => host_idx::COREGC_ROOT_STORE,
                "js_coregc_root_pop" => host_idx::COREGC_ROOT_POP,
                "js_coregc_array_copy" => host_idx::COREGC_ARRAY_COPY,
                other => {
                    cx.diag(format!("minibc: unknown runtime entry point {other:?}"));
                    return;
                }
            };
            cx.host(idx, args, results.first().copied());
        }
        LeCallee::Direct(id) => {
            // Import call: no control transfer — an FFI host op.
            if cx.m.imports.iter().any(|i| i.func.0 == id.0) {
                let ordinal = cx
                    .m
                    .imports
                    .iter()
                    .position(|i| i.func.0 == id.0)
                    .expect("import") as u64;
                let ord = cx.const64(ordinal, LeType::I32);
                let mut call_args = vec![ord];
                call_args.extend(args);
                cx.host(host_idx::FFI_CALL, call_args, results.first().copied());
                return;
            }
            let freg = cx.temp(LeType::I64);
            cx.push(FlatOp::FuncAddrLoad { func: id.0, dst: freg });
            // Shuffle args into the window.
            for (i, a) in args.iter().enumerate() {
                cx.push(FlatOp::WindowStore { run: i as u32, src: *a });
            }
            let seg = cx.close(FlatExit::Call { func_reg: freg });
            cx.calls.push(seg);
            // Continuation: pop saves (emitted post-liveness) and move
            // results out of the window.
            for (i, &r) in results.iter().enumerate() {
                cx.push(FlatOp::WindowLoad { run: i as u32, dst: r });
            }
        }
        LeCallee::Ptr(target) => {
            let LeExpr::TableElem { table, index } = &**target else {
                cx.diag("minibc: non-table indirect call".into());
                return;
            };
            let table = &cx.m.tables[*table as usize];
            let idx = lower_expr(cx, index);
            for (i, a) in args.iter().enumerate() {
                cx.push(FlatOp::WindowStore { run: i as u32, src: *a });
            }
            let freg = cx.temp(LeType::I64);
            // Table dispatch into per-slot arm shims that load the entry pc
            // and fall into the shared call segment.
            let pre = cx.close(FlatExit::Next); // rewritten to Table below
            let mut arm_segs = Vec::new();
            for (slot, elem) in table.elems.iter().enumerate() {
                let arm = cx.close(FlatExit::Next); // arm segment
                cx.push(FlatOp::FuncAddrLoad { func: elem.0, dst: freg });
                arm_segs.push((slot as u32, FlatTarget::Seg(arm)));
                // arm exit patched to the call segment below
            }
            let call_seg = cx.segs.len() - 1;
            for (_, arm) in &arm_segs {
                let FlatTarget::Seg(arm) = *arm else {
                    unreachable!()
                };
                cx.segs[arm].exit = FlatExit::Jump(FlatTarget::Seg(call_seg));
            }
            // The call segment: the Call exit.
            let call_idx = cx.close(FlatExit::Call { func_reg: freg });
            cx.calls.push(call_idx);
            let cont_seg = cx.segs.len() - 1;
            cx.segs[pre].exit = FlatExit::Table {
                index: idx,
                arms: arm_segs,
                default: FlatTarget::Seg(call_seg), // slot 0 is the trap stub
            };
            let _ = cont_seg;
            for (i, &r) in results.iter().enumerate() {
                cx.push(FlatOp::WindowLoad { run: i as u32, dst: r });
            }
        }
    }
}

// ── function flattening ─────────────────────────────────────────────────────

fn flatten_function(m: &LeModule, func: &LeFunc) -> FnFlat {
    let mut cx = FnCx {
        m,
        local_tys: func.locals.iter().map(|(_, t)| *t).collect(),
        next_temp: func.locals.len() as u32,
        segs: vec![FlatSeg {
            ops: Vec::new(),
            exit: FlatExit::Next,
        }],
        block_seg: BTreeMap::new(),
        break_lists: Vec::new(),
        calls: Vec::new(),
        diags: Vec::new(),
    };
    match &func.body {
        super::LeBody::Blocks(blocks) => {
            // Entry: move window params into their locals (plain functions;
            // step functions read their frame instead — their block params
            // are unpacked by skeleton-emitted statements already).
            for (i, (l, _)) in func.params.iter().enumerate() {
                cx.push(FlatOp::WindowLoad {
                    run: i as u32,
                    dst: l.0,
                });
            }
            for block in blocks {
                cx.block_seg.insert(block.id.0, cx.segs.len() as u32 - 1);
                for s in &block.body {
                    lower_stmt(&mut cx, s);
                }
            }
        }
        super::LeBody::Structured(stmts) => {
            for (i, (l, _)) in func.params.iter().enumerate() {
                cx.push(FlatOp::WindowLoad {
                    run: i as u32,
                    dst: l.0,
                });
            }
            for s in stmts {
                lower_stmt(&mut cx, s);
            }
        }
        super::LeBody::Runtime => {}
    }
    // The trailing segment is unreachable; make it trap rather than fall off
    // the function's end.
    let last = cx.segs.len() - 1;
    cx.segs[last].ops.push(FlatOp::Trap);
    cx.segs[last].exit = FlatExit::Trap;
    FnFlat {
        segs: cx.segs,
        local_tys: cx.local_tys,
        diags: cx.diags,
        calls: cx.calls,
        block_seg: cx.block_seg,
    }
}

// ── liveness + register allocation ──────────────────────────────────────────

/// Successor segments of an exit (block targets resolved via `block_seg`).
fn exit_succs(exit: &FlatExit, block_seg: &BTreeMap<u32, u32>) -> Vec<u32> {
    let resolve = |t: &FlatTarget| match t {
        FlatTarget::Block(b) => *block_seg.get(b).unwrap_or(&0),
        FlatTarget::Seg(s) => *s as u32,
    };
    match exit {
        FlatExit::Next => vec![],
        FlatExit::Jump(t) => vec![resolve(t)],
        FlatExit::Cond { if_true, .. } => vec![resolve(if_true)],
        FlatExit::Table { arms, default, .. } => arms
            .iter()
            .map(|(_, s)| resolve(s))
            .chain([resolve(default)])
            .collect(),
        FlatExit::HostOp { .. } | FlatExit::Call { .. } => vec![],
        FlatExit::Ret | FlatExit::Trap => vec![],
    }
}

/// Uses/defs of a flat op (logical ids only; window runs are physical).
fn op_uses_defs(op: &FlatOp) -> (Vec<u32>, Vec<u32>) {
    match op {
        FlatOp::LoadConst { dst, .. } => (vec![], vec![*dst]),
        FlatOp::Move { src, dst } => (vec![*src], vec![*dst]),
        FlatOp::AluRaw { a, b, dst, .. } => (vec![*a, *b], vec![*dst]),
        FlatOp::AluUn { src, dst, .. } => (vec![*src], vec![*dst]),
        FlatOp::Stock3 { a, b, dst, .. } => (vec![*a, *b], vec![*dst]),
        FlatOp::Stock2 { src, dst, .. } => (vec![*src], vec![*dst]),
        FlatOp::WindowStore { src, .. } => (vec![*src], vec![]),
        FlatOp::WindowLoad { dst, .. } => (vec![], vec![*dst]),
        FlatOp::FuncAddrLoad { dst, .. } => (vec![], vec![*dst]),
        FlatOp::Trap => (vec![], vec![]),
    }
}

fn exit_uses(exit: &FlatExit) -> Vec<u32> {
    match exit {
        FlatExit::Cond { cond, .. } => vec![*cond],
        FlatExit::Table { index, .. } => vec![*index],
        FlatExit::Call { func_reg } => vec![*func_reg],
        FlatExit::HostOp { args, .. } => args.clone(),
        _ => vec![],
    }
}

fn exit_defs(exit: &FlatExit) -> Vec<u32> {
    match exit {
        FlatExit::HostOp { out, .. } => out.iter().copied().collect(),
        _ => vec![],
    }
}

// ── emission ────────────────────────────────────────────────────────────────

/// Liveness results reused for call saves.
struct Liveness {
    live_out: Vec<BTreeSet<u32>>,
}

/// The allocation result for one function.
struct Alloc {
    base_of: BTreeMap<u32, u32>,
    liveness: Liveness,
    max_base: u32,
}

fn liveness(flat: &FnFlat) -> (Liveness, BTreeMap<u32, ((u32, u32), (u32, u32))>) {
    let n = flat.segs.len();

    let mut use_set: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];
    let mut def_set: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];
    let mut first_pos: BTreeMap<u32, (u32, u32)> = BTreeMap::new();
    let mut last_pos: BTreeMap<u32, (u32, u32)> = BTreeMap::new();
    for (i, seg) in flat.segs.iter().enumerate() {
        for (j, op) in seg.ops.iter().enumerate() {
            let (uses, defs) = op_uses_defs(op);
            for u in uses {
                if !def_set[i].contains(&u) {
                    use_set[i].insert(u);
                }
                last_pos.insert(u, (i as u32, j as u32));
            }
            for d in defs {
                def_set[i].insert(d);
                first_pos.entry(d).or_insert((i as u32, j as u32));
                last_pos.insert(d, (i as u32, j as u32));
            }
        }
        for u in exit_uses(&seg.exit) {
            if !def_set[i].contains(&u) {
                use_set[i].insert(u);
            }
            last_pos.insert(u, (i as u32, seg.ops.len() as u32));
        }
        for d in exit_defs(&seg.exit) {
            def_set[i].insert(d);
            first_pos.entry(d).or_insert((i as u32, seg.ops.len() as u32));
            last_pos.insert(d, (i as u32, seg.ops.len() as u32));
        }
    }

    let mut live_in: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];
    let mut live_out: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); n];
    loop {
        let mut changed = false;
        for i in (0..n).rev() {
            let mut out_set: BTreeSet<u32> = BTreeSet::new();
            let falls_through = !matches!(
                flat.segs[i].exit,
                FlatExit::Ret | FlatExit::Trap | FlatExit::Jump(_) | FlatExit::Table { .. }
            );
            let mut succs = exit_succs(&flat.segs[i].exit, &flat.block_seg);
            if falls_through && i + 1 < n {
                succs.push(i as u32 + 1);
            }
            for s in succs {
                let s = s as usize;
                if s < n {
                    out_set.extend(live_in[s].iter().copied());
                }
            }
            let in_set: BTreeSet<u32> = use_set[i]
                .iter()
                .copied()
                .chain(out_set.iter().copied().filter(|v| !def_set[i].contains(v)))
                .collect();
            if out_set != live_out[i] || in_set != live_in[i] {
                live_out[i] = out_set;
                live_in[i] = in_set;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let mut intervals: BTreeMap<u32, ((u32, u32), (u32, u32))> = BTreeMap::new();
    let locals: BTreeSet<u32> = def_set
        .iter()
        .flat_map(|s| s.iter().copied())
        .chain(use_set.iter().flat_map(|s| s.iter().copied()))
        .collect();
    for l in locals {
        let start = first_pos.get(&l).copied().unwrap_or((0, 0));
        let end = last_pos.get(&l).copied().unwrap_or(start);
        intervals.insert(l, (start, end));
    }
    // Extend intervals across live segment boundaries (iterate segments once,
    // not locals × segments).
    for i in 0..n {
        let end_pos = flat.segs[i].ops.len() as u32;
        for &l in live_in[i].iter().chain(live_out[i].iter()) {
            if let Some(iv) = intervals.get_mut(&l) {
                iv.1 = iv.1.max((i as u32, end_pos));
            }
        }
    }
    (Liveness { live_out }, intervals)
}

/// Linear-scan allocation over the intervals. The window zone
/// (`[0, window_runs*4)`) is reserved for calls.
fn allocate(flat: &FnFlat, window_runs: u32) -> Result<Alloc, LeDiag> {
    let (live, intervals) = liveness(flat);
    let window_end = window_runs * 4;
    let mut sorted: Vec<(u32, (u32, u32), (u32, u32))> =
        intervals.iter().map(|(&l, &(s, e))| (l, s, e)).collect();
    sorted.sort_by_key(|&(_, s, _)| s);
    let mut active: Vec<((u32, u32), u32)> = Vec::new();
    let mut base_of: BTreeMap<u32, u32> = BTreeMap::new();
    let mut max_base = window_end;
    for (l, start, end) in sorted {
        active.retain(|&(e, _)| e > start);
        let mut base = window_end;
        while active.iter().any(|&(_, b)| b == base) {
            base += 4;
        }
        if base > 252 {
            return Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: "minibc: function exceeds the 64-run register file".into(),
            });
        }
        active.push((end, base));
        base_of.insert(l, base);
        max_base = max_base.max(base + 4);
    }
    Ok(Alloc {
        base_of,
        liveness: live,
        max_base,
    })
}

/// A fixup to patch after all segments are positioned.
#[derive(Clone, Debug)]
enum Fixup {
    /// Write the u32 LE segment start pc at this byte position.
    SegPc {
        at: usize,
        func: u32,
        target: FlatTarget,
    },
    /// Write the f64 bits of a function's entry pc into a LoadConst64 payload.
    FuncAddr { at: usize, func: u32 },
    /// Write the module terminator pc (buffer end).
    End { at: usize },
}

/// The emitted MiniBC artifact plus the metadata the host needs.
pub struct MinibcArtifact {
    pub bytes: Vec<u8>,
    /// Export name → entry pc.
    pub exports: BTreeMap<String, u32>,
    pub start: Option<u32>,
    /// Interp store size in physical 16-bit registers.
    pub store_regs: usize,
    pub window_runs: u32,
    /// FFI imports in ordinal order (`FFI_CALL` arg 0).
    pub imports: Vec<(String, String)>,
    pub lane: String,
    /// Debug: (function name, entry pc) for defined functions.
    pub func_entries: Vec<(String, u32)>,
}

/// Map a logical id to its physical run base (window runs stay physical).
fn phys(base_of: &BTreeMap<u32, u32>, id: u32) -> u8 {
    *base_of.get(&id).unwrap_or(&id) as u8
}

fn encode_mini(op: MiniOp, out: &mut Vec<u8>) {
    op.encode(out.len() as u32, &mut |b| out.push(b));
}

fn emit_op(
    op: &FlatOp,
    base_of: &BTreeMap<u32, u32>,
    out: &mut Vec<u8>,
    fixups: &mut Vec<Fixup>,
) {
    match op {
        FlatOp::LoadConst { dst, bits } => {
            encode_mini(MiniOp::LoadConst64(Reg(phys(base_of, *dst)), bits.to_le_bytes()), out);
        }
        FlatOp::Move { src, dst } => encode_mini(
            MiniOp::Move(
                Reg(phys(base_of, *src)),
                Reg(phys(base_of, *dst)),
                RegForm::R4,
            ),
            out,
        ),
        FlatOp::AluRaw { a, b, dst, sub } => encode_mini(
            MiniOp::AluRaw(
                Reg(phys(base_of, *a)),
                Reg(phys(base_of, *b)),
                Reg(phys(base_of, *dst)),
                *sub,
            ),
            out,
        ),
        FlatOp::AluUn { src, dst, sub } => encode_mini(
            MiniOp::AluUn(Reg(phys(base_of, *src)), Reg(phys(base_of, *dst)), *sub),
            out,
        ),
        FlatOp::Stock3 { kind, a, b, dst } => {
            let (a, b, dst) = (
                Reg(phys(base_of, *a)),
                Reg(phys(base_of, *b)),
                Reg(phys(base_of, *dst)),
            );
            let op = match kind {
                Stock3::Add64 => MiniOp::Add64(a, b, dst),
                Stock3::Sub64 => MiniOp::Subtract64(a, b, dst),
                Stock3::Mul64 => MiniOp::Multiply64(a, b, dst),
                Stock3::And64 => MiniOp::BitwiseAnd64(a, b, dst),
                Stock3::Or64 => MiniOp::BitwiseOr64(a, b, dst),
                Stock3::Xor64 => MiniOp::BitwiseXor64(a, b, dst),
                Stock3::Shl64 => MiniOp::ShiftLeft64(a, b, dst),
                Stock3::ShrS64 => MiniOp::ShiftRight64(a, b, dst),
                Stock3::ShrU64 => MiniOp::ShiftRightUnsigned64(a, b, dst),
                Stock3::FAdd => MiniOp::FAdd(a, b, dst),
                Stock3::FSub => MiniOp::FSubtract(a, b, dst),
                Stock3::FMul => MiniOp::FMultiply(a, b, dst),
                Stock3::FDiv => MiniOp::FDivide(a, b, dst),
                Stock3::FMin => MiniOp::FMin(a, b, dst),
            };
            encode_mini(op, out);
        }
        FlatOp::Stock2 { kind, src, dst } => {
            let (s, d) = (Reg(phys(base_of, *src)), Reg(phys(base_of, *dst)));
            encode_mini(
                match kind {
                    Stock2::FFloor => MiniOp::FFloor(s, d),
                    Stock2::FTrunc => MiniOp::FTrunc(s, d),
                },
                out,
            );
        }
        FlatOp::WindowStore { run, src } => encode_mini(
            MiniOp::Move(
                Reg(phys(base_of, *src)),
                Reg(window_base(*run)),
                RegForm::R4,
            ),
            out,
        ),
        FlatOp::WindowLoad { run, dst } => encode_mini(
            MiniOp::Move(
                Reg(window_base(*run)),
                Reg(phys(base_of, *dst)),
                RegForm::R4,
            ),
            out,
        ),
        FlatOp::FuncAddrLoad { func, dst } => {
            let at = out.len() + 2; // opcode + reg, then the 8-byte payload
            fixups.push(Fixup::FuncAddr { at, func: *func });
            encode_mini(
                MiniOp::LoadConst64(Reg(phys(base_of, *dst)), [0; 8]),
                out,
            );
        }
        FlatOp::Trap => encode_mini(MiniOp::Trap, out),
    }
}

fn emit_exit(
    exit: &FlatExit,
    func_idx: u32,
    seg_idx: u32,
    base_of: &BTreeMap<u32, u32>,
    out: &mut Vec<u8>,
    fixups: &mut Vec<Fixup>,
) {
    out.push(0xFF);
    match exit {
        FlatExit::Next | FlatExit::Jump(_) => {
            let target = match exit {
                FlatExit::Jump(t) => *t,
                FlatExit::Next => FlatTarget::Seg(seg_idx as usize + 1),
                _ => unreachable!(),
            };
            let at = out.len() + 1; // Jump's target field follows the tag
            fixups.push(Fixup::SegPc {
                at,
                func: func_idx,
                target,
            });
            StdExtOp::encode_ext_op(&StdExtOp::Jump { target: 0 }, out.len() as u32, &mut |b| out.push(b));
        }
        FlatExit::Cond { cond, if_true } => {
            let at = out.len() + 2; // tag + cond_reg, then the target field
            fixups.push(Fixup::SegPc {
                at,
                func: func_idx,
                target: *if_true,
            });
            StdExtOp::encode_ext_op(&StdExtOp::Cond {
                cond_reg: phys(base_of, *cond),
                target: 0,
            }, out.len() as u32, &mut |b| out.push(b));
        }
        FlatExit::Table {
            index,
            arms,
            default,
        } => {
            // Wire: tag(1) index(1) n_arms(1) [key:u32 off:u32]*n default:u32
            let mut at = out.len() + 3 + 4; // first arm's offset field
            for (_, seg) in arms {
                fixups.push(Fixup::SegPc {
                    at,
                    func: func_idx,
                    target: *seg,
                });
                at += 8;
            }
            fixups.push(Fixup::SegPc {
                at,
                func: func_idx,
                target: *default,
            });
            StdExtOp::encode_ext_op(&StdExtOp::Table {
                index_reg: phys(base_of, *index),
                arms: arms.iter().map(|(k, _)| (*k, 0)).collect(),
                default_offset: 0,
            }, out.len() as u32, &mut |b| out.push(b));
        }
        FlatExit::HostOp { idx, args, out: out_reg } => {
            out.push(0x9B);
            out.push(*idx as u8);
            out.push(if out_reg.is_some() { 1 } else { 0 });
            out.push(args.len() as u8);
            for a in args {
                out.push(phys(base_of, *a));
            }
            out.push(phys(base_of, out_reg.unwrap_or(0)));
        }
        FlatExit::Call { func_reg } => {
            StdExtOp::encode_ext_op(&StdExtOp::Call {
                func_reg: phys(base_of, *func_reg),
            }, out.len() as u32, &mut |b| out.push(b));
        }
        FlatExit::Ret | FlatExit::Trap => {
            let at = out.len() + 1;
            fixups.push(Fixup::End { at });
            StdExtOp::encode_ext_op(&StdExtOp::Jump { target: 0 }, out.len() as u32, &mut |b| out.push(b));
        }
    }
}

/// Render a low-level module to a MiniBC artifact.
pub fn render_minibc(m: &LeModule) -> Result<MinibcArtifact, Vec<LeDiag>> {
    let mut diags: Vec<LeDiag> = Vec::new();
    let mut window_runs = 0usize;
    for f in &m.funcs {
        window_runs = window_runs.max(f.params.len()).max(f.results.len());
    }
    let window_runs = window_runs.max(1) as u32;

    let import_ids: BTreeSet<u32> = m.imports.iter().map(|i| i.func.0).collect();
    let mut flats: Vec<Option<(FnFlat, Alloc)>> = Vec::new();
    for (i, f) in m.funcs.iter().enumerate() {
        if import_ids.contains(&(i as u32)) || matches!(f.body, super::LeBody::Runtime) {
            flats.push(None);
            continue;
        }
        let flat = flatten_function(m, f);
        diags.extend(flat.diags.iter().cloned());
        match allocate(&flat, window_runs) {
            Ok(alloc) => flats.push(Some((flat, alloc))),
            Err(d) => {
                diags.push(d);
                flats.push(None);
            }
        }
    }
    if !diags.is_empty() {
        return Err(diags);
    }

    let mut out: Vec<u8> = Vec::new();
    let mut seg_pcs: BTreeMap<(u32, u32), u32> = BTreeMap::new();
    let mut func_entry: BTreeMap<u32, u32> = BTreeMap::new();
    let mut fixups: Vec<Fixup> = Vec::new();
    let mut store_regs = (window_runs * 4) as usize;
    for (i, f) in m.funcs.iter().enumerate() {
        let Some((flat, alloc)) = &flats[i] else { continue };
        func_entry.insert(i as u32, out.len() as u32);
        store_regs = store_regs.max(alloc.max_base as usize);
        for (si, seg) in flat.segs.iter().enumerate() {
            // A Call's pops splice may already have recorded this segment's
            // pc (the continuation includes the pops).
            seg_pcs.entry((i as u32, si as u32)).or_insert(out.len() as u32);
            for op in &seg.ops {
                emit_op(op, &alloc.base_of, &mut out, &mut fixups);
            }
            if let FlatExit::Call { .. } = &seg.exit {
                // Call saves: push locals live out of this segment right
                // before the Call exit; the continuation (physically next)
                // begins with the pops, then its own ops.
                let mut saves: Vec<u32> = alloc.liveness.live_out[si].iter().copied().collect();
                saves.sort();
                for s in &saves {
                    MiniOp::Push(Reg(phys(&alloc.base_of, *s)), RegForm::R4)
                        .encode(out.len() as u32, &mut |b| out.push(b));
                }
                emit_exit(
                    &seg.exit,
                    i as u32,
                    si as u32,
                    &alloc.base_of,
                    &mut out,
                    &mut fixups,
                );
                // The continuation segment starts here: record its pc and
                // emit the pops (its own ops follow in the next iteration).
                seg_pcs.insert((i as u32, si as u32 + 1), out.len() as u32);
                for s in saves.iter().rev() {
                    MiniOp::Pop(Reg(phys(&alloc.base_of, *s)), RegForm::R4)
                        .encode(out.len() as u32, &mut |b| out.push(b));
                }
            } else {
                emit_exit(
                    &seg.exit,
                    i as u32,
                    si as u32,
                    &alloc.base_of,
                    &mut out,
                    &mut fixups,
                );
            }
        }
    }

    for fixup in &fixups {
        match *fixup {
            Fixup::SegPc { at, func, target } => {
                let seg = match target {
                    FlatTarget::Seg(s) => s as u32,
                    FlatTarget::Block(b) => *flats[func as usize]
                        .as_ref()
                        .expect("func")
                        .0
                        .block_seg
                        .get(&b)
                        .expect("block fixup target"),
                };
                let pc = *seg_pcs.get(&(func, seg)).expect("seg fixup target");
                out[at..at + 4].copy_from_slice(&pc.to_le_bytes());
            }
            Fixup::FuncAddr { at, func } => {
                let pc = *func_entry.get(&func).expect("func entry") as u64;
                out[at..at + 8].copy_from_slice(&(pc as f64).to_bits().to_le_bytes());
            }
            Fixup::End { .. } => {} // patched at the buffer terminator
        }
    }

    let term_pc = out.len() as u32;
    out.push(0xFF); // buffer terminator; Ret/Trap jumps land here
    for fixup in &fixups {
        if let Fixup::End { at } = *fixup {
            out[at..at + 4].copy_from_slice(&term_pc.to_le_bytes());
        }
    }

    Ok(MinibcArtifact {
        bytes: out,
        exports: m
            .exports
            .iter()
            .filter_map(|e| func_entry.get(&e.func.0).map(|&pc| (e.name.clone(), pc)))
            .collect(),
        start: m.start.and_then(|s| func_entry.get(&s.0).copied()),
        store_regs,
        window_runs,
        imports: m
            .imports
            .iter()
            .map(|i| (i.module.clone(), i.name.clone()))
            .collect(),
        lane: m.lane.clone(),
        func_entries: func_entry
            .iter()
            .map(|(id, pc)| (m.funcs[*id as usize].name.clone(), *pc))
            .collect(),
    })
}
