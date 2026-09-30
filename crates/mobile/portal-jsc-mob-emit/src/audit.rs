//! The feature-closure contract for the mobile emitters.
//!
//! The emitters support exactly the WasmGC closure the jsaw compiler emits
//! (see `docs/plan-wasmgc-mobile-backends.md` §6.0): GC structs and arrays,
//! the i31 JS-null sentinel, typed funcrefs, direct/`CallRef` calls,
//! frame-replacing tail calls, and scalar numeric operators. Anything
//! outside that closure — host imports, linear memory, globals, tables with
//! runtime manipulation, exceptions, SIMD, multi-value returns — is a hard
//! error here, never a silent fallback somewhere in a renderer.
//!
//! This audit runs over every module the e2e suite compiles, making the
//! closure a living contract.

use std::fmt;

use portal_pc_waffle::{
    EntityRef, FuncDecl, HeapType, Module, Operator, SignatureData, StorageType, Terminator, Type,
    ValueDef,
};

/// A single feature-closure violation.
#[derive(Clone, Debug)]
pub struct AuditError {
    /// Where the violation was found (function/signature context).
    pub context: String,
    /// What was found that lies outside the closure.
    pub message: String,
}

impl fmt::Display for AuditError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "mobile audit failed in {}: {}",
            self.context, self.message
        )
    }
}

impl std::error::Error for AuditError {}

type AuditResult = Result<(), AuditError>;

fn err<T>(context: &str, message: impl Into<String>) -> Result<T, AuditError> {
    Err(AuditError {
        context: context.to_string(),
        message: message.into(),
    })
}

/// Audit a whole module against the mobile feature closure.
pub fn audit_module(module: &Module<'_>) -> AuditResult {
    if !module.imports.is_empty() {
        return err(
            "module",
            format!(
                "host imports are not supported ({} imports present)",
                module.imports.len()
            ),
        );
    }
    if module.globals.len() > 0 {
        return err("module", "globals are not supported");
    }
    if module.memories.len() > 0 {
        return err("module", "linear memories are not supported");
    }
    if module.control_tags.len() > 0 {
        return err("module", "exception tags are not supported");
    }
    if module.start_func.is_some() {
        return err("module", "a start function is not supported");
    }
    for (table, data) in module.tables.entries() {
        let ok_ty = matches!(
            &data.ty,
            Type::Heap(h)
                if matches!(h.value, HeapType::FuncRef | HeapType::Sig { .. } | HeapType::NoFunc)
        );
        if !ok_ty {
            return err("module", format!("table {table:?} is not funcref-typed"));
        }
        if data.func_elements.is_none() {
            return err(
                "module",
                format!(
                    "table {table:?} has no static func_elements; runtime table use is unsupported"
                ),
            );
        }
    }
    for export in &module.exports {
        if !matches!(export.kind, portal_pc_waffle::ExportKind::Func(_)) {
            return err(
                "module",
                format!("export {:?} is not a function export", export.name),
            );
        }
    }
    for (sig, data) in module.signatures.entries() {
        audit_signature(sig.index(), data)?;
    }
    for (func, decl) in module.funcs.entries() {
        match decl {
            FuncDecl::Body(sig, name, body) => {
                let context = format!("func {name} ({:?})", func);
                let rets = match &module.signatures[*sig] {
                    SignatureData::Func { returns, .. } => returns.clone(),
                    _ => {
                        return err(&context, "function has a non-function signature");
                    }
                };
                if rets.len() > 1 {
                    return err(
                        &context,
                        format!("multi-value returns are not supported ({rets:?})"),
                    );
                }
                for &(ty, _) in &body.blocks[body.entry].params {
                    audit_type(&context, ty)?;
                }
                for def in body.blocks.entries() {
                    for inst in &def.1.insts {
                        audit_value(&context, &body.values[inst.value], body)?;
                    }
                    audit_terminator(&context, &def.1.terminator.terminator)?;
                }
            }
            FuncDecl::Import(..) => {
                return err(
                    "module",
                    format!("imported function {func:?} is not supported"),
                );
            }
            _ => {
                return err(
                    "module",
                    format!("function {func:?} is not a materialized IR body"),
                );
            }
        }
    }
    Ok(())
}

fn audit_signature(sig: usize, data: &SignatureData) -> AuditResult {
    let context = format!("signature {sig}");
    match data {
        SignatureData::Func {
            params, returns, ..
        } => {
            for ty in params.iter().chain(returns.iter()) {
                audit_type(&context, *ty)?;
            }
        }
        SignatureData::Struct { fields, .. } => {
            for field in fields {
                match field.value {
                    StorageType::Val(ty) => audit_type(&context, ty)?,
                    packed => {
                        return err(
                            &context,
                            format!("packed struct field {packed:?} is not supported"),
                        );
                    }
                }
            }
        }
        SignatureData::Array { ty, .. } => match ty.value {
            StorageType::I8 | StorageType::I16 => {}
            StorageType::Val(ty) => audit_type(&context, ty)?,
            other => {
                return err(
                    &context,
                    format!("array element storage {other:?} is not supported"),
                );
            }
        },
        other => {
            // Be robust to SignatureData variants added upstream: only the
            // three kinds above are in the closure.
            return err(&context, format!("unsupported signature kind {other:?}"));
        }
    }
    Ok(())
}

fn audit_type(context: &str, ty: Type) -> AuditResult {
    match ty {
        Type::I32 | Type::I64 | Type::F32 | Type::F64 => Ok(()),
        Type::Heap(h) => match h.value {
            HeapType::FuncRef
            | HeapType::Sig { .. }
            | HeapType::Any
            | HeapType::Eq
            | HeapType::I31
            | HeapType::Struct
            | HeapType::Array
            | HeapType::None
            | HeapType::NoFunc => Ok(()),
            other => err(
                context,
                format!("heap type {other:?} is outside the mobile closure"),
            ),
        },
        other => err(
            context,
            format!("type {other:?} is outside the mobile closure"),
        ),
    }
}

fn audit_terminator(context: &str, term: &Terminator) -> AuditResult {
    match term {
        Terminator::Br { .. } | Terminator::CondBr { .. } | Terminator::Unreachable => Ok(()),
        Terminator::Return { values } => {
            if values.len() > 1 {
                return err(context, "multi-value return terminator is not supported");
            }
            Ok(())
        }
        Terminator::ReturnCall { .. } | Terminator::ReturnCallRef { .. } => Ok(()),
        other => err(
            context,
            format!("terminator {other:?} is outside the mobile closure"),
        ),
    }
}

fn audit_value(
    context: &str,
    def: &ValueDef,
    body: &portal_pc_waffle::FunctionBody,
) -> AuditResult {
    match def {
        ValueDef::BlockParam(..) | ValueDef::Alias(_) => Ok(()),
        ValueDef::PickOutput(..) => err(
            context,
            "multi-result operator projection (PickOutput) is not supported",
        ),
        ValueDef::Operator(op, _args, tys) => {
            if tys.len() > 1 {
                return err(
                    context,
                    format!("multi-result operator {op:?} is not supported"),
                );
            }
            for ty in body.type_pool[*tys].iter() {
                audit_type(context, *ty)?;
            }
            audit_operator(context, op)
        }
        other => err(
            context,
            format!("value definition {other:?} is not supported"),
        ),
    }
}

/// The operator whitelist. Numeric scalar families are whitelisted as a
/// block; everything memory/global/table/SIMD/atomic/exception related is
/// rejected by the catch-all.
fn audit_operator(context: &str, op: &Operator) -> AuditResult {
    use Operator::*;
    match op {
        // Control / calls.
        Unreachable | Nop | Call { .. } | CallRef { .. } | TypedSelect { .. } => Ok(()),
        // Constants.
        I32Const { .. } | I64Const { .. } | F32Const { .. } | F64Const { .. } => Ok(()),
        // i32 comparisons and arithmetic.
        I32Eqz | I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS
        | I32GeU | I32Clz | I32Ctz | I32Popcnt | I32Add | I32Sub | I32Mul | I32DivS | I32DivU
        | I32RemS | I32RemU | I32And | I32Or | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotl
        | I32Rotr => Ok(()),
        // i64 comparisons and arithmetic.
        I64Eqz | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS
        | I64GeU | I64Clz | I64Ctz | I64Popcnt | I64Add | I64Sub | I64Mul | I64DivS | I64DivU
        | I64RemS | I64RemU | I64And | I64Or | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl
        | I64Rotr => Ok(()),
        // f32/f64 comparisons and arithmetic.
        F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge | F32Abs | F32Neg | F32Ceil | F32Floor
        | F32Trunc | F32Nearest | F32Sqrt | F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max
        | F32Copysign | F64Eq | F64Ne | F64Lt | F64Gt | F64Le | F64Ge | F64Abs | F64Neg
        | F64Ceil | F64Floor | F64Trunc | F64Nearest | F64Sqrt | F64Add | F64Sub | F64Mul
        | F64Div | F64Min | F64Max | F64Copysign => Ok(()),
        // Conversions (trapping and saturating) and reinterprets.
        I32WrapI64 | I32TruncF32S | I32TruncF32U | I32TruncF64S | I32TruncF64U | I64ExtendI32S
        | I64ExtendI32U | I64TruncF32S | I64TruncF32U | I64TruncF64S | I64TruncF64U
        | F32ConvertI32S | F32ConvertI32U | F32ConvertI64S | F32ConvertI64U | F32DemoteF64
        | F64ConvertI32S | F64ConvertI32U | F64ConvertI64S | F64ConvertI64U | F64PromoteF32
        | I32Extend8S | I32Extend16S | I64Extend8S | I64Extend16S | I64Extend32S
        | I32TruncSatF32S | I32TruncSatF32U | I32TruncSatF64S | I32TruncSatF64U
        | I64TruncSatF32S | I64TruncSatF32U | I64TruncSatF64S | I64TruncSatF64U
        | F32ReinterpretI32 | F64ReinterpretI64 | I32ReinterpretF32 | I64ReinterpretF64 => Ok(()),
        // References.
        RefNull { .. }
        | RefIsNull { .. }
        | RefFunc { .. }
        | RefTest { .. }
        | RefCast { .. }
        | RefEq { .. }
        | RefI31 { .. }
        | I31GetS { .. }
        | I31GetU { .. } => Ok(()),
        // Structs.
        StructNew { .. }
        | StructNewDefault { .. }
        | StructGet { .. }
        | StructGetS { .. }
        | StructGetU { .. }
        | StructSet { .. } => Ok(()),
        // Arrays (no data/elem-segment variants: the module has none).
        ArrayNew { .. }
        | ArrayNewFixed { .. }
        | ArrayNewDefault { .. }
        | ArrayGet { .. }
        | ArrayGetS { .. }
        | ArrayGetU { .. }
        | ArraySet { .. }
        | ArrayLen { .. }
        | ArrayCopy { .. } => Ok(()),
        other => err(
            context,
            format!("operator {other:?} is outside the mobile closure"),
        ),
    }
}
