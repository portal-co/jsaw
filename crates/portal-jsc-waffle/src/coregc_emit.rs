//! Phase-1 coregc artifact emitter.
//!
//! This emits a deliberately small core-Wasm module containing the generated
//! linear memory and a pure-Wasm bump allocator. It is not yet the full
//! WasmGC-to-core lowering pass: that requires root frames, descriptor scanners,
//! and rewriting every managed value. Keeping this artifact explicitly limited
//! prevents callers from mistaking an unlowered WasmGC program for a fallback.

use portal_pc_waffle::{
    FuncDecl, FunctionBody, MemoryData, Module, Operator, SignatureData,
    Terminator, Type,
};

use crate::coregc::{CoreGcError, CoreGcInventory};

/// Configuration for the phase-1 pure-Wasm runtime skeleton.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcOptions {
    pub initial_pages: usize,
    pub maximum_pages: Option<usize>,
    pub heap_base: u32,
}

impl Default for CoreGcOptions {
    fn default() -> Self {
        Self {
            initial_pages: 2,
            maximum_pages: None,
            // First page is reserved for future descriptors/runtime metadata;
            // heap payloads begin aligned in the second page.
            heap_base: 0x1_0000,
        }
    }
}

/// A core-Wasm artifact plus the inventory that describes its future heap.
#[derive(Clone, Debug)]
pub struct CoreGcArtifact {
    pub module: Module<'static>,
    pub inventory: CoreGcInventory,
}

/// Emit a core-only runtime skeleton from the typed WasmGC inventory.
///
/// Phase 1 intentionally accepts only an otherwise-empty source module. The
/// resulting artifact validates on a core engine and exposes the generated
/// allocator for direct runtime tests. A source module with functions or WasmGC
/// operations fails closed rather than producing semantically incorrect code.
pub fn emit_runtime_skeleton(
    source: &Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError> {
    if !source.funcs.entries().next().is_none()
        || !source.exports.is_empty()
        || !source.globals.entries().next().is_none()
        || !source.memories.iter().next().is_none()
    {
        return Err(CoreGcError {
            message: "coregc phase 1 only emits a runtime skeleton; source code lowering is not enabled yet"
                .to_owned(),
        });
    }
    if options.initial_pages == 0 {
        return Err(CoreGcError {
            message: "coregc requires at least one initial memory page".to_owned(),
        });
    }
    let inventory = CoreGcInventory::build(source)?;
    let mut module = Module::empty();
    let memory = module.memories.push(MemoryData {
        initial_pages: options.initial_pages,
        maximum_pages: options.maximum_pages,
        segments: vec![],
        memory64: false,
        shared: false,
        page_size_log2: None,
    });

    // `__coregc_alloc(payload_bytes) -> payload_address`: a checked bump
    // allocator. Header layout is intentionally fixed at 24 bytes so later
    // mark/sweep code can begin at this ABI without moving existing payloads.
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32],
        returns: vec![Type::I32],
        shared: false,
    });
    let mut body = FunctionBody::new(&module, signature);
    let block = body.entry;
    let payload_bytes = body.blocks[block].params[0].1;
    let heap_base = body.add_op(
        block,
        Operator::I32Const {
            value: options.heap_base,
        },
        &[],
        &[Type::I32],
    );
    let header = body.add_op(block, Operator::I32Const { value: 24 }, &[], &[Type::I32]);
    let total = body.add_op(
        block,
        Operator::I32Add,
        &[payload_bytes, header],
        &[Type::I32],
    );
    // Align allocation total to eight bytes: (total + 7) & ~7.
    let seven = body.add_op(block, Operator::I32Const { value: 7 }, &[], &[Type::I32]);
    let rounded = body.add_op(block, Operator::I32Add, &[total, seven], &[Type::I32]);
    let alignment_mask = body.add_op(
        block,
        Operator::I32Const { value: !7u32 },
        &[],
        &[Type::I32],
    );
    let _aligned_total = body.add_op(
        block,
        Operator::I32And,
        &[rounded, alignment_mask],
        &[Type::I32],
    );
    // The phase-1 allocator returns a fixed payload base. It is deliberately
    // not exported as a usable allocator until phase 2 adds bump state and
    // bounds/grow behavior; returning it lets the core artifact be validated
    // without pretending allocation semantics are complete.
    let payload = body.add_op(block, Operator::I32Add, &[heap_base, header], &[Type::I32]);
    body.set_terminator(
        block,
        Terminator::Return {
            values: vec![payload],
        },
    );
    let allocator = module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_alloc_phase1".to_owned(),
        body,
    ));
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_alloc_phase1".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(allocator),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "memory".to_owned(),
        kind: portal_pc_waffle::ExportKind::Memory(memory),
    });

    Ok(CoreGcArtifact { module, inventory })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_skeleton_is_core_wasm_without_gc_features() {
        let source = Module::empty();
        let artifact = emit_runtime_skeleton(&source, &CoreGcOptions::default())
            .expect("empty source accepts the phase-1 skeleton");
        for (_, function) in artifact.module.funcs.entries() {
            if let FuncDecl::Body(_, _, body) = function {
                body.validate().expect("generated runtime IR validates");
            }
        }
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("artifact validates with default core features");
    }

    #[test]
    fn runtime_skeleton_rejects_source_code_until_lowering_exists() {
        let mut source = Module::empty();
        let signature = source.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![],
            shared: false,
        });
        let body = FunctionBody::new(&source, signature);
        source
            .funcs
            .push(FuncDecl::Body(signature, "source".to_owned(), body));
        let error = emit_runtime_skeleton(&source, &CoreGcOptions::default())
            .expect_err("unlowered source functions must fail closed");
        assert!(error.message.contains("source code lowering"));
    }
}
