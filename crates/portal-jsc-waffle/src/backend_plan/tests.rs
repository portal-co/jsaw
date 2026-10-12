//! Tests for the stable BackendPlan representation.

use super::*;
use portal_pc_waffle::{
    BlockTarget, Export, FuncDecl, FunctionBody, Import, WithMutablility, WithNullable,
};

fn field(storage: StorageType) -> WithMutablility<StorageType> {
    WithMutablility {
        value: storage,
        mutable: true,
    }
}

fn func_sig(
    module: &mut Module<'_>,
    params: Vec<Type>,
    returns: Vec<Type>,
) -> portal_pc_waffle::Signature {
    module.signatures.push(SignatureData::Func {
        params,
        returns,
        shared: false,
    })
}

/// `(i32, i32) -> i32` returning `a + b`.
fn add_module() -> Module<'static> {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32, Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let a = body.blocks[entry].params[0].1;
    let b = body.blocks[entry].params[1].1;
    let sum = body.add_op(entry, Operator::I32Add, &[a, b], &[Type::I32]);
    body.set_terminator(entry, Terminator::Return { values: vec![sum] });
    let func = module.funcs.push(FuncDecl::Body(sig, "add".to_owned(), body));
    module.exports.push(portal_pc_waffle::Export {
        name: "add".to_owned(),
        kind: ExportKind::Func(func),
    });
    module
}

#[test]
fn scalars_and_deterministic_fingerprint() {
    let module = add_module();
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    assert_eq!(plan.schema, BACKEND_PLAN_SCHEMA);
    assert_eq!(plan.functions.len(), 1);
    let function = &plan.functions[0];
    assert_eq!(function.name, "add");
    assert_eq!(function.entry, Some(PlanBlockId(0)));
    assert_eq!(function.blocks.len(), 1);
    let block = &function.blocks[0];
    assert_eq!(block.params.len(), 2);
    assert_eq!(block.insts.len(), 1);
    assert_eq!(block.insts[0].operator, "I32Add");
    assert_eq!(block.insts[0].args, vec![PlanValueId(0), PlanValueId(1)]);
    assert_eq!(block.insts[0].results, vec![PlanValueId(2)]);
    assert!(matches!(
        block.terminator,
        PlanTerminator::Return { ref values } if values == &vec![PlanValueId(2)]
    ));
    assert_eq!(
        function.completions.kinds,
        vec![CompletionKind::Return],
        "plain function has only a return completion"
    );
    // Layouts: everything scalar.
    assert!(plan
        .functions[0]
        .values
        .iter()
        .all(|value| matches!(value.layout, PlanLayout::Scalar { bytes: 4 })));
    assert!(plan
        .functions[0]
        .values
        .iter()
        .all(|value| value.representation == NanoValueRepresentation::Scalar));

    // Determinism: planning twice yields identical JSON and fingerprint.
    let again = BackendPlan::from_module(&module).expect("valid plan");
    assert_eq!(plan.to_json(), again.to_json());
    assert_eq!(plan.fingerprint(), again.fingerprint());
    assert_eq!(plan.fingerprint().len(), 64);
    assert!(plan
        .fingerprint()
        .chars()
        .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()));

    // A fresh but logically identical module (fresh arenas, fresh pointers)
    // produces the identical plan.
    let rebuilt = add_module();
    let rebuilt_plan = BackendPlan::from_module(&rebuilt).expect("valid plan");
    assert_eq!(plan.to_json(), rebuilt_plan.to_json());
    assert_eq!(plan.fingerprint(), rebuilt_plan.fingerprint());
}

#[test]
fn aggregates_get_arena_representations_and_layouts() {
    let mut module = Module::empty();
    let pair = module.signatures.push(SignatureData::Struct {
        fields: vec![
            field(StorageType::Val(Type::I32)),
            field(StorageType::Val(Type::I64)),
        ],
        shared: false,
    });
    let pair_ref = Type::Heap(WithNullable {
        value: HeapType::Sig { sig_index: pair },
        nullable: false,
    });
    let sig = func_sig(&mut module, vec![Type::I32], vec![pair_ref]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let zero = body.add_op(entry, Operator::I64Const { value: 0 }, &[], &[Type::I64]);
    let allocated = body.add_op(entry, Operator::StructNew { sig: pair }, &[input, zero], &[pair_ref]);
    let read = body.add_op(
        entry,
        Operator::StructGet { sig: pair, idx: 0 },
        &[allocated],
        &[Type::I32],
    );
    let written = body.add_op(
        entry,
        Operator::StructSet { sig: pair, idx: 0 },
        &[allocated, read],
        &[],
    );
    let _ = written;
    body.set_terminator(entry, Terminator::Return {
        values: vec![allocated],
    });
    module.funcs.push(FuncDecl::Body(sig, "pair".to_owned(), body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let function = &plan.functions[0];
    let insts = &function.blocks[0].insts;
    assert_eq!(insts[1].operator, "StructNew");
    assert_eq!(insts[1].arena, Some(PlanArenaOp::Alloc));
    assert_eq!(insts[2].arena, Some(PlanArenaOp::Read));
    assert_eq!(insts[3].arena, Some(PlanArenaOp::Write));
    // The aggregate value carries an arena representation and an aggregate
    // layout referencing the interned signature.
    let aggregate_value = &function.values[2]; // struct.new result
    assert_eq!(
        aggregate_value.representation,
        NanoValueRepresentation::ArenaRef
    );
    assert_eq!(aggregate_value.lifetime, NanoLifetime::Artifact);
    assert!(matches!(aggregate_value.layout, PlanLayout::Aggregate { .. }));
    assert!(plan.runtime.needs_arena);
    // Aggregate signature was interned structurally with both fields.
    let struct_sig = plan
        .signatures
        .iter()
        .find_map(|sig| match sig {
            PlanSignature::Struct { fields, .. } => Some(fields),
            _ => None,
        })
        .expect("struct signature present");
    assert_eq!(struct_sig.len(), 2);
}

#[test]
fn function_references_and_ref_calls() {
    let mut module = Module::empty();
    let callee_sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let callee_funcref = Type::Heap(WithNullable {
        value: HeapType::Sig {
            sig_index: callee_sig,
        },
        nullable: false,
    });
    // callee: identity
    let mut callee_body = FunctionBody::new(&module, callee_sig);
    let entry = callee_body.entry;
    let input = callee_body.blocks[entry].params[0].1;
    callee_body.set_terminator(entry, Terminator::Return { values: vec![input] });
    let callee = module
        .funcs
        .push(FuncDecl::Body(callee_sig, "id".to_owned(), callee_body));

    // caller: takes i32, returns i32; calls callee through a typed funcref.
    let mut body = FunctionBody::new(&module, callee_sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let funcref = body.add_op(
        entry,
        Operator::RefFunc { func_index: callee },
        &[],
        &[callee_funcref],
    );
    let result = body.add_op(
        entry,
        Operator::CallRef {
            sig_index: callee_sig,
        },
        &[input, funcref],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![result],
    });
    module
        .funcs
        .push(FuncDecl::Body(callee_sig, "caller".to_owned(), body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let caller = &plan.functions[1];
    let insts = &caller.blocks[0].insts;
    assert_eq!(insts[0].operator, "RefFunc");
    // The funcref value (values[1]: param, funcref, call result) is
    // boundary-represented: function identity crosses the code boundary.
    assert_eq!(
        caller.values[1].representation,
        NanoValueRepresentation::Boundary
    );
    let call = insts[1].call.as_ref().expect("call_ref recorded");
    assert_eq!(call.kind, PlanCallKind::Ref);
    assert!(!call.tail);
    assert!(plan.runtime.uses_ref_calls);
    // The callee's signature is interned and referenced canonically.
    let sig = plan.signatures[call.signature.index()].clone();
    assert!(matches!(sig, PlanSignature::Func { .. }));
}

#[test]
fn direct_and_tail_calls_with_matching_results() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    // callee
    let mut callee_body = FunctionBody::new(&module, sig);
    let entry = callee_body.entry;
    let input = callee_body.blocks[entry].params[0].1;
    callee_body.set_terminator(entry, Terminator::Return { values: vec![input] });
    let callee = module
        .funcs
        .push(FuncDecl::Body(sig, "callee".to_owned(), callee_body));

    // tail-caller: return_call callee(x)
    let mut tail_body = FunctionBody::new(&module, sig);
    let entry = tail_body.entry;
    let input = tail_body.blocks[entry].params[0].1;
    tail_body.set_terminator(entry, Terminator::ReturnCall {
        func: callee,
        args: vec![input],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "tail".to_owned(), tail_body));

    // direct caller: return callee(x)
    let mut direct_body = FunctionBody::new(&module, sig);
    let entry = direct_body.entry;
    let input = direct_body.blocks[entry].params[0].1;
    let result = direct_body.add_op(
        entry,
        Operator::Call {
            function_index: callee,
        },
        &[input],
        &[Type::I32],
    );
    direct_body.set_terminator(entry, Terminator::Return {
        values: vec![result],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "direct".to_owned(), direct_body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    // Tail call.
    let tail = &plan.functions[1];
    assert!(matches!(
        &tail.blocks[0].terminator,
        PlanTerminator::TailCall {
            call: PlanCallKind::Direct { callee },
            ..
        } if *callee == PlanFuncId(0)
    ));
    assert!(plan.runtime.uses_tail_calls);
    // Direct call instruction.
    let direct = &plan.functions[2];
    let call = direct.blocks[0].insts[0].call.as_ref().expect("call recorded");
    assert_eq!(
        call.kind,
        PlanCallKind::Direct {
            callee: PlanFuncId(0)
        }
    );
    assert!(!call.tail);
}

#[test]
fn multi_result_calls_project_pick_outputs() {
    let mut module = Module::empty();
    let pair_sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32, Type::I32]);
    // producer: returns (x, x)
    let mut producer_body = FunctionBody::new(&module, pair_sig);
    let entry = producer_body.entry;
    let input = producer_body.blocks[entry].params[0].1;
    producer_body.set_terminator(entry, Terminator::Return {
        values: vec![input, input],
    });
    let producer = module
        .funcs
        .push(FuncDecl::Body(pair_sig, "producer".to_owned(), producer_body));

    // consumer: sums the two results of producer(x)
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let tuple = body.add_op(
        entry,
        Operator::Call {
            function_index: producer,
        },
        &[input],
        &[Type::I32, Type::I32],
    );
    let first = body.add_value(ValueDef::PickOutput(tuple, 0, Type::I32));
    body.append_to_block(entry, first);
    let second = body.add_value(ValueDef::PickOutput(tuple, 1, Type::I32));
    body.append_to_block(entry, second);
    let sum = body.add_op(entry, Operator::I32Add, &[first, second], &[Type::I32]);
    body.set_terminator(entry, Terminator::Return { values: vec![sum] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "consumer".to_owned(), body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    assert!(plan.runtime.uses_multi_result);
    let consumer = &plan.functions[1];
    let insts = &consumer.blocks[0].insts;
    assert_eq!(insts[0].operator, "Call");
    assert_eq!(insts[0].result_types.len(), 2);
    assert_eq!(insts[1].operator, "pick_output");
    assert_eq!(insts[2].operator, "pick_output");
    // The tuple value is an inline aggregate with no standalone type.
    let tuple_value = &consumer.values[1];
    assert_eq!(tuple_value.ty, None);
    assert_eq!(
        tuple_value.representation,
        NanoValueRepresentation::InlineAggregate
    );
    assert!(matches!(tuple_value.layout, PlanLayout::Tuple { .. }));
    assert!(matches!(
        consumer.values[2].def,
        PlanValueDef::PickOutput { index: 0, .. }
    ));
}

#[test]
fn block_parameters_and_predecessor_args_validate() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let header = body.add_block();
    let counter = body.add_blockparam(header, Type::I32);
    let one = body.add_op(header, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let next = body.add_op(header, Operator::I32Add, &[counter, one], &[Type::I32]);
    body.set_terminator(entry, Terminator::Br {
        target: BlockTarget {
            block: header,
            args: vec![input],
        },
    });
    body.set_terminator(header, Terminator::Br {
        target: BlockTarget {
            block: header,
            args: vec![next],
        },
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "loop".to_owned(), body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let function = &plan.functions[0];
    assert_eq!(function.blocks.len(), 2);
    let header_block = &function.blocks[1];
    assert_eq!(header_block.params.len(), 1);
    assert!(matches!(
        &header_block.terminator,
        PlanTerminator::Br { target } if target.block == PlanBlockId(1) && target.args.len() == 1
    ));
}

#[test]
fn trap_and_return_completions_are_recorded() {
    let mut module = Module::empty();
    // Function that returns normally.
    let sig = func_sig(&mut module, vec![], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let seven = body.add_op(entry, Operator::I32Const { value: 7 }, &[], &[Type::I32]);
    body.set_terminator(entry, Terminator::Return { values: vec![seven] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "ok".to_owned(), body));
    // Function that can trap: divides (may trap) and has an explicit
    // unreachable arm selected by a conditional.
    let div_sig = func_sig(&mut module, vec![Type::I32, Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, div_sig);
    let entry = body.entry;
    let a = body.blocks[entry].params[0].1;
    let b = body.blocks[entry].params[1].1;
    let quotient = body.add_op(entry, Operator::I32DivS, &[a, b], &[Type::I32]);
    let trap_block = body.add_block();
    let ok_block = body.add_block();
    let ok_param = body.add_blockparam(ok_block, Type::I32);
    body.set_terminator(entry, Terminator::CondBr {
        cond: b,
        if_true: BlockTarget {
            block: ok_block,
            args: vec![quotient],
        },
        if_false: BlockTarget {
            block: trap_block,
            args: vec![],
        },
    });
    body.set_terminator(trap_block, Terminator::Unreachable);
    body.set_terminator(ok_block, Terminator::Return {
        values: vec![ok_param],
    });
    module
        .funcs
        .push(FuncDecl::Body(div_sig, "div".to_owned(), body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    assert_eq!(plan.functions[0].completions.kinds, vec![CompletionKind::Return]);
    assert_eq!(
        plan.functions[1].completions.kinds,
        vec![CompletionKind::Return, CompletionKind::Trap]
    );
    assert!(plan.runtime.may_trap);
    // RPO places the entry first, then the conditional's successors.
    assert!(matches!(
        plan.functions[1].blocks[0].terminator,
        PlanTerminator::CondBr { .. }
    ));
    assert_eq!(plan.functions[1].blocks[1].terminator, PlanTerminator::Trap);
}

#[test]
fn host_import_and_export_boundaries() {
    let mut module = Module::empty();
    let import_sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let imported = module
        .funcs
        .push(FuncDecl::Import(import_sig, "increment".to_owned()));
    module.imports.push(Import {
        module: "host".to_owned(),
        name: "increment".to_owned(),
        kind: ImportKind::Func(imported),
    });
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let result = body.add_op(
        entry,
        Operator::Call {
            function_index: imported,
        },
        &[input],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![result],
    });
    let caller = module
        .funcs
        .push(FuncDecl::Body(sig, "caller".to_owned(), body));
    module.exports.push(Export {
        name: "caller".to_owned(),
        kind: ExportKind::Func(caller),
    });

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    assert_eq!(plan.boundaries.len(), 2);
    let import = &plan.boundaries[0];
    assert_eq!(import.direction, BoundaryDirection::Import);
    assert_eq!(import.module.as_deref(), Some("host"));
    assert_eq!(import.name, "increment");
    assert_eq!(import.target, BoundaryTarget::Func(PlanFuncId(0)));
    let abi = import.abi.as_ref().expect("function import has an ABI");
    assert_eq!(abi.params.len(), 1);
    assert_eq!(abi.results.len(), 1);
    let export = &plan.boundaries[1];
    assert_eq!(export.direction, BoundaryDirection::Export);
    assert_eq!(export.target, BoundaryTarget::Func(PlanFuncId(1)));
    // The imported function has no body.
    assert_eq!(plan.functions[0].entry, None);
    assert!(matches!(
        plan.functions[0].kind,
        PlanFunctionKind::Import { .. }
    ));
}

#[test]
fn signature_ids_do_not_depend_on_allocation_order() {
    // Build two logically identical modules whose Waffle signature entities
    // are allocated in different orders. The plan's structural signature
    // table must be identical.
    fn build(order_swapped: bool) -> Module<'static> {
        let mut module = Module::empty();
        let push_i32 = |module: &mut Module<'_>| {
            module.signatures.push(SignatureData::Struct {
                fields: vec![field(StorageType::Val(Type::I32))],
                shared: false,
            })
        };
        let push_i64 = |module: &mut Module<'_>| {
            module.signatures.push(SignatureData::Struct {
                fields: vec![field(StorageType::Val(Type::I64))],
                shared: false,
            })
        };
        // Only the Waffle arena allocation order differs; the logical content
        // (global list order and structure) is identical.
        let (i32_struct, i64_struct) = if order_swapped {
            let i64s = push_i64(&mut module);
            let i32s = push_i32(&mut module);
            (i32s, i64s)
        } else {
            let i32s = push_i32(&mut module);
            let i64s = push_i64(&mut module);
            (i32s, i64s)
        };
        module.globals.push(portal_pc_waffle::GlobalData {
            ty: Type::Heap(WithNullable {
                value: HeapType::Sig {
                    sig_index: i32_struct,
                },
                nullable: true,
            }),
            value: None,
            mutable: false,
        });
        module.globals.push(portal_pc_waffle::GlobalData {
            ty: Type::Heap(WithNullable {
                value: HeapType::Sig {
                    sig_index: i64_struct,
                },
                nullable: true,
            }),
            value: None,
            mutable: false,
        });
        module
    }
    let normal = BackendPlan::from_module(&build(false)).expect("valid plan");
    let swapped = BackendPlan::from_module(&build(true)).expect("valid plan");
    assert_eq!(normal.to_json(), swapped.to_json());
    assert_eq!(normal.fingerprint(), swapped.fingerprint());
}

#[test]
fn malformed_block_param_arity_fails_closed() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![], vec![]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let target = body.add_block();
    let _param = body.add_blockparam(target, Type::I32);
    body.set_terminator(entry, Terminator::Br {
        target: BlockTarget {
            block: target,
            args: vec![], // arity mismatch: one param, zero args
        },
    });
    body.set_terminator(target, Terminator::Return { values: vec![] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "bad".to_owned(), body));

    let diagnostics = BackendPlan::from_module(&module).expect_err("must fail");
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind == DiagnosticKind::BlockParamArity));
}

#[test]
fn malformed_def_after_use_fails_closed() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    // Use `later` before its definition in the same block.
    let later = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    // Rebuild: create a second op that uses `later` and force it earlier by
    // constructing the block instruction list manually.
    let earlier = body.add_op(entry, Operator::I32Const { value: 2 }, &[], &[Type::I32]);
    // Manually craft a use-before-def: add an op whose arg is `later` and
    // place it before `later`'s record.
    let user = body.add_op(entry, Operator::I32Add, &[earlier, later], &[Type::I32]);
    // Reorder the block's instructions so `user` precedes `later`.
    let block = &mut body.blocks[entry];
    let records = &mut block.insts;
    records.swap(0, 2);
    body.set_terminator(entry, Terminator::Return { values: vec![user] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "bad".to_owned(), body));

    let diagnostics = BackendPlan::from_module(&module).expect_err("must fail");
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind == DiagnosticKind::DefAfterUse));
}

#[test]
fn malformed_call_shape_fails_closed() {
    let mut module = Module::empty();
    let callee_sig = func_sig(&mut module, vec![Type::I32, Type::I32], vec![Type::I32]);
    let mut callee_body = FunctionBody::new(&module, callee_sig);
    let entry = callee_body.entry;
    let input = callee_body.blocks[entry].params[0].1;
    callee_body.set_terminator(entry, Terminator::Return { values: vec![input] });
    let callee = module
        .funcs
        .push(FuncDecl::Body(callee_sig, "callee".to_owned(), callee_body));

    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    // Wrong arity: one argument for a two-parameter callee.
    let result = body.add_op(
        entry,
        Operator::Call {
            function_index: callee,
        },
        &[input],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![result],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "caller".to_owned(), body));

    let diagnostics = BackendPlan::from_module(&module).expect_err("must fail");
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind == DiagnosticKind::CallShape));
}

#[test]
fn malformed_duplicate_export_names_fail_closed() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![], vec![]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    body.set_terminator(entry, Terminator::Return { values: vec![] });
    let first = module
        .funcs
        .push(FuncDecl::Body(sig, "a".to_owned(), body));
    module.exports.push(Export {
        name: "dup".to_owned(),
        kind: ExportKind::Func(first),
    });
    module.exports.push(Export {
        name: "dup".to_owned(),
        kind: ExportKind::Func(first),
    });

    let diagnostics = BackendPlan::from_module(&module).expect_err("must fail");
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind == DiagnosticKind::BoundaryAbi));
}

#[test]
fn malformed_missing_terminator_fails_closed() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![], vec![]);
    let body = FunctionBody::new(&module, sig); // entry has no terminator
    module
        .funcs
        .push(FuncDecl::Body(sig, "bad".to_owned(), body));

    let diagnostics = BackendPlan::from_module(&module).expect_err("must fail");
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind == DiagnosticKind::InvalidTerminator));
}

#[test]
fn exception_values_are_ordinary_aggregate_returns() {
    // jsaw's exception convention (#13): exception results are ordinary
    // typed aggregates, so the plan records them as aggregate returns with
    // Return completion, never as a Wasm exception ABI.
    let mut module = Module::empty();
    let exception = module.signatures.push(SignatureData::Struct {
        fields: vec![field(StorageType::Val(Type::I32))],
        shared: false,
    });
    let exception_ref = Type::Heap(WithNullable {
        value: HeapType::Sig {
            sig_index: exception,
        },
        nullable: false,
    });
    let sig = func_sig(&mut module, vec![Type::I32], vec![exception_ref]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let thrown = body.add_op(
        entry,
        Operator::StructNew {
            sig: exception,
        },
        &[input],
        &[exception_ref],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![thrown],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "thrower".to_owned(), body));

    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let function = &plan.functions[0];
    assert_eq!(function.completions.kinds, vec![CompletionKind::Return]);
    let result_ty = function.completions.results[0];
    assert!(matches!(
        plan.types[result_ty.index()],
        PlanType::Ref { .. }
    ));
    assert_eq!(function.values[1].representation, NanoValueRepresentation::ArenaRef);
}
