//! Tests for nano native-function mode and bounded inlining.

use super::*;
use crate::backend_plan::{BackendPlan, CompletionKind, PlanFuncId, PlanType};
use portal_jsc_common::assumptions::AssumptionSet;
use crate::nano::{NanoLifetime, NanoValueRepresentation};
use portal_pc_waffle::{
    EntityRef, Export, ExportKind, FuncDecl, FunctionBody, HeapType, Module, Operator,
    SignatureData, StorageType, Terminator, Type, WithMutablility, WithNullable,
};

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

fn assumptions() -> AssumptionSet {
    AssumptionSet::empty()
}

/// `id(x) = x`; `caller(x) = id(x)`.
fn direct_call_module() -> Module<'static> {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut id_body = FunctionBody::new(&module, sig);
    let entry = id_body.entry;
    let x = id_body.blocks[entry].params[0].1;
    id_body.set_terminator(entry, Terminator::Return { values: vec![x] });
    let id = module.funcs.push(FuncDecl::Body(sig, "id".to_owned(), id_body));

    let mut caller_body = FunctionBody::new(&module, sig);
    let entry = caller_body.entry;
    let x = caller_body.blocks[entry].params[0].1;
    let called = caller_body.add_op(
        entry,
        Operator::Call {
            function_index: id,
        },
        &[x],
        &[Type::I32],
    );
    caller_body.set_terminator(entry, Terminator::Return {
        values: vec![called],
    });
    let caller = module
        .funcs
        .push(FuncDecl::Body(sig, "caller".to_owned(), caller_body));
    module.exports.push(Export {
        name: "caller".to_owned(),
        kind: ExportKind::Func(caller),
    });
    module
}

// ---------------------------------------------------------------------------
// Acceptance 1: default configuration is unchanged; native mode is opt-in.
// ---------------------------------------------------------------------------

#[test]
fn default_configuration_is_the_generic_adapter_everywhere() {
    let module = direct_call_module();
    let default_outcome = plan_nano_native(&module, &assumptions(), &NanoNativeConfig::default())
        .expect("default configuration plans");
    assert!(!default_outcome.native.native_functions);
    assert_eq!(default_outcome.native.inlining, InliningMode::Off);
    assert!(!default_outcome.native.decisions.is_empty());
    assert!(default_outcome
        .native
        .decisions
        .iter()
        .all(|decision| decision.strategy == NanoCallStrategy::GenericAdapter));
    assert!(default_outcome.native.inline_records.is_empty());
    assert!(default_outcome.native.capability_checks.is_empty());
    // The fixed adapter ABI record is always present and unchanged.
    assert_eq!(
        default_outcome.native.adapter_abi.params,
        ["context", "this", "arguments"]
    );
    assert!(default_outcome.native.adapter_abi.source_arity_preserved);
    assert!(default_outcome.native.adapter_abi.arguments_array_spread);

    // The underlying BackendPlan is untouched by the default config.
    let plain = BackendPlan::from_module(&module).expect("valid plan");
    assert_eq!(plain.to_json(), default_outcome.backend.to_json());
    assert_eq!(plain.fingerprint(), default_outcome.backend.fingerprint());

    // Opting in selects the direct strategy for the module-known call.
    let native = NanoNativeConfig {
        native_functions: true,
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &native).expect("native mode plans");
    assert!(outcome
        .native
        .decisions
        .iter()
        .any(|decision| decision.strategy == NanoCallStrategy::Direct
            && decision.callee == Some(PlanFuncId(0))));
}

// ---------------------------------------------------------------------------
// Acceptance 2: direct calls never occur for uncertain/rebound references.
// ---------------------------------------------------------------------------

#[test]
fn uncertain_and_rebound_references_keep_the_guarded_adapter() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let funcref = Type::Heap(WithNullable {
        value: HeapType::Sig { sig_index: sig },
        nullable: true,
    });
    // caller(ref, x) = call_ref(x, ref): identity unknown.
    let caller_sig = func_sig(&mut module, vec![funcref, Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, caller_sig);
    let entry = body.entry;
    let reference = body.blocks[entry].params[0].1;
    let x = body.blocks[entry].params[1].1;
    let result = body.add_op(
        entry,
        Operator::CallRef { sig_index: sig },
        &[x, reference],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![result],
    });
    module
        .funcs
        .push(FuncDecl::Body(caller_sig, "caller".to_owned(), body));

    let native = NanoNativeConfig {
        native_functions: true,
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &native).expect("plans");
    let decision = &outcome.native.decisions[0];
    assert_eq!(decision.strategy, NanoCallStrategy::GuardedAdapter);
    assert_eq!(decision.callee, None, "no identity is claimed");
    assert_eq!(
        decision.fallback,
        Some(NanoCallStrategy::GenericAdapter),
        "the generic adapter path remains available"
    );
    assert!(outcome
        .native
        .diagnostics
        .iter()
        .any(|d| d.kind == NanoNativeDiagnosticKind::GuardedFallback));
    assert!(!outcome
        .native
        .decisions
        .iter()
        .any(|d| matches!(d.strategy, NanoCallStrategy::Direct | NanoCallStrategy::TailDirect)));
}

#[test]
fn ref_func_provenance_enables_direct_calls() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut id_body = FunctionBody::new(&module, sig);
    let entry = id_body.entry;
    let x = id_body.blocks[entry].params[0].1;
    id_body.set_terminator(entry, Terminator::Return { values: vec![x] });
    let id = module.funcs.push(FuncDecl::Body(sig, "id".to_owned(), id_body));

    // caller(x) = call_ref(x, ref.func id): provenance-tracked identity.
    let funcref = Type::Heap(WithNullable {
        value: HeapType::Sig { sig_index: sig },
        nullable: false,
    });
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let x = body.blocks[entry].params[0].1;
    let reference = body.add_op(entry, Operator::RefFunc { func_index: id }, &[], &[funcref]);
    let result = body.add_op(
        entry,
        Operator::CallRef { sig_index: sig },
        &[x, reference],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![result],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "caller".to_owned(), body));

    let native = NanoNativeConfig {
        native_functions: true,
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &native).expect("plans");
    assert!(outcome.native.decisions.iter().any(|decision| {
        decision.strategy == NanoCallStrategy::Direct && decision.callee == Some(PlanFuncId(0))
    }));
}

// ---------------------------------------------------------------------------
// Acceptance 3: required inlining fails closed, deterministically.
// ---------------------------------------------------------------------------

/// `factorial(n) = n <= 1 ? 1 : n * factorial(n - 1)`-shaped recursion:
/// a self-recursive direct call.
fn recursive_module() -> Module<'static> {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let n = body.blocks[entry].params[0].1;
    // We cannot reference the function before it exists; create with a
    // placeholder body first, then rewrite the call target. Simpler: create
    // the signature + a stub function, then build the real body calling the
    // stub's slot and replace the decl.
    module.funcs.push(FuncDecl::None(Default::default()));
    let me = portal_pc_waffle::Func::new(0_usize);
    let one = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let decremented = body.add_op(entry, Operator::I32Sub, &[n, one], &[Type::I32]);
    let called = body.add_op(
        entry,
        Operator::Call {
            function_index: me,
        },
        &[decremented],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![called],
    });
    module.funcs[me] = FuncDecl::Body(sig, "rec".to_owned(), body);
    module
}

#[test]
fn required_inlining_fails_closed_on_recursion() {
    let module = recursive_module();
    let config = NanoNativeConfig {
        native_functions: true,
        inlining: InliningMode::Required,
        required_inline: [PlanFuncId(0)].into_iter().collect(),
        ..NanoNativeConfig::default()
    };
    let errors = plan_nano_native(&module, &assumptions(), &config)
        .expect_err("required recursive inlining fails closed");
    assert!(errors
        .iter()
        .any(|error| error.kind == NanoNativeDiagnosticKind::RecursionProofFailure));
    // Determinism: the same failure, in the same order, every time.
    let again = plan_nano_native(&module, &assumptions(), &config)
        .expect_err("required recursive inlining fails closed");
    assert_eq!(errors, again);
}

#[test]
fn required_inlining_fails_closed_on_budget() {
    // A long acyclic chain exceeds a tiny cloned-block budget.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut previous = None;
    for index in 0..6 {
        let mut body = FunctionBody::new(&module, sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        // Give every function several blocks so the budget trips quickly.
        let header = body.add_block();
        let param = body.add_blockparam(header, Type::I32);
        body.set_terminator(entry, Terminator::Br {
            target: portal_pc_waffle::BlockTarget {
                block: header,
                args: vec![x],
            },
        });
        let result = match previous {
            Some(callee) => body.add_op(
                header,
                Operator::Call {
                    function_index: callee,
                },
                &[param],
                &[Type::I32],
            ),
            None => param,
        };
        body.set_terminator(header, Terminator::Return {
            values: vec![result],
        });
        previous = Some(module.funcs.push(FuncDecl::Body(
            sig,
            format!("f{index}"),
            body,
        )));
    }
    let _last = previous.expect("chain built");
    // Every callee that is actually called by a site (f0..f4) is required.
    let required: std::collections::BTreeSet<PlanFuncId> =
        (0..5).map(PlanFuncId).collect();
    let config = NanoNativeConfig {
        native_functions: true,
        inlining: InliningMode::Required,
        required_inline: required,
        inline_budget: InlineBudget {
            max_call_depth: 8,
            max_cloned_blocks: 1, // absurdly small: guaranteed to fail
            max_code_size: 4,
            max_work_units: 16,
        },
        ..NanoNativeConfig::default()
    };
    let errors = plan_nano_native(&module, &assumptions(), &config)
        .expect_err("required inlining over budget fails closed");
    assert!(errors
        .iter()
        .any(|error| error.kind == NanoNativeDiagnosticKind::RequiredInlineBudgetExceeded));
}

#[test]
fn opportunistic_inlining_falls_back_and_records() {
    let module = recursive_module();
    let config = NanoNativeConfig {
        native_functions: true,
        inlining: InliningMode::Opportunistic,
        ..NanoNativeConfig::default()
    };
    // Recursion is not an error in opportunistic mode: the call stays a
    // framed direct call, and the fallback is recorded.
    let outcome = plan_nano_native(&module, &assumptions(), &config).expect("opportunistic plans");
    assert!(outcome.native.inline_records.is_empty());
    let decision = &outcome.native.decisions[0];
    assert_eq!(decision.strategy, NanoCallStrategy::Framed);
    assert_eq!(decision.fallback, Some(NanoCallStrategy::Direct));
    assert!(decision
        .adaptation
        .as_deref()
        .is_some_and(|reason| reason.contains("recursion")));
    assert!(outcome
        .native
        .diagnostics
        .iter()
        .any(|d| d.kind == NanoNativeDiagnosticKind::InlineFallback));
}

#[test]
fn opportunistic_inlining_records_bounded_inline_schedules() {
    let module = direct_call_module();
    let config = NanoNativeConfig {
        native_functions: true,
        inlining: InliningMode::Opportunistic,
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &config).expect("plans");
    assert_eq!(outcome.native.inline_records.len(), 1);
    let record = &outcome.native.inline_records[0];
    assert_eq!(record.callee, PlanFuncId(0));
    assert_eq!(record.cloned_blocks, 1);
    assert!(!record.required);
    assert!(outcome
        .native
        .decisions
        .iter()
        .any(|decision| decision.strategy == NanoCallStrategy::Inline));
}

// ---------------------------------------------------------------------------
// Acceptance 4: tail calls frame-replace only when ABI/kinds match.
// ---------------------------------------------------------------------------

/// `caller(x) = tail_call callee(x)` with identical signatures.
fn tail_call_module() -> Module<'static> {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut callee_body = FunctionBody::new(&module, sig);
    let entry = callee_body.entry;
    let x = callee_body.blocks[entry].params[0].1;
    callee_body.set_terminator(entry, Terminator::Return { values: vec![x] });
    let callee = module
        .funcs
        .push(FuncDecl::Body(sig, "callee".to_owned(), callee_body));

    let mut caller_body = FunctionBody::new(&module, sig);
    let entry = caller_body.entry;
    let x = caller_body.blocks[entry].params[0].1;
    caller_body.set_terminator(entry, Terminator::ReturnCall {
        func: callee,
        args: vec![x],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "caller".to_owned(), caller_body));
    module
}

#[test]
fn tail_calls_frame_replace_only_when_abi_and_kinds_match() {
    let module = tail_call_module();
    let native = NanoNativeConfig {
        native_functions: true,
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &native).expect("plans");
    let decision = &outcome.native.decisions[0];
    assert_eq!(decision.strategy, NanoCallStrategy::TailDirect);

    // When the callee may throw and the caller cannot, frame replacement is
    // impossible: the call is framed and adapted.
    let throwing = NanoNativeConfig {
        native_functions: true,
        throwing_functions: [PlanFuncId(0)].into_iter().collect(),
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &throwing).expect("plans");
    let decision = &outcome.native.decisions[0];
    assert_eq!(decision.strategy, NanoCallStrategy::Framed);
    assert!(decision
        .adaptation
        .as_deref()
        .is_some_and(|note| note.contains("completion")));
    // The caller now transitively exposes Throw at this site.
    assert!(decision.completions.contains(&CompletionKind::Throw));
}

// ---------------------------------------------------------------------------
// Acceptance 5: exceptions and object writebacks are preserved.
// ---------------------------------------------------------------------------

/// g(x) -> aggregate; f(x) = g(g(x))-shaped nested calls over an arena
/// object, exercising transitive throws and object-parameter passing.
fn nested_object_module() -> Module<'static> {
    let mut module = Module::empty();
    let cell = module.signatures.push(SignatureData::Struct {
        fields: vec![WithMutablility {
            value: StorageType::Val(Type::I32),
            mutable: true,
        }],
        shared: false,
    });
    let cell_ref = Type::Heap(WithNullable {
        value: HeapType::Sig { sig_index: cell },
        nullable: false,
    });
    // touch(cell): increments cell.0 and returns the same cell (object
    // identity flows through).
    let sig = func_sig(&mut module, vec![cell_ref], vec![cell_ref]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let cell_value = body.blocks[entry].params[0].1;
    let current = body.add_op(
        entry,
        Operator::StructGet { sig: cell, idx: 0 },
        &[cell_value],
        &[Type::I32],
    );
    let one = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let incremented = body.add_op(entry, Operator::I32Add, &[current, one], &[Type::I32]);
    body.add_op(
        entry,
        Operator::StructSet { sig: cell, idx: 0 },
        &[cell_value, incremented],
        &[],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![cell_value],
    });
    let touch = module
        .funcs
        .push(FuncDecl::Body(sig, "touch".to_owned(), body));

    // outer(cell): touch(touch(cell)) — nested exception propagation and
    // repeated object-parameter writebacks.
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let cell_value = body.blocks[entry].params[0].1;
    let once = body.add_op(
        entry,
        Operator::Call {
            function_index: touch,
        },
        &[cell_value],
        &[cell_ref],
    );
    let twice = body.add_op(
        entry,
        Operator::Call {
            function_index: touch,
        },
        &[once],
        &[cell_ref],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![twice],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "outer".to_owned(), body));
    module
}

#[test]
fn nested_exceptions_and_object_writebacks_are_preserved() {
    let module = nested_object_module();
    let config = NanoNativeConfig {
        native_functions: true,
        throwing_functions: [PlanFuncId(0)].into_iter().collect(),
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &config).expect("plans");
    assert_eq!(outcome.native.decisions.len(), 2);
    for decision in &outcome.native.decisions {
        // Nested may-throw: both call sites expose Return + Throw.
        assert!(decision.completions.contains(&CompletionKind::Throw));
        assert!(decision.completions.contains(&CompletionKind::Return));
        // The object crosses both calls as one opaque arena reference:
        // object identity, aliasing, and the callee's writebacks are
        // preserved (no field copies, no writeback lists).
        assert_eq!(decision.arena_args, vec![0]);
        assert_eq!(decision.strategy, NanoCallStrategy::Direct);
    }
    // Arena handle lifetimes stay artifact-scoped in the plan.
    let outer = &outcome.backend.functions[1];
    assert!(outer
        .values
        .iter()
        .any(|value| value.representation == NanoValueRepresentation::ArenaRef
            && value.lifetime == NanoLifetime::Artifact));
}

// ---------------------------------------------------------------------------
// Acceptance 6: capability gating reports unsupported combinations.
// ---------------------------------------------------------------------------

#[test]
fn capability_gating_reports_unsupported_targets() {
    // The tail-call module requires tail-call support; WASM MVP has none.
    let module = tail_call_module();
    let config = NanoNativeConfig {
        targets: vec![TargetCapabilities::wasm_mvp()],
        ..NanoNativeConfig::default()
    };
    let errors = plan_nano_native(&module, &assumptions(), &config)
        .expect_err("wasm-mvp cannot lower tail calls");
    assert!(errors
        .iter()
        .any(|error| error.kind == NanoNativeDiagnosticKind::UnsupportedTargetCombination
            && error.message.contains("wasm-mvp")
            && error.message.contains("tail-calls")));

    // MiniBC is a parity target: direct calls, tail calls, and multi-result
    // returns are representable, never rejected.
    let config = NanoNativeConfig {
        native_functions: true,
        targets: vec![TargetCapabilities::minibc()],
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &config).expect("minibc supports all");
    assert_eq!(outcome.native.capability_checks.len(), 1);
    assert!(outcome.native.capability_checks[0].supported);
    assert!(outcome.native.decisions.iter().any(|decision| matches!(
        decision.strategy,
        NanoCallStrategy::TailDirect | NanoCallStrategy::Direct
    )));
}

#[test]
fn minibc_parity_multi_result_and_direct_calls_are_representable() {
    let mut module = Module::empty();
    let pair_sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32, Type::I32]);
    let mut producer_body = FunctionBody::new(&module, pair_sig);
    let entry = producer_body.entry;
    let x = producer_body.blocks[entry].params[0].1;
    producer_body.set_terminator(entry, Terminator::Return {
        values: vec![x, x],
    });
    module
        .funcs
        .push(FuncDecl::Body(pair_sig, "producer".to_owned(), producer_body));
    let config = NanoNativeConfig {
        targets: vec![TargetCapabilities::minibc()],
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &config).expect("plans");
    assert!(outcome.backend.runtime.uses_multi_result);
    assert!(outcome.native.capability_checks[0].supported);
}

#[test]
fn unused_parameters_are_elided_before_specialization() {
    // f(used, unused) = used; native mode must elide parameter 1 from the
    // specialization inputs it forms.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32, Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let used = body.blocks[entry].params[0].1;
    body.set_terminator(entry, Terminator::Return { values: vec![used] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "f".to_owned(), body));

    let native = NanoNativeConfig {
        native_functions: true,
        ..NanoNativeConfig::default()
    };
    let outcome = plan_nano_native(&module, &assumptions(), &native).expect("plans");
    assert_eq!(outcome.native.param_elisions.len(), 1);
    let elision = &outcome.native.param_elisions[0];
    assert_eq!(elision.function, PlanFuncId(0));
    assert_eq!(elision.live, vec![0]);
    assert_eq!(elision.elided, vec![1]);
}
