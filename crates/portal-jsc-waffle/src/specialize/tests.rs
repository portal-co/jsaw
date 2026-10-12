//! Tests for jsaw-owned specialization facts, keys, budgets, and the bounded
//! constant evaluator.

use super::*;
use crate::backend_plan::BackendPlan;
use portal_jsc_common::assumptions::{AssumptionId, AssumptionRegistry};
use portal_pc_waffle::{
    BlockTarget, Export, ExportKind, FuncDecl, FunctionBody, HeapType, Module, Operator,
    SignatureData, Terminator, Type, WithNullable,
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

fn assumptions(ids: &[AssumptionId]) -> AssumptionSet {
    AssumptionRegistry::new()
        .resolve(ids.iter().copied())
        .expect("assumptions resolve")
}

fn empty_assumptions() -> AssumptionSet {
    AssumptionSet::empty()
}

/// `(i32) -> i32` returning `x + 1`, exported as `name`.
fn increment_module() -> (Module<'static>, BackendPlan) {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let x = body.blocks[entry].params[0].1;
    let one = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let sum = body.add_op(entry, Operator::I32Add, &[x, one], &[Type::I32]);
    body.set_terminator(entry, Terminator::Return { values: vec![sum] });
    let func = module
        .funcs
        .push(FuncDecl::Body(sig, "inc".to_owned(), body));
    module.exports.push(Export {
        name: "inc".to_owned(),
        kind: ExportKind::Func(func),
    });
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    (module, plan)
}

// ---------------------------------------------------------------------------
// Key determinism
// ---------------------------------------------------------------------------

#[test]
fn keys_are_order_independent_and_assumption_sensitive() {
    let (_, plan) = increment_module();
    let function = PlanFuncId(0);
    let live: BTreeSet<u32> = [0].into_iter().collect();

    // Same logical facts, different insertion order (including a duplicate
    // index with a conflicting fact) -> identical keys.
    let key_a = SpecializationKey::form(
        function,
        vec![
            (0, ValueFact::Constant(PlanConst::I32(7))),
            (0, ValueFact::Unknown),
        ],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    let key_b = SpecializationKey::form(
        function,
        vec![
            (0, ValueFact::Unknown),
            (0, ValueFact::Constant(PlanConst::I32(7))),
        ],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    assert_eq!(key_a, key_b);
    assert_eq!(key_a.fingerprint(), key_b.fingerprint());
    assert_eq!(key_a.fingerprint().len(), 64);

    // The assumption digest participates in the key.
    let with_no_jit = SpecializationKey::form(
        function,
        vec![(0, ValueFact::Constant(PlanConst::I32(7)))],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &assumptions(&[AssumptionId::NoJit]),
        None,
    );
    assert_ne!(key_a.fingerprint(), with_no_jit.fingerprint());
    assert_eq!(
        with_no_jit.assumption_digest,
        assumptions(&[AssumptionId::NoJit]).digest()
    );

    // A different plan function changes the key.
    let _ = plan;
}

#[test]
fn unused_parameters_are_elided_before_key_formation() {
    // Function `(used, unused) -> i32` where only parameter 0 is read.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32, Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let used = body.blocks[entry].params[0].1;
    body.set_terminator(entry, Terminator::Return { values: vec![used] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "f".to_owned(), body));
    let plan = BackendPlan::from_module(&module).expect("valid plan");

    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    assert_eq!(facts.used_params, vec![0], "parameter 1 is unused");

    let live: BTreeSet<u32> = facts.used_params.iter().copied().collect();
    let key_with_unused_fact = SpecializationKey::form(
        PlanFuncId(0),
        vec![
            (0, ValueFact::Constant(PlanConst::I32(3))),
            (1, ValueFact::Constant(PlanConst::I32(99))),
        ],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    let key_without_unused_fact = SpecializationKey::form(
        PlanFuncId(0),
        vec![(0, ValueFact::Constant(PlanConst::I32(3)))],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    assert_eq!(
        key_with_unused_fact.fingerprint(),
        key_without_unused_fact.fingerprint(),
        "unused-parameter facts never change the key"
    );
    assert_eq!(key_with_unused_fact.params.len(), 1);
}

#[test]
fn boundary_liveness_elides_unused_context_this_arguments() {
    let used: BoundaryLiveness = BoundaryLiveness {
        context: false,
        this: true,
        arguments: false,
    };
    let unused = BoundaryLiveness::default();
    let function = PlanFuncId(0);
    let live: BTreeSet<u32> = [1].into_iter().collect();
    let make = |boundary: BoundaryLiveness| {
        SpecializationKey::form(
            function,
            vec![(1, ValueFact::Constant(PlanConst::I32(1)))],
            &live,
            boundary,
            vec![],
            vec![CompletionKind::Return],
            None,
            &empty_assumptions(),
            None,
        )
    };
    assert_ne!(make(used).fingerprint(), make(unused).fingerprint());
    assert!(make(used).boundary.this);
}

// ---------------------------------------------------------------------------
// Object facts
// ---------------------------------------------------------------------------

#[test]
fn object_facts_retain_shape_alias_arena_flattened_and_waffle_ids() {
    let fact = ValueFact::Object(ObjectFact {
        shape: PlanSigId(2),
        alias: AliasClass::Unaliased,
        mutation: MutationClass::Immutable,
        arena_opaque: false,
        flattened_fields: vec![
            FlattenedFieldFact {
                field: 0,
                fact: ValueFact::Constant(PlanConst::I32(4)),
                waffle_value: Some(17),
            },
            FlattenedFieldFact {
                field: 1,
                fact: ValueFact::Unknown,
                waffle_value: Some(19),
            },
        ],
    });
    // Facts serialize deterministically and retain every component.
    let json = serde_json::to_string(&fact).expect("serialize");
    let roundtrip: ValueFact = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(fact, roundtrip);

    // Keys carry the object fact (canonical shape/alias/arena/flattened data)
    // but never the session-local Waffle identity.
    let live: BTreeSet<u32> = [0].into_iter().collect();
    let key_a = SpecializationKey::form(
        PlanFuncId(0),
        vec![(0, fact.clone())],
        &live,
        BoundaryLiveness::default(),
        vec![],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    let mut rewritten = fact.clone();
    if let ValueFact::Object(object) = &mut rewritten {
        object.flattened_fields[0].waffle_value = Some(9999);
    }
    let key_b = SpecializationKey::form(
        PlanFuncId(0),
        vec![(0, rewritten)],
        &live,
        BoundaryLiveness::default(),
        vec![],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    assert_ne!(
        key_a.fingerprint(),
        key_b.fingerprint(),
        "flattened-field payloads participate in the key"
    );

    // The session-local Waffle identity lives alongside facts, outside keys.
    let mut waffle = WaffleIdentity {
        func: 3,
        values: BTreeMap::new(),
    };
    waffle.values.insert(0, 42);
    let mut facts = FunctionFacts {
        function: PlanFuncId(0),
        ..FunctionFacts::default()
    };
    facts.waffle = Some(waffle.clone());
    facts.values.insert(0, fact);
    assert_eq!(facts.waffle.as_ref().expect("waffle ids").values[&0], 42);
}

// ---------------------------------------------------------------------------
// Bounded constant evaluator
// ---------------------------------------------------------------------------

/// Build a chain of nested direct calls: f0(x) -> f1(x) -> f2(x) -> f3(x),
/// where f3 returns x + 10, f2 returns f3(x) + 100, f1 returns f2(x) + 1000,
/// f0 returns f1(x) + 10000.
fn nested_call_chain() -> (Module<'static>, BackendPlan) {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut funcs = Vec::new();
    // Build from the inside out so callers can reference callees.
    // f3: x + 10
    {
        let mut body = FunctionBody::new(&module, sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        let ten = body.add_op(entry, Operator::I32Const { value: 10 }, &[], &[Type::I32]);
        let sum = body.add_op(entry, Operator::I32Add, &[x, ten], &[Type::I32]);
        body.set_terminator(entry, Terminator::Return { values: vec![sum] });
        funcs.push(module.funcs.push(FuncDecl::Body(sig, "f3".to_owned(), body)));
    }
    for (name, addend) in [("f2", 100_u32), ("f1", 1000), ("f0", 10000)] {
        let callee = *funcs.last().unwrap();
        let mut body = FunctionBody::new(&module, sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        let called = body.add_op(
            entry,
            Operator::Call {
                function_index: callee,
            },
            &[x],
            &[Type::I32],
        );
        let extra = body.add_op(entry, Operator::I32Const { value: addend }, &[], &[Type::I32]);
        let sum = body.add_op(entry, Operator::I32Add, &[called, extra], &[Type::I32]);
        body.set_terminator(entry, Terminator::Return { values: vec![sum] });
        funcs.push(module.funcs.push(FuncDecl::Body(sig, name.to_owned(), body)));
    }
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    (module, plan)
}

#[test]
fn evaluator_propagates_through_nested_call_site_environments() {
    let (_, plan) = nested_call_chain();
    // f0 is the last function.
    let f0 = PlanFuncId(3);
    let mut root: BTreeMap<u32, ValueFact> = BTreeMap::new();
    root.insert(0, ValueFact::Constant(PlanConst::I32(1)));
    let facts = analyze_function(
        &plan,
        f0,
        root,
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    // 1 + 10 + 100 + 1000 + 10000 = 11111, propagated through three nested
    // call-site environments.
    assert_eq!(
        facts.results,
        vec![ValueFact::Constant(PlanConst::I32(11111))],
        "three nested environments propagate constants"
    );
    assert!(facts.eval.steps_used <= EVAL_STEP_BOUND);
    // Direct calls recorded as proven callees.
    assert!(!facts.proven_callees.is_empty());
}

#[test]
fn evaluator_depth_bound_fails_closed() {
    // A chain deeper than MAX_ENV_DEPTH: evaluation must terminate with
    // Unknown rather than receding forever.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut previous = None;
    for depth in 0..8 {
        let mut body = FunctionBody::new(&module, sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        let result = match previous {
            Some(callee) => body.add_op(
                entry,
                Operator::Call {
                    function_index: callee,
                },
                &[x],
                &[Type::I32],
            ),
            None => x,
        };
        body.set_terminator(entry, Terminator::Return { values: vec![result] });
        previous = Some(module.funcs.push(FuncDecl::Body(
            sig,
            format!("depth{depth}"),
            body,
        )));
    }
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let mut root: BTreeMap<u32, ValueFact> = BTreeMap::new();
    root.insert(0, ValueFact::Constant(PlanConst::I32(1)));
    let facts = analyze_function(
        &plan,
        PlanFuncId(7),
        root,
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    // Beyond the depth bound the fact coarsens to Unknown (fail-closed),
    // never a guess.
    assert_eq!(facts.results, vec![ValueFact::Unknown]);
    assert!(facts.eval.steps_used <= EVAL_STEP_BOUND + 8);
}

#[test]
fn wrapping_arithmetic_requires_the_explicit_flag() {
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let max = body.add_op(
        entry,
        Operator::I32Const { value: i32::MAX as u32 },
        &[],
        &[Type::I32],
    );
    let one = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let sum = body.add_op(entry, Operator::I32Add, &[max, one], &[Type::I32]);
    body.set_terminator(entry, Terminator::Return { values: vec![sum] });
    module
        .funcs
        .push(FuncDecl::Body(sig, "overflow".to_owned(), body));
    let plan = BackendPlan::from_module(&module).expect("valid plan");

    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    assert_eq!(
        facts.results,
        vec![ValueFact::Unknown],
        "overflow is unknown without the wrapping flag"
    );

    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::Allowed,
    );
    assert_eq!(
        facts.results,
        vec![ValueFact::Constant(PlanConst::I32(i32::MIN as u32))],
        "with the flag, two's-complement wrapping is applied"
    );
}

#[test]
fn evaluator_cycles_and_join_disagreement_are_unknown() {
    // Loop: header param counter comes from entry (0) and the backedge
    // (counter + 1). The cycle makes the fact unknown, deterministically.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let input = body.blocks[entry].params[0].1;
    let header = body.add_block();
    let counter = body.add_blockparam(header, Type::I32);
    let one = body.add_op(header, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let next = body.add_op(header, Operator::I32Add, &[counter, one], &[Type::I32]);
    let exit = body.add_block();
    let exit_param = body.add_blockparam(exit, Type::I32);
    body.set_terminator(entry, Terminator::Br {
        target: BlockTarget {
            block: header,
            args: vec![input],
        },
    });
    body.set_terminator(header, Terminator::CondBr {
        cond: input,
        if_true: BlockTarget {
            block: exit,
            args: vec![next],
        },
        if_false: BlockTarget {
            block: header,
            args: vec![next],
        },
    });
    body.set_terminator(exit, Terminator::Return {
        values: vec![exit_param],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "loop".to_owned(), body));
    let plan = BackendPlan::from_module(&module).expect("valid plan");

    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    // The loop-carried parameter is cyclic: unknown, never a guess.
    let header_param = plan.functions[0].blocks[1].params[0];
    assert_eq!(
        facts.values[&(header_param.index() as u32)],
        ValueFact::Unknown
    );

    // Join disagreement: two constant predecessors that disagree.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let cond = body.blocks[entry].params[0].1;
    let left = body.add_block();
    let right = body.add_block();
    let join = body.add_block();
    let merged = body.add_blockparam(join, Type::I32);
    let one = body.add_op(left, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let two = body.add_op(right, Operator::I32Const { value: 2 }, &[], &[Type::I32]);
    body.set_terminator(entry, Terminator::CondBr {
        cond,
        if_true: BlockTarget {
            block: left,
            args: vec![],
        },
        if_false: BlockTarget {
            block: right,
            args: vec![],
        },
    });
    body.set_terminator(left, Terminator::Br {
        target: BlockTarget {
            block: join,
            args: vec![one],
        },
    });
    body.set_terminator(right, Terminator::Br {
        target: BlockTarget {
            block: join,
            args: vec![two],
        },
    });
    body.set_terminator(join, Terminator::Return {
        values: vec![merged],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "join".to_owned(), body));
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    let join_param = plan.functions[0].blocks[3].params[0];
    assert_eq!(
        facts.values[&(join_param.index() as u32)],
        ValueFact::Unknown,
        "disagreeing predecessors join to Unknown"
    );
}

#[test]
fn evaluator_step_bound_and_unsupported_values_fail_closed() {
    // A long dependency chain exceeds the 64-step bound.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![], vec![Type::I32]);
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let mut current = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    for _ in 0..100 {
        let one = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        current = body.add_op(entry, Operator::I32Add, &[current, one], &[Type::I32]);
    }
    body.set_terminator(entry, Terminator::Return {
        values: vec![current],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "chain".to_owned(), body));
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    assert!(
        facts.eval.steps_used <= EVAL_STEP_BOUND + 8,
        "the walk is bounded ({} steps)",
        facts.eval.steps_used
    );

    // Unsupported values (loads, indirect calls) stay unknown.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let memory = module.memories.push(portal_pc_waffle::MemoryData {
        initial_pages: 1,
        maximum_pages: None,
        segments: vec![],
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    let mut body = FunctionBody::new(&module, sig);
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let loaded = body.add_op(
        entry,
        Operator::I32Load {
            memory: portal_pc_waffle::MemoryArg {
                align: 2,
                offset: 0,
                memory,
            },
        },
        &[address],
        &[Type::I32],
    );
    body.set_terminator(entry, Terminator::Return {
        values: vec![loaded],
    });
    module
        .funcs
        .push(FuncDecl::Body(sig, "load".to_owned(), body));
    let plan = BackendPlan::from_module(&module).expect("valid plan");
    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    assert_eq!(facts.results, vec![ValueFact::Unknown]);
}

// ---------------------------------------------------------------------------
// Budgets and coarsening
// ---------------------------------------------------------------------------

#[test]
fn variant_budgets_are_bounded_deterministic_and_coarsen() {
    let (_, plan) = increment_module();
    let budget = VariantBudget {
        max_variants_per_function: 3,
        max_variants_per_identity: 1,
        max_global_variants: 8,
        max_work_units: 64,
        max_code_size: 1024,
    };
    let mut specializer = Specializer::new(
        &plan,
        &empty_assumptions(),
        budget,
        SpecializationMode::NanoVariants,
    );
    let live: BTreeSet<u32> = [0].into_iter().collect();
    let key = |value: u32| {
        SpecializationKey::form(
            PlanFuncId(0),
            vec![(0, ValueFact::Constant(PlanConst::I32(value)))],
            &live,
            BoundaryLiveness::default(),
            vec![ValueFact::Unknown],
            vec![CompletionKind::Return],
            None,
            &empty_assumptions(),
            None,
        )
    };

    // Generic + two specialized variants fit; the third specialized request
    // exceeds the per-function budget (generic counts as one variant).
    assert!(matches!(
        specializer.request_variant(&key(1)),
        Ok(VariantRequest::Created(_))
    ));
    assert!(matches!(
        specializer.request_variant(&key(2)),
        Ok(VariantRequest::Created(_))
    ));
    let third = specializer.request_variant(&key(3)).expect("not an error");
    assert!(
        matches!(third, VariantRequest::Coarsened { .. }),
        "over-budget requests coarsen: {third:?}"
    );
    // Re-requesting an existing key is idempotent.
    assert!(matches!(
        specializer.request_variant(&key(1)),
        Ok(VariantRequest::Existing(_))
    ));

    let (variants, diagnostics, usage, coarsenings) = specializer.finish();
    assert!(
        variants.len() <= budget.max_variants_per_function as usize,
        "variants stay bounded: {}",
        variants.len()
    );
    assert!(diagnostics
        .iter()
        .any(|d| d.kind == SpecializationDiagnosticKind::BudgetExceeded));
    assert_eq!(coarsenings.len(), 1, "coarsening is observable");
    assert_eq!(usage.coarsened_requests, 1);
    assert!(variants.iter().any(|variant| variant.generic));
}

#[test]
fn variant_requests_validate_the_assumption_digest() {
    let (_, plan) = increment_module();
    let mut specializer = Specializer::new(
        &plan,
        &empty_assumptions(),
        VariantBudget::default(),
        SpecializationMode::NanoVariants,
    );
    let live: BTreeSet<u32> = [0].into_iter().collect();
    let foreign_key = SpecializationKey::form(
        PlanFuncId(0),
        vec![(0, ValueFact::Constant(PlanConst::I32(1)))],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &assumptions(&[AssumptionId::NoJit]),
        None,
    );
    let error = specializer
        .request_variant(&foreign_key)
        .expect_err("mismatched digests fail closed");
    assert_eq!(error.kind, SpecializationDiagnosticKind::InvalidRequest);
    let unknown_function = SpecializationKey::form(
        PlanFuncId(99),
        vec![],
        &live,
        BoundaryLiveness::default(),
        vec![],
        vec![],
        None,
        &empty_assumptions(),
        None,
    );
    assert!(specializer.request_variant(&unknown_function).is_err());
}

// ---------------------------------------------------------------------------
// CoreGC facts-only adapter and generic fallback
// ---------------------------------------------------------------------------

#[test]
fn coregc_keeps_facts_without_forced_variants() {
    let (_, plan) = increment_module();
    let outcome = analyze(&plan, &empty_assumptions());
    assert_eq!(outcome.facts.len(), 1);
    // The constant `1` in the body is a fact CoreGC may use as a runtime
    // fast-path hint.
    let coregc = CoreGcFacts::from_facts(&outcome.facts);
    assert!(coregc
        .fast_paths
        .iter()
        .any(|hint| hint.hint == FastPathHint::Constant(PlanConst::I32(1))));

    // Variant requests in facts-only mode never materialize variants.
    let mut specializer = Specializer::new(
        &plan,
        &empty_assumptions(),
        VariantBudget::default(),
        SpecializationMode::PreserveFactsOnly,
    );
    let live: BTreeSet<u32> = [0].into_iter().collect();
    let key = SpecializationKey::form(
        PlanFuncId(0),
        vec![(0, ValueFact::Constant(PlanConst::I32(1)))],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    assert_eq!(
        specializer.request_variant(&key).expect("ok"),
        VariantRequest::Coarsened { generic: None }
    );
    assert!(specializer.generic_variant(PlanFuncId(0)).is_none());
    let (variants, diagnostics, _, _) = specializer.finish();
    assert!(variants.is_empty(), "CoreGC never materializes variants");
    assert!(diagnostics
        .iter()
        .any(|d| d.kind == SpecializationDiagnosticKind::GenericFallback));
}

#[test]
fn dynamic_function_references_stay_generic() {
    // call_ref through a non-proven reference: no callee identity is claimed.
    let mut module = Module::empty();
    let sig = func_sig(&mut module, vec![Type::I32], vec![Type::I32]);
    let funcref = Type::Heap(WithNullable {
        value: HeapType::Sig { sig_index: sig },
        nullable: true,
    });
    // caller(ref, x): call_ref(x, ref)
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
    let plan = BackendPlan::from_module(&module).expect("valid plan");

    let facts = analyze_function(
        &plan,
        PlanFuncId(0),
        BTreeMap::new(),
        BoundaryLiveness::default(),
        WrappingArithmetic::FailClosed,
    );
    assert!(
        facts.proven_callees.is_empty(),
        "call_ref sites have no proven identity"
    );
    assert_eq!(facts.results, vec![ValueFact::Unknown]);

    // A variant request without proven identity must not claim one.
    let live: BTreeSet<u32> = [1].into_iter().collect();
    let key = SpecializationKey::form(
        PlanFuncId(0),
        vec![(1, ValueFact::Unknown)],
        &live,
        BoundaryLiveness::default(),
        vec![ValueFact::Unknown],
        vec![CompletionKind::Return],
        None,
        &empty_assumptions(),
        None,
    );
    assert_eq!(key.callee_identity, None);
}

#[test]
fn analyze_is_deterministic() {
    let (_, plan) = increment_module();
    let first = analyze(&plan, &empty_assumptions());
    let second = analyze(&plan, &empty_assumptions());
    assert_eq!(first.manifest.fingerprint(), second.manifest.fingerprint());
    assert_eq!(
        first.manifest.assumption_digest,
        empty_assumptions().digest()
    );
    assert_eq!(first.manifest.plan_fingerprint, plan.fingerprint());
}
