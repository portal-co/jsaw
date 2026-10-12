//! Nano native-function mode and bounded full inlining (DreamComp #19, #27).
//!
//! This module is an **optional nano lowering configuration**, consumed while
//! nano lowers a WasmGC module, before target emitters consume the
//! `BackendPlan`. It is not a CoreGC semantic flag: with the default
//! configuration every call site stays on the fixed adapter ABI and nothing
//! is inlined, which is exactly the pre-existing nano behavior.
//!
//! Contract (per the #19 resolution):
//!
//! - The fixed adapter ABI `(context, this, arguments-array) -> value`,
//!   source-function arity, spread handling, and ordinary function-value
//!   wrapping are preserved; the record is embedded in every plan.
//! - Direct/local calls are selected only for module-known or
//!   provenance-tracked (`ref.func`) function identity. Rebound or uncertain
//!   references keep a tag-guarded identity check with the generic adapter
//!   fallback.
//! - Unused parameters are elided *before* specialization keys are formed;
//!   proven identity enters keys only when it is module-known or
//!   provenance-tracked.
//! - Full inlining clones nothing here — it records a bounded, validated
//!   inline schedule an emitter executes. Required inlining fails closed on
//!   budget or recursion-proof failure; opportunistic inlining falls back to
//!   a framed/direct call with ABI adaptation, recorded in the plan.
//! - Exception propagation (transitive may-throw completion sets), object
//!   identity/aliasing (opaque `ArenaRef` parameter passing, never field
//!   copies), arena handle lifetimes, and return-shape adaptation are
//!   preserved explicitly in every call decision.
//! - Capability gating reports unsupported target combinations as structured
//!   diagnostics instead of claiming an invalid plan. MiniBC-relevant
//!   operations (direct calls, tail calls, multi-result returns) are always
//!   representable; gating never rewrites or rejects them from the plan.

use portal_jsc_common::assumptions::AssumptionSet;
use portal_pc_waffle::Module;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt;

use crate::backend_plan::{
    BackendPlan, BackendPlanDiagnostic, CompletionKind, PlanCallKind, PlanFuncId, PlanTerminator,
};
use crate::nano::NanoValueRepresentation;
use crate::specialize::{
    BoundaryLiveness, SpecializationKey, SpecializationManifest, SpecializationMode, Specializer,
    ValueFact, VariantBudget, VariantRequest, WrappingArithmetic, analyze_function,
};

/// Schema identifier for nano-native lowering plans.
pub const NANO_NATIVE_SCHEMA: &str = "jsaw.nano-native.v1";

/// The fixed adapter ABI: `(context, this, arguments-array) -> value`.
///
/// This contract is invariant under every nano-native configuration; the
/// record exists so emitters and auditors can see that native mode did not
/// replace the boundary ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdapterAbiRecord {
    pub params: [String; 3],
    pub result: String,
    /// Spread handling is fixed: the arguments array carries source-level
    /// spreads; native mode never changes the source function's arity.
    pub arguments_array_spread: bool,
    pub source_arity_preserved: bool,
}

impl Default for AdapterAbiRecord {
    fn default() -> Self {
        Self {
            params: [
                "context".to_owned(),
                "this".to_owned(),
                "arguments".to_owned(),
            ],
            result: "value".to_owned(),
            arguments_array_spread: true,
            source_arity_preserved: true,
        }
    }
}

// ---------------------------------------------------------------------------
// Target capabilities
// ---------------------------------------------------------------------------

/// What a target backend can lower. MiniBC is a parity target: it supports
/// every canonical operation (direct and tail calls and multi-result returns
/// included) and is never an unsupported-operation escape hatch.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TargetCapabilities {
    pub name: String,
    pub direct_calls: bool,
    pub tail_calls: bool,
    pub indirect_calls: bool,
    pub ref_calls: bool,
    pub multi_result: bool,
    pub adapter_calls: bool,
    pub arena: bool,
}

impl TargetCapabilities {
    /// Pure core Wasm (MVP): no tail-call or function-reference proposals.
    pub fn wasm_mvp() -> Self {
        Self {
            name: "wasm-mvp".to_owned(),
            direct_calls: true,
            tail_calls: false,
            indirect_calls: true,
            ref_calls: false,
            multi_result: true,
            adapter_calls: true,
            arena: true,
        }
    }

    pub fn c() -> Self {
        Self {
            name: "c".to_owned(),
            direct_calls: true,
            tail_calls: true,
            indirect_calls: true,
            ref_calls: true,
            multi_result: true,
            adapter_calls: true,
            arena: true,
        }
    }

    pub fn rust() -> Self {
        Self {
            name: "rust".to_owned(),
            ..Self::c()
        }
    }

    /// MiniBC: parity target with the full canonical operation set.
    pub fn minibc() -> Self {
        Self {
            name: "minibc".to_owned(),
            direct_calls: true,
            tail_calls: true,
            indirect_calls: true,
            ref_calls: true,
            multi_result: true,
            adapter_calls: true,
            arena: true,
        }
    }

    fn missing(&self, required: &RequiredCapabilities) -> Vec<String> {
        let mut missing = Vec::new();
        let mut check = |have: bool, need: bool, what: &str| {
            if need && !have {
                missing.push(what.to_owned());
            }
        };
        check(self.direct_calls, required.direct_calls, "direct-calls");
        check(self.tail_calls, required.tail_calls, "tail-calls");
        check(self.indirect_calls, required.indirect_calls, "indirect-calls");
        check(self.ref_calls, required.ref_calls, "ref-calls");
        check(self.multi_result, required.multi_result, "multi-result");
        check(self.adapter_calls, required.adapter_calls, "adapter-calls");
        check(self.arena, required.arena, "arena");
        missing
    }
}

/// Capabilities a plan plus its decisions actually require.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct RequiredCapabilities {
    pub direct_calls: bool,
    pub tail_calls: bool,
    pub indirect_calls: bool,
    pub ref_calls: bool,
    pub multi_result: bool,
    pub adapter_calls: bool,
    pub arena: bool,
}

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

/// Inlining policy. `Required` applies to `required_inline` callees;
/// `Opportunistic` attempts every inlinable direct call and records
/// fallbacks.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InliningMode {
    #[default]
    Off,
    Opportunistic,
    Required,
}

/// Deterministic inline budgets.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InlineBudget {
    pub max_call_depth: u32,
    pub max_cloned_blocks: u32,
    pub max_code_size: u32,
    pub max_work_units: u32,
}

impl Default for InlineBudget {
    fn default() -> Self {
        Self {
            max_call_depth: 4,
            max_cloned_blocks: 128,
            max_code_size: 4096,
            max_work_units: 1024,
        }
    }
}

/// The optional nano lowering configuration. All defaults are off: the
/// default configuration is byte-for-byte the pre-existing nano behavior.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct NanoNativeConfig {
    /// Enable native (direct/guarded) call selection from proven identity.
    pub native_functions: bool,
    pub inlining: InliningMode,
    /// Callees that *must* be inlined at every direct call site when
    /// `inlining` is `Required`; failure is a fail-closed diagnostic.
    pub required_inline: BTreeSet<PlanFuncId>,
    pub inline_budget: InlineBudget,
    pub variant_budget: Option<VariantBudget>,
    /// Explicit compiler-control gate for wrapping arithmetic in the constant
    /// evaluator; off by default.
    pub wrapping_arithmetic: bool,
    /// Functions whose bodies may complete with `Throw` (jsaw exception
    /// results are ordinary aggregates, so the seed is declared by the
    /// lowering that owns the adapter convention). Transitive may-throw
    /// propagation is computed from these seeds plus structural traps.
    pub throwing_functions: BTreeSet<PlanFuncId>,
    /// Targets the artifact must support. Gating is fail-closed: an
    /// unsupported combination is a structured diagnostic, never a silently
    /// invalid plan.
    pub targets: Vec<TargetCapabilities>,
}

// ---------------------------------------------------------------------------
// Decisions and records
// ---------------------------------------------------------------------------

/// How one call site is lowered.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NanoCallStrategy {
    /// Fixed adapter ABI dispatch (the default path).
    GenericAdapter,
    /// Tag-guarded identity check with generic adapter fallback: the
    /// reference was retained but not fully proven.
    GuardedAdapter,
    /// Module-known or provenance-tracked direct call.
    Direct,
    /// Direct call with frame replacement: return ABI and completion kinds
    /// match exactly.
    TailDirect,
    /// Framed call with return-shape adaptation (opportunistic-inline or
    /// tail-incompatible fallback).
    Framed,
    /// The body is cloned inline per the recorded inline schedule.
    Inline,
}

/// One call-site decision.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct NanoCallDecision {
    pub function: PlanFuncId,
    pub block: u32,
    /// Instruction index, or `None` for a terminator (tail-call) site.
    pub inst: Option<u32>,
    pub strategy: NanoCallStrategy,
    /// Proven callee identity (module-known or `ref.func` provenance).
    pub callee: Option<PlanFuncId>,
    /// The full transitive completion set exposed by this call.
    pub completions: Vec<CompletionKind>,
    /// Argument positions passed as opaque `ArenaRef` handles (object
    /// identity and writebacks are preserved; no field copies exist).
    pub arena_args: Vec<u32>,
    /// Fallback retained for this site, if any.
    pub fallback: Option<NanoCallStrategy>,
    /// Return-shape adaptation applied at this site, if any.
    pub adaptation: Option<String>,
}

/// One validated inline step.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InlineRecord {
    pub function: PlanFuncId,
    pub block: u32,
    pub inst: Option<u32>,
    pub callee: PlanFuncId,
    pub depth: u32,
    pub cloned_blocks: u32,
    pub required: bool,
}

/// Unused-parameter elision (before specialization key formation).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParamElision {
    pub function: PlanFuncId,
    pub live: Vec<u32>,
    pub elided: Vec<u32>,
}

/// Per-target capability check.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapabilityCheck {
    pub target: String,
    pub supported: bool,
    pub unsupported: Vec<String>,
}

/// Structured nano-native diagnostic kinds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NanoNativeDiagnosticKind {
    /// The underlying BackendPlan failed (wrapped diagnostics).
    PlanError,
    UnsupportedTargetCombination,
    RequiredInlineBudgetExceeded,
    RecursionProofFailure,
    /// Opportunistic inline fell back to a framed/direct call (recorded,
    /// not an error).
    InlineFallback,
    /// An uncertain reference retained the guarded adapter path (recorded,
    /// not an error).
    GuardedFallback,
}

/// One structured nano-native diagnostic.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct NanoNativeDiagnostic {
    pub kind: NanoNativeDiagnosticKind,
    pub function: Option<PlanFuncId>,
    pub message: String,
    /// Wrapped BackendPlan diagnostics when `kind == PlanError`.
    pub plan_diagnostics: Vec<BackendPlanDiagnostic>,
}

impl fmt::Display for NanoNativeDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.message)
    }
}

/// The nano-native lowering sidecar, consumed with the `BackendPlan` by
/// target emitters.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct NanoNativePlan {
    pub schema: String,
    pub native_functions: bool,
    pub inlining: InliningMode,
    pub adapter_abi: AdapterAbiRecord,
    pub param_elisions: Vec<ParamElision>,
    pub decisions: Vec<NanoCallDecision>,
    pub inline_records: Vec<InlineRecord>,
    pub capability_checks: Vec<CapabilityCheck>,
    pub diagnostics: Vec<NanoNativeDiagnostic>,
    pub specialization: SpecializationManifest,
}

/// The outcome of nano-native planning: the stable BackendPlan plus the
/// nano-native sidecar.
#[derive(Clone, Debug)]
pub struct NanoNativeOutcome {
    pub backend: BackendPlan,
    pub native: NanoNativePlan,
}

// ---------------------------------------------------------------------------
// Planning
// ---------------------------------------------------------------------------

/// Transitive completion sets over the direct-call graph: every function
/// exposes its own structural completions plus the completions of every
/// transitively called function (a transitively throwing callee exposes
/// `Throw` at every caller).
fn transitive_completions(
    plan: &BackendPlan,
    throwing: &BTreeSet<PlanFuncId>,
) -> BTreeMap<PlanFuncId, Vec<CompletionKind>> {
    let mut edges: BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>> = BTreeMap::new();
    for (index, func) in plan.functions.iter().enumerate() {
        let mut callees = BTreeSet::new();
        for block in &func.blocks {
            for inst in &block.insts {
                if let Some(call) = &inst.call {
                    if let PlanCallKind::Direct { callee } = call.kind {
                        callees.insert(callee);
                    }
                }
            }
            if let PlanTerminator::TailCall {
                call: PlanCallKind::Direct { callee },
                ..
            } = &block.terminator
            {
                callees.insert(*callee);
            }
        }
        edges.insert(PlanFuncId(index as u32), callees);
    }
    let mut completions: BTreeMap<PlanFuncId, BTreeSet<CompletionKind>> = BTreeMap::new();
    for (index, func) in plan.functions.iter().enumerate() {
        let id = PlanFuncId(index as u32);
        let mut set: BTreeSet<CompletionKind> = func.completions.kinds.iter().copied().collect();
        if throwing.contains(&id) {
            set.insert(CompletionKind::Throw);
        }
        completions.insert(id, set);
    }
    // Bounded fixpoint over the finite completion lattice.
    let mut changed = true;
    while changed {
        changed = false;
        for (func, callees) in &edges {
            let mut merged = completions.get(func).cloned().unwrap_or_default();
            for callee in callees {
                if let Some(callee_kinds) = completions.get(callee) {
                    merged.extend(callee_kinds.iter().copied());
                }
            }
            if completions.get(func) != Some(&merged) {
                completions.insert(*func, merged);
                changed = true;
            }
        }
    }
    completions
        .into_iter()
        .map(|(func, kinds)| (func, kinds.into_iter().collect()))
        .collect()
}

/// Direct-call edges of the whole module.
fn direct_call_edges(plan: &BackendPlan) -> BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>> {
    let mut edges: BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>> = BTreeMap::new();
    for (index, func) in plan.functions.iter().enumerate() {
        let mut callees = BTreeSet::new();
        for block in &func.blocks {
            for inst in &block.insts {
                if let Some(call) = &inst.call {
                    if let PlanCallKind::Direct { callee } = call.kind {
                        callees.insert(callee);
                    }
                }
            }
        }
        edges.insert(PlanFuncId(index as u32), callees);
    }
    edges
}

/// Transitive reachability over direct-call edges (for recursion proofs).
/// The origin is included only when it is genuinely reached through an edge,
/// so `reachable(f).contains(f)` is exactly "f is recursive".
fn reachable(
    edges: &BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>>,
    from: PlanFuncId,
) -> BTreeSet<PlanFuncId> {
    let mut seen = BTreeSet::new();
    let mut queue: VecDeque<PlanFuncId> = edges
        .get(&from)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .collect();
    while let Some(current) = queue.pop_front() {
        if !seen.insert(current) {
            continue;
        }
        if let Some(next) = edges.get(&current) {
            for callee in next {
                queue.push_back(*callee);
            }
        }
    }
    seen
}

/// BFS depths from module roots (functions with no direct callers, plus all
/// exports). Functions inside cycles get no depth and are not inlinable.
fn root_depths(
    plan: &BackendPlan,
    edges: &BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>>,
) -> BTreeMap<PlanFuncId, u32> {
    let mut has_caller: BTreeSet<PlanFuncId> = BTreeSet::new();
    for callees in edges.values() {
        has_caller.extend(callees.iter().copied());
    }
    let mut roots: BTreeSet<PlanFuncId> = BTreeSet::new();
    for index in 0..plan.functions.len() {
        let id = PlanFuncId(index as u32);
        if !has_caller.contains(&id) {
            roots.insert(id);
        }
    }
    let mut depths: BTreeMap<PlanFuncId, u32> = BTreeMap::new();
    let mut queue: VecDeque<(PlanFuncId, u32)> = roots.into_iter().map(|root| (root, 0)).collect();
    while let Some((current, depth)) = queue.pop_front() {
        match depths.get(&current) {
            Some(existing) if *existing <= depth => continue,
            _ => depths.insert(current, depth),
        };
        if let Some(callees) = edges.get(&current) {
            for callee in callees {
                queue.push_back((*callee, depth + 1));
            }
        }
    }
    depths
}

struct SiteInfo {
    function: PlanFuncId,
    block: u32,
    inst: Option<u32>,
    kind: PlanCallKind,
    args: Vec<crate::backend_plan::PlanValueId>,
    arg_representations: Vec<NanoValueRepresentation>,
    /// For terminator tail calls: whether the site is a tail position.
    tail_position: bool,
}

/// Plan nano-native lowering for `module`.
///
/// The default configuration produces the pre-existing nano behavior:
/// every call site is the generic adapter, nothing is inlined, no variant is
/// requested, and no target is gated.
pub fn plan_nano_native(
    module: &Module<'_>,
    assumptions: &AssumptionSet,
    config: &NanoNativeConfig,
) -> Result<NanoNativeOutcome, Vec<NanoNativeDiagnostic>> {
    let plan = BackendPlan::from_module(module).map_err(|plan_diagnostics| {
        vec![NanoNativeDiagnostic {
            kind: NanoNativeDiagnosticKind::PlanError,
            function: None,
            message: "BackendPlan construction failed; nano-native lowering is not attempted"
                .to_owned(),
            plan_diagnostics,
        }]
    })?;

    let mut diagnostics: Vec<NanoNativeDiagnostic> = Vec::new();
    let completions = transitive_completions(&plan, &config.throwing_functions);
    let edges = direct_call_edges(&plan);
    let depths = root_depths(&plan, &edges);
    let reachability: BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>> = edges
        .keys()
        .map(|func| (*func, reachable(&edges, *func)))
        .collect();

    // -----------------------------------------------------------------
    // Per-function facts and parameter elision (before specialization).
    // -----------------------------------------------------------------
    let wrapping = if config.wrapping_arithmetic {
        WrappingArithmetic::Allowed
    } else {
        WrappingArithmetic::FailClosed
    };
    let mut elisions = Vec::new();
    let mut function_facts = Vec::new();
    for index in 0..plan.functions.len() {
        let function = PlanFuncId(index as u32);
        let facts = analyze_function(
            &plan,
            function,
            BTreeMap::new(),
            BoundaryLiveness::default(),
            wrapping,
        );
        let param_count = plan.functions[index]
            .entry
            .map(|entry| plan.functions[index].blocks[entry.index()].params.len())
            .unwrap_or(0);
        let live: BTreeSet<u32> = facts.used_params.iter().copied().collect();
        let elided: Vec<u32> = (0..param_count as u32)
            .filter(|index| !live.contains(index))
            .collect();
        if !elided.is_empty() {
            elisions.push(ParamElision {
                function,
                live: facts.used_params.clone(),
                elided,
            });
        }
        function_facts.push(facts);
    }

    // -----------------------------------------------------------------
    // Collect call sites in canonical order.
    // -----------------------------------------------------------------
    let mut sites: Vec<SiteInfo> = Vec::new();
    for (func_index, func) in plan.functions.iter().enumerate() {
        for (block_index, block) in func.blocks.iter().enumerate() {
            for (inst_index, inst) in block.insts.iter().enumerate() {
                let Some(call) = &inst.call else {
                    continue;
                };
                let arg_representations = inst
                    .args
                    .iter()
                    .map(|arg| {
                        func.values
                            .get(arg.index())
                            .map(|value| value.representation)
                            .unwrap_or(NanoValueRepresentation::Boundary)
                    })
                    .collect();
                sites.push(SiteInfo {
                    function: PlanFuncId(func_index as u32),
                    block: block_index as u32,
                    inst: Some(inst_index as u32),
                    kind: call.kind,
                    args: inst.args.clone(),
                    arg_representations,
                    tail_position: false,
                });
            }
            if let PlanTerminator::TailCall { call, args, .. } = &block.terminator {
                let arg_representations = args
                    .iter()
                    .map(|arg| {
                        func.values
                            .get(arg.index())
                            .map(|value| value.representation)
                            .unwrap_or(NanoValueRepresentation::Boundary)
                    })
                    .collect();
                sites.push(SiteInfo {
                    function: PlanFuncId(func_index as u32),
                    block: block_index as u32,
                    inst: None,
                    kind: *call,
                    args: args.clone(),
                    arg_representations,
                    tail_position: true,
                });
            }
        }
    }

    // -----------------------------------------------------------------
    // Classify each site: identity provenance -> strategy.
    // -----------------------------------------------------------------
    let mut decisions: Vec<NanoCallDecision> = Vec::new();
    let mut inline_records: Vec<InlineRecord> = Vec::new();
    let mut inline_usage = (0_u32, 0_u32, 0_u32); // (cloned blocks, code size, work)
    let mut errors: Vec<NanoNativeDiagnostic> = Vec::new();

    // Facts lookup for ref.func provenance: the callee operand of a
    // call_ref site is its last argument.
    let facts_of = |function: PlanFuncId| -> Option<&crate::specialize::FunctionFacts> {
        function_facts.get(function.index())
    };

    for site in &sites {
        let caller_completions = completions
            .get(&site.function)
            .cloned()
            .unwrap_or_else(|| vec![CompletionKind::Return]);
        // The caller's own declared completion set (before call-graph
        // propagation): what its frame is prepared to expose.
        let caller_declared: Vec<CompletionKind> = {
            let mut kinds: BTreeSet<CompletionKind> = plan.functions[site.function.index()]
                .completions
                .kinds
                .iter()
                .copied()
                .collect();
            if config.throwing_functions.contains(&site.function) {
                kinds.insert(CompletionKind::Throw);
            }
            kinds.into_iter().collect()
        };
        // Arena arguments: opaque ArenaRef positions preserve object identity
        // and writebacks across the call.
        let arena_args: Vec<u32> = site
            .arg_representations
            .iter()
            .enumerate()
            .filter(|(_, repr)| **repr == NanoValueRepresentation::ArenaRef)
            .map(|(index, _)| index as u32)
            .collect();

        // Identity provenance.
        let proven: Option<PlanFuncId> = match site.kind {
            PlanCallKind::Direct { callee } => Some(callee),
            PlanCallKind::Ref => site
                .args
                .last()
                .and_then(|operand| {
                    facts_of(site.function)
                        .and_then(|facts| facts.values.get(&(operand.index() as u32)))
                })
                .and_then(|fact| match fact {
                    ValueFact::FuncIdentity(callee) => Some(*callee),
                    _ => None,
                }),
            PlanCallKind::Indirect { .. } => None,
        };

        let mut fallback = None;
        let mut adaptation = None;
        let strategy = if !config.native_functions {
            NanoCallStrategy::GenericAdapter
        } else {
            match (site.kind, proven) {
                (PlanCallKind::Indirect { .. }, _) => NanoCallStrategy::GenericAdapter,
                (_, None) => {
                    // Rebound/uncertain references keep the tag-guarded
                    // identity check plus the generic adapter fallback.
                    diagnostics.push(NanoNativeDiagnostic {
                        kind: NanoNativeDiagnosticKind::GuardedFallback,
                        function: Some(site.function),
                        message: format!(
                            "call site at block {} keeps the guarded generic adapter path (identity not proven)",
                            site.block
                        ),
                        plan_diagnostics: Vec::new(),
                    });
                    fallback = Some(NanoCallStrategy::GenericAdapter);
                    NanoCallStrategy::GuardedAdapter
                }
                (_, Some(callee)) => {
                    let callee_completions = completions
                        .get(&callee)
                        .cloned()
                        .unwrap_or_else(|| vec![CompletionKind::Return]);
                    if site.tail_position {
                        // Frame replacement only when the return ABI matches
                        // and the callee's completion kinds are a subset of
                        // what the caller's own frame declares (propagating
                        // the callee's kinds through the tail edge itself
                        // does not count); otherwise the call is framed and
                        // adapted.
                        let abi_matches = tail_abi_matches(&plan, site.function, callee);
                        let completions_match = callee_completions
                            .iter()
                            .all(|kind| caller_declared.contains(kind));
                        if abi_matches && completions_match {
                            NanoCallStrategy::TailDirect
                        } else {
                            adaptation = Some(if !abi_matches {
                                "return ABI differs; framed call with return-shape adaptation"
                                    .to_owned()
                            } else {
                                "completion kinds differ; framed call with completion adaptation"
                                    .to_owned()
                            });
                            NanoCallStrategy::Framed
                        }
                    } else {
                        // Non-tail site: consider full inlining.
                        let inline = consider_inline(
                            site,
                            callee,
                            &plan,
                            config,
                            &depths,
                            &reachability,
                            &mut inline_usage,
                            &mut errors,
                        );
                        match inline {
                            InlineVerdict::Inline(record) => {
                                inline_records.push(record);
                                NanoCallStrategy::Inline
                            }
                            InlineVerdict::FallbackFramed { reason } => {
                                diagnostics.push(NanoNativeDiagnostic {
                                    kind: NanoNativeDiagnosticKind::InlineFallback,
                                    function: Some(site.function),
                                    message: reason.clone(),
                                    plan_diagnostics: Vec::new(),
                                });
                                fallback = Some(NanoCallStrategy::Direct);
                                adaptation = Some(reason);
                                NanoCallStrategy::Framed
                            }
                            InlineVerdict::NotRequested => NanoCallStrategy::Direct,
                        }
                    }
                }
            }
        };

        decisions.push(NanoCallDecision {
            function: site.function,
            block: site.block,
            inst: site.inst,
            strategy,
            callee: proven,
            completions: caller_completions,
            arena_args,
            fallback,
            adaptation,
        });
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    // -----------------------------------------------------------------
    // Specialization variants (nano identity/native extensions).
    // -----------------------------------------------------------------
    let mode = if config.native_functions {
        SpecializationMode::NanoVariants
    } else {
        SpecializationMode::PreserveFactsOnly
    };
    let mut specializer = Specializer::new(
        &plan,
        assumptions,
        config.variant_budget.unwrap_or_default(),
        mode,
    );
    for facts in &function_facts {
        let function = facts.function;
        if plan.functions.get(function.index()).is_none()
            || plan.functions[function.index()].blocks.is_empty()
        {
            continue;
        }
        let live: BTreeSet<u32> = facts.used_params.iter().copied().collect();
        let param_facts: Vec<(u32, ValueFact)> = facts
            .params
            .iter()
            .enumerate()
            .map(|(index, fact)| (index as u32, fact.clone()))
            .collect();
        // Proven identity enters the key only when the function is the
        // target of at least one proven call site (module-known or
        // provenance-tracked).
        let identity = if decisions
            .iter()
            .any(|decision| decision.callee == Some(function))
        {
            Some(function)
        } else {
            None
        };
        let key = SpecializationKey::form(
            function,
            param_facts,
            &live,
            facts.boundary,
            facts.results.clone(),
            facts.completions.clone(),
            identity,
            assumptions,
            if config.native_functions {
                Some("nano-native".to_owned())
            } else {
                None
            },
        );
        match specializer.request_variant(&key) {
            Ok(VariantRequest::Created(_)) | Ok(VariantRequest::Existing(_)) => {}
            Ok(VariantRequest::Coarsened { .. }) => {}
            Err(diagnostic) => diagnostics.push(NanoNativeDiagnostic {
                kind: NanoNativeDiagnosticKind::InlineFallback,
                function: Some(function),
                message: format!("variant request rejected: {diagnostic}"),
                plan_diagnostics: Vec::new(),
            }),
        }
    }
    let (_variants, spec_diagnostics, budget_usage, coarsenings) = specializer.finish();
    let mut manifest_entries = Vec::new();
    for facts in &function_facts {
        let function_coarsenings: Vec<_> = coarsenings
            .iter()
            .filter(|record| record.function == facts.function)
            .cloned()
            .collect();
        manifest_entries.push(crate::specialize::ManifestEntry {
            function: facts.function,
            key_fingerprint: None,
            generic: true,
            coarsenings: function_coarsenings,
            fallbacks: Vec::new(),
        });
    }
    let specialization_manifest = SpecializationManifest {
        schema: crate::specialize::SPECIALIZATION_SCHEMA.to_owned(),
        plan_fingerprint: plan.fingerprint(),
        assumption_digest: assumptions.digest(),
        entries: manifest_entries,
        budget_usage,
    };
    for diagnostic in spec_diagnostics {
        diagnostics.push(NanoNativeDiagnostic {
            kind: NanoNativeDiagnosticKind::InlineFallback,
            function: diagnostic.function,
            message: format!("specialization: {}", diagnostic.message),
            plan_diagnostics: Vec::new(),
        });
    }

    // -----------------------------------------------------------------
    // Capability gating (fail-closed).
    // -----------------------------------------------------------------
    let mut required = RequiredCapabilities {
        direct_calls: plan
            .functions
            .iter()
            .flat_map(|func| &func.blocks)
            .flat_map(|block| &block.insts)
            .any(|inst| matches!(inst.call.as_ref().map(|call| call.kind), Some(PlanCallKind::Direct { .. }))),
        tail_calls: plan.runtime.uses_tail_calls,
        indirect_calls: plan.runtime.uses_indirect_calls,
        ref_calls: plan.runtime.uses_ref_calls,
        multi_result: plan.runtime.uses_multi_result,
        adapter_calls: true, // the fixed adapter ABI is always present
        arena: plan.runtime.needs_arena,
    };
    required.direct_calls |= decisions.iter().any(|decision| {
        matches!(
            decision.strategy,
            NanoCallStrategy::Direct
                | NanoCallStrategy::TailDirect
                | NanoCallStrategy::Inline
                | NanoCallStrategy::Framed
        )
    });
    required.tail_calls |= decisions
        .iter()
        .any(|decision| decision.strategy == NanoCallStrategy::TailDirect);

    let mut capability_checks = Vec::new();
    for target in &config.targets {
        let unsupported = target.missing(&required);
        capability_checks.push(CapabilityCheck {
            target: target.name.clone(),
            supported: unsupported.is_empty(),
            unsupported,
        });
    }
    let gating_failures: Vec<NanoNativeDiagnostic> = capability_checks
        .iter()
        .filter(|check| !check.supported)
        .map(|check| NanoNativeDiagnostic {
            kind: NanoNativeDiagnosticKind::UnsupportedTargetCombination,
            function: None,
            message: format!(
                "target {} cannot lower this plan: unsupported operations: {}",
                check.target,
                check.unsupported.join(", ")
            ),
            plan_diagnostics: Vec::new(),
        })
        .collect();
    if !gating_failures.is_empty() {
        return Err(gating_failures);
    }

    Ok(NanoNativeOutcome {
        backend: plan,
        native: NanoNativePlan {
            schema: NANO_NATIVE_SCHEMA.to_owned(),
            native_functions: config.native_functions,
            inlining: config.inlining,
            adapter_abi: AdapterAbiRecord::default(),
            param_elisions: elisions,
            decisions,
            inline_records,
            capability_checks,
            diagnostics,
            specialization: specialization_manifest,
        },
    })
}

/// Whether a tail call's return ABI matches the caller exactly (frame
/// replacement requirement).
fn tail_abi_matches(plan: &BackendPlan, caller: PlanFuncId, callee: PlanFuncId) -> bool {
    let Some(caller_func) = plan.functions.get(caller.index()) else {
        return false;
    };
    let Some(callee_func) = plan.functions.get(callee.index()) else {
        return false;
    };
    caller_func.completions.results == callee_func.completions.results
}

enum InlineVerdict {
    Inline(InlineRecord),
    FallbackFramed { reason: String },
    NotRequested,
}

/// Decide whether one direct call site is inlined, under budget and
/// recursion-proof constraints. Required failures push fail-closed
/// diagnostics into `errors`; opportunistic failures return a recorded
/// framed fallback.
#[allow(clippy::too_many_arguments)]
fn consider_inline(
    site: &SiteInfo,
    callee: PlanFuncId,
    plan: &BackendPlan,
    config: &NanoNativeConfig,
    depths: &BTreeMap<PlanFuncId, u32>,
    reachability: &BTreeMap<PlanFuncId, BTreeSet<PlanFuncId>>,
    usage: &mut (u32, u32, u32),
    errors: &mut Vec<NanoNativeDiagnostic>,
) -> InlineVerdict {
    let required = config.required_inline.contains(&callee);
    let active = match config.inlining {
        InliningMode::Off => false,
        InliningMode::Opportunistic => true,
        InliningMode::Required => required,
    };
    if !active {
        return InlineVerdict::NotRequested;
    }
    usage.2 = usage.2.saturating_add(1);

    let fail = |errors: &mut Vec<NanoNativeDiagnostic>, message: String| {
        if required && config.inlining == InliningMode::Required {
            errors.push(NanoNativeDiagnostic {
                kind: if message.contains("recursion") {
                    NanoNativeDiagnosticKind::RecursionProofFailure
                } else {
                    NanoNativeDiagnosticKind::RequiredInlineBudgetExceeded
                },
                function: Some(site.function),
                message,
                plan_diagnostics: Vec::new(),
            });
            InlineVerdict::NotRequested
        } else {
            InlineVerdict::FallbackFramed { reason: message }
        }
    };

    // Recursion proof: inlining is impossible when the callee reaches the
    // caller (a cycle) or itself (self-recursion). Runtime-depth recursion
    // is never fully inlined; tail recursion stays a canonical tail call.
    let callee_reaches = reachability.get(&callee).cloned().unwrap_or_default();
    if callee_reaches.contains(&site.function) || callee_reaches.contains(&callee) {
        return fail(
            errors,
            format!(
                "recursion proof failed for callee {} at block {}: cycles are never fully inlined",
                callee, site.block
            ),
        );
    }
    // Depth budget (BFS depth from module roots; cyclic functions have no
    // depth and already failed the recursion proof).
    let Some(depth) = depths.get(&site.function).copied() else {
        return fail(
            errors,
            format!(
                "recursion proof failed for callee {}: caller has no acyclic call depth",
                callee
            ),
        );
    };
    let inline_depth = depth + 1;
    if inline_depth > config.inline_budget.max_call_depth {
        return fail(
            errors,
            format!(
                "inline depth {inline_depth} exceeds the call-depth budget {}",
                config.inline_budget.max_call_depth
            ),
        );
    }
    let Some(callee_func) = plan.functions.get(callee.index()) else {
        return fail(errors, format!("callee {callee} has no inlinable body"));
    };
    if callee_func.blocks.is_empty() {
        // Imports have no body: framed direct call, never an error.
        return InlineVerdict::FallbackFramed {
            reason: format!("callee {callee} is an import without a body"),
        };
    }
    let cloned_blocks = callee_func.blocks.len() as u32;
    let code_size: u32 = callee_func
        .blocks
        .iter()
        .map(|block| block.insts.len() as u32 + 1)
        .sum();
    if usage.0.saturating_add(cloned_blocks) > config.inline_budget.max_cloned_blocks {
        return fail(
            errors,
            format!(
                "cloned-block budget {} would be exceeded inlining callee {} ({} blocks)",
                config.inline_budget.max_cloned_blocks, callee, cloned_blocks
            ),
        );
    }
    if usage.1.saturating_add(code_size) > config.inline_budget.max_code_size {
        return fail(
            errors,
            format!(
                "code-size budget {} would be exceeded inlining callee {}",
                config.inline_budget.max_code_size, callee
            ),
        );
    }
    if usage.2 > config.inline_budget.max_work_units {
        return fail(
            errors,
            format!(
                "work-unit budget {} exceeded scheduling inline of callee {}",
                config.inline_budget.max_work_units, callee
            ),
        );
    }
    usage.0 = usage.0.saturating_add(cloned_blocks);
    usage.1 = usage.1.saturating_add(code_size);
    InlineVerdict::Inline(InlineRecord {
        function: site.function,
        block: site.block,
        inst: site.inst,
        callee,
        depth: inline_depth,
        cloned_blocks,
        required,
    })
}

#[cfg(test)]
mod tests;
