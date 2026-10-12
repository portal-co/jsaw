//! jsaw-owned specialization facts, keys, and bounded variants (DreamComp #18, #26).
//!
//! This module ports DreamComp's specialization and monomorphization semantics
//! onto jsaw-owned representations: it consumes the stable [`BackendPlan`]
//! (the jsaw-owned SSA CFG) rather than DreamComp's CPS identities. Facts and
//! keys are canonical: they use plan-level canonical IDs, include the active
//! assumption digest, and never leak Waffle entity allocation order into
//! stable identity. Waffle entity IDs may be attached to facts only as
//! session-local metadata ([`WaffleIdentity`]); they never enter keys or
//! fingerprints.
//!
//! Pipeline (per the #18 resolution):
//!
//! ```text
//! unused-parameter elimination
//!   -> SSA facts and bounded constant propagation
//!   -> direct-call and transitive completion analysis
//!   -> specialization keys (with assumption digest)
//!   -> bounded variant accounting with deterministic coarsening
//!   -> manifest
//! ```
//!
//! CoreGC consumes facts only ([`CoreGcFacts`]) as optional runtime-fast-path
//! metadata; it never forces variants. Nano may request bounded variants via
//! [`Specializer::request_variant`]. Ambiguous or dynamic facts always resolve
//! to the generic fallback — never to a guessed variant.

use portal_jsc_common::assumptions::AssumptionSet;
use serde::{Deserialize, Serialize};
use sha3::{Digest, Sha3_256};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use crate::backend_plan::{
    BackendPlan, CompletionKind, PlanCallKind, PlanConst, PlanFuncId, PlanSigId, PlanTerminator,
    PlanValueId,
};

/// Schema identifier for specialization artifacts.
pub const SPECIALIZATION_SCHEMA: &str = "jsaw.specialization.v1";
/// Domain separator for specialization key fingerprints.
const KEY_DOMAIN: &[u8] = b"portal-jsc-specialization\0";
/// Domain separator for manifest fingerprints.
const MANIFEST_DOMAIN: &[u8] = b"portal-jsc-specialization-manifest\0";
/// Global bound on evaluator steps (the documented 64-step walk).
pub const EVAL_STEP_BOUND: u32 = 64;
/// Bound on nested call-site environments.
pub const MAX_ENV_DEPTH: u32 = 4;

// ---------------------------------------------------------------------------
// Facts
// ---------------------------------------------------------------------------

/// Nullability of a reference value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Nullability {
    Nullable,
    NonNull,
}

/// Alias class of an object value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AliasClass {
    /// No other live reference aliases this value.
    Unaliased,
    /// Other references may alias this value.
    Aliased,
    /// The value escapes the current function's control.
    Escaped,
}

/// Mutation class of an object value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MutationClass {
    Immutable,
    Mutable,
    Unknown,
}

/// One scalarized/inline field of an object, with its own fact. The optional
/// Waffle value identity is session-local and never enters keys.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct FlattenedFieldFact {
    pub field: u32,
    pub fact: ValueFact,
    pub waffle_value: Option<u32>,
}

/// Canonical object facts: structural shape, alias/mutation class, arena
/// opacity, and flattened fields.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ObjectFact {
    /// Canonical structural shape (plan-interned signature).
    pub shape: PlanSigId,
    pub alias: AliasClass,
    pub mutation: MutationClass,
    /// True when the object crosses calls as an opaque logical arena
    /// reference; false when scalarized/inline is proven safe.
    pub arena_opaque: bool,
    pub flattened_fields: Vec<FlattenedFieldFact>,
}

/// A canonical fact about one value. `Unknown` is the fail-closed bottom:
/// ambiguous, dynamic, unsupported, or over-budget facts are `Unknown`, never
/// guessed.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ValueFact {
    Unknown,
    /// A bit-exact constant.
    Constant(PlanConst),
    /// Inclusive integer range (signed interpretation, i64 domain).
    Range { lo: i64, hi: i64 },
    /// Nullability of a reference value.
    Null(Nullability),
    /// Object shape/alias/mutation/arena facts.
    Object(ObjectFact),
    /// A proven function identity (canonical plan function).
    FuncIdentity(PlanFuncId),
}

impl ValueFact {
    /// Join two facts. Anything that disagrees or is only partially known
    /// coarsens to `Unknown`.
    pub fn join(left: &ValueFact, right: &ValueFact) -> ValueFact {
        if left == right {
            left.clone()
        } else {
            ValueFact::Unknown
        }
    }
}

// ---------------------------------------------------------------------------
// Keys
// ---------------------------------------------------------------------------

/// Liveness of the jsaw adapter boundary slots (`context`, `this`,
/// `arguments`). Unused boundary values are elided from keys just like
/// ordinary unused parameters.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct BoundaryLiveness {
    pub context: bool,
    pub this: bool,
    pub arguments: bool,
}

/// A canonical specialization key. Two keys are equal iff they request the
/// same variant; serialization and fingerprint are deterministic.
///
/// Keys are formed only after unused-parameter elimination: the `params` list
/// contains facts for live parameters in canonical index order. The active
/// assumption digest always participates.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpecializationKey {
    pub schema: String,
    pub function: PlanFuncId,
    /// Facts for live parameters, in canonical parameter index order.
    pub params: Vec<ValueFact>,
    pub boundary: BoundaryLiveness,
    pub results: Vec<ValueFact>,
    pub completions: Vec<CompletionKind>,
    /// Proven callee identity when this key is formed at a direct-call or
    /// proven-`ref.func` site.
    pub callee_identity: Option<PlanFuncId>,
    /// Lowercase hex SHA3-256 digest of the active `AssumptionSet`.
    pub assumption_digest: String,
    /// Strategy-specific extension (e.g. nano identity/native extensions).
    pub strategy_extension: Option<String>,
}

impl SpecializationKey {
    /// Form a canonical key. Parameter facts may arrive in any order and may
    /// contain duplicate indices (the smallest fact by canonical order wins,
    /// so the result is order-independent). Facts for parameters not listed
    /// in `live_params` are dropped: unused-parameter elimination happens
    /// here, before key formation.
    pub fn form(
        function: PlanFuncId,
        param_facts: impl IntoIterator<Item = (u32, ValueFact)>,
        live_params: &BTreeSet<u32>,
        boundary: BoundaryLiveness,
        results: Vec<ValueFact>,
        completions: Vec<CompletionKind>,
        callee_identity: Option<PlanFuncId>,
        assumptions: &AssumptionSet,
        strategy_extension: Option<String>,
    ) -> Self {
        let mut merged: BTreeMap<u32, ValueFact> = BTreeMap::new();
        for (index, fact) in param_facts {
            merged
                .entry(index)
                .and_modify(|existing| {
                    if fact < *existing {
                        *existing = fact.clone();
                    }
                })
                .or_insert(fact);
        }
        let params = merged
            .into_iter()
            .filter(|(index, _)| live_params.contains(index))
            .map(|(_, fact)| fact)
            .collect();
        let mut completions = completions;
        completions.sort();
        completions.dedup();
        Self {
            schema: SPECIALIZATION_SCHEMA.to_owned(),
            function,
            params,
            boundary,
            results,
            completions,
            callee_identity,
            assumption_digest: assumptions.digest(),
            strategy_extension,
        }
    }

    /// Deterministic JSON serialization (the key contains no maps).
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("SpecializationKey serialization is infallible")
    }

    /// Domain-separated, length-delimited canonical bytes.
    pub fn canonical_bytes(&self) -> Vec<u8> {
        let json = self.to_json();
        let mut bytes = Vec::with_capacity(KEY_DOMAIN.len() + 8 + json.len());
        bytes.extend_from_slice(KEY_DOMAIN);
        bytes.extend_from_slice(&(json.len() as u64).to_be_bytes());
        bytes.extend_from_slice(json.as_bytes());
        bytes
    }

    /// Lowercase hexadecimal SHA3-256 fingerprint.
    pub fn fingerprint(&self) -> String {
        let digest = Sha3_256::digest(self.canonical_bytes());
        digest.iter().map(|byte| format!("{byte:02x}")).collect()
    }
}

// ---------------------------------------------------------------------------
// Bounded constant evaluator over the BackendPlan CFG
// ---------------------------------------------------------------------------

/// Nested call-site parameter environments. Frame 0 is the outermost
/// function's parameter facts; the last frame is the current function's.
/// Nesting is bounded by [`MAX_ENV_DEPTH`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CallSiteParamEnv {
    frames: Vec<BTreeMap<u32, ValueFact>>,
}

impl CallSiteParamEnv {
    pub fn root(params: BTreeMap<u32, ValueFact>) -> Self {
        Self {
            frames: vec![params],
        }
    }

    /// Push a callee frame. Returns `None` when the depth bound would be
    /// exceeded (fail-closed: the call evaluates to `Unknown` instead).
    pub fn push(&self, callee_params: BTreeMap<u32, ValueFact>) -> Option<Self> {
        if self.frames.len() >= MAX_ENV_DEPTH as usize {
            return None;
        }
        let mut frames = self.frames.clone();
        frames.push(callee_params);
        Some(Self { frames })
    }

    pub fn depth(&self) -> u32 {
        self.frames.len() as u32
    }

    fn current(&self) -> Option<&BTreeMap<u32, ValueFact>> {
        self.frames.last()
    }
}

/// Whether integer Add/Sub/Mul may wrap. Wrapping is permitted only under an
/// explicit compiler-control flag; without it, overflow yields `Unknown`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WrappingArithmetic {
    /// Overflow or unsupported shapes yield `Unknown`.
    #[default]
    FailClosed,
    /// Two's-complement wrapping arithmetic is permitted.
    Allowed,
}

/// Evaluator accounting.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvalReport {
    pub steps_used: u32,
    pub truncated: bool,
    pub cycles_cut: u32,
    pub unsupported: u32,
}

struct EvalState {
    steps: u32,
    truncated: bool,
    cycles_cut: u32,
    unsupported: u32,
    cache: BTreeMap<(PlanFuncId, PlanValueId), ValueFact>,
    visiting: BTreeSet<(PlanFuncId, PlanValueId)>,
}

impl EvalState {
    fn new() -> Self {
        Self {
            steps: 0,
            truncated: false,
            cycles_cut: 0,
            unsupported: 0,
            cache: BTreeMap::new(),
            visiting: BTreeSet::new(),
        }
    }

    /// Consume one step. Returns false when the bound is exhausted, which
    /// fails closed to `Unknown`. Once truncated the counter saturates:
    /// every further step is rejected without counting.
    fn step(&mut self) -> bool {
        if self.truncated {
            return false;
        }
        self.steps += 1;
        if self.steps > EVAL_STEP_BOUND {
            self.truncated = true;
            return false;
        }
        true
    }

    fn report(&self) -> EvalReport {
        EvalReport {
            steps_used: self.steps,
            truncated: self.truncated,
            cycles_cut: self.cycles_cut,
            unsupported: self.unsupported,
        }
    }
}

/// Predecessor edges of a block: (predecessor block, argument list) pairs in
/// canonical order.
fn predecessor_edges(plan: &BackendPlan, function: PlanFuncId) -> BTreeMap<u32, Vec<(u32, Vec<PlanValueId>)>> {
    let mut edges: BTreeMap<u32, Vec<(u32, Vec<PlanValueId>)>> = BTreeMap::new();
    let Some(func) = plan.functions.get(function.index()) else {
        return edges;
    };
    for (index, block) in func.blocks.iter().enumerate() {
        let mut push = |target: &crate::backend_plan::PlanTarget| {
            edges
                .entry(target.block.index() as u32)
                .or_default()
                .push((index as u32, target.args.clone()));
        };
        match &block.terminator {
            PlanTerminator::Br { target } => push(target),
            PlanTerminator::CondBr {
                if_true, if_false, ..
            } => {
                push(if_true);
                push(if_false);
            }
            PlanTerminator::Select {
                targets, default, ..
            } => {
                for target in targets {
                    push(target);
                }
                push(default);
            }
            _ => {}
        }
    }
    edges
}

/// Evaluate one value's fact in the current environment.
fn eval_value(
    plan: &BackendPlan,
    function: PlanFuncId,
    value: PlanValueId,
    env: &CallSiteParamEnv,
    preds: &BTreeMap<u32, Vec<(u32, Vec<PlanValueId>)>>,
    wrapping: WrappingArithmetic,
    state: &mut EvalState,
) -> ValueFact {
    if !state.step() {
        return ValueFact::Unknown;
    }
    let key = (function, value);
    if let Some(cached) = state.cache.get(&key) {
        return cached.clone();
    }
    if !state.visiting.insert(key) {
        // A cycle (e.g. a loop-carried block parameter) fails closed.
        state.cycles_cut += 1;
        return ValueFact::Unknown;
    }
    let fact = eval_value_inner(plan, function, value, env, preds, wrapping, state);
    state.visiting.remove(&key);
    state.cache.insert(key, fact.clone());
    fact
}

fn eval_value_inner(
    plan: &BackendPlan,
    function: PlanFuncId,
    value: PlanValueId,
    env: &CallSiteParamEnv,
    preds: &BTreeMap<u32, Vec<(u32, Vec<PlanValueId>)>>,
    wrapping: WrappingArithmetic,
    state: &mut EvalState,
) -> ValueFact {
    let Some(func) = plan.functions.get(function.index()) else {
        state.unsupported += 1;
        return ValueFact::Unknown;
    };
    let Some(plan_value) = func.values.get(value.index()) else {
        state.unsupported += 1;
        return ValueFact::Unknown;
    };
    match &plan_value.def {
        crate::backend_plan::PlanValueDef::BlockParam { block, index } => {
            if block.index() == 0 {
                // Entry-block parameters come from the current environment.
                return env
                    .current()
                    .and_then(|frame| frame.get(index))
                    .cloned()
                    .unwrap_or(ValueFact::Unknown);
            }
            // Non-entry parameters: predecessor agreement. Every predecessor
            // edge must agree on a non-Unknown fact.
            let Some(edges) = preds.get(&(block.index() as u32)) else {
                return ValueFact::Unknown;
            };
            if edges.is_empty() {
                return ValueFact::Unknown;
            }
            let mut agreed: Option<ValueFact> = None;
            for (_, args) in edges {
                let Some(arg) = args.get(*index as usize).copied() else {
                    return ValueFact::Unknown;
                };
                let fact = eval_value(plan, function, arg, env, preds, wrapping, state);
                if fact == ValueFact::Unknown {
                    return ValueFact::Unknown;
                }
                match &agreed {
                    None => agreed = Some(fact),
                    Some(existing) => {
                        let joined = ValueFact::join(existing, &fact);
                        if joined == ValueFact::Unknown {
                            return ValueFact::Unknown;
                        }
                        agreed = Some(joined);
                    }
                }
            }
            agreed.unwrap_or(ValueFact::Unknown)
        }
        crate::backend_plan::PlanValueDef::Operator { block, inst } => {
            let Some(block_def) = func.blocks.get(block.index()) else {
                state.unsupported += 1;
                return ValueFact::Unknown;
            };
            let Some(plan_inst) = block_def.insts.get(*inst as usize) else {
                state.unsupported += 1;
                return ValueFact::Unknown;
            };
            eval_inst(plan, function, plan_inst, env, preds, wrapping, state)
        }
        crate::backend_plan::PlanValueDef::PickOutput { source, .. } => {
            // Transparent node: only alias chains project; tuple constants are
            // not modeled, so non-alias projections are unknown.
            let _ = source;
            state.unsupported += 1;
            ValueFact::Unknown
        }
        crate::backend_plan::PlanValueDef::Alias { target } => {
            if *target == PlanValueId(u32::MAX) {
                return ValueFact::Unknown;
            }
            eval_value(plan, function, *target, env, preds, wrapping, state)
        }
    }
}

fn eval_inst(
    plan: &BackendPlan,
    function: PlanFuncId,
    inst: &crate::backend_plan::PlanInst,
    env: &CallSiteParamEnv,
    preds: &BTreeMap<u32, Vec<(u32, Vec<PlanValueId>)>>,
    wrapping: WrappingArithmetic,
    state: &mut EvalState,
) -> ValueFact {
    // Transparent alias pseudo-instruction.
    if inst.operator == "alias" {
        return match inst.args.first() {
            Some(target) if *target != PlanValueId(u32::MAX) => {
                eval_value(plan, function, *target, env, preds, wrapping, state)
            }
            _ => ValueFact::Unknown,
        };
    }
    // Constants.
    if let Some(constant) = inst.const_value {
        return ValueFact::Constant(constant);
    }
    // Proven function identity.
    if let Some(callee) = inst.ref_func {
        return ValueFact::FuncIdentity(callee);
    }
    // Bounded integer arithmetic.
    if matches!(inst.operator.as_str(), "I32Add" | "I32Sub" | "I32Mul" | "I64Add" | "I64Sub" | "I64Mul") {
        let mut arg_facts = Vec::new();
        for arg in &inst.args {
            arg_facts.push(eval_value(plan, function, *arg, env, preds, wrapping, state));
        }
        return eval_arith(&inst.operator, &arg_facts, wrapping);
    }
    // Nested call-site parameter propagation for direct calls.
    if let Some(call) = &inst.call {
        if let PlanCallKind::Direct { callee } = call.kind {
            if let Some(fact) = eval_nested_call(
                plan,
                callee,
                &inst.args,
                function,
                env,
                preds,
                wrapping,
                state,
            ) {
                return fact;
            }
        }
        state.unsupported += 1;
        return ValueFact::Unknown;
    }
    state.unsupported += 1;
    ValueFact::Unknown
}

fn eval_arith(operator: &str, args: &[ValueFact], wrapping: WrappingArithmetic) -> ValueFact {
    let [left, right] = args else {
        return ValueFact::Unknown;
    };
    let is_32 = operator.starts_with("I32");
    let (ValueFact::Constant(a), ValueFact::Constant(b)) = (left, right) else {
        return ValueFact::Unknown;
    };
    let (a, b) = (*a, *b);
    match (a, b) {
        (PlanConst::I32(a), PlanConst::I32(b)) if is_32 => {
            let (a, b) = (a as i32, b as i32);
            let result = match operator {
                "I32Add" => a.checked_add(b),
                "I32Sub" => a.checked_sub(b),
                "I32Mul" => a.checked_mul(b),
                _ => None,
            };
            match (result, wrapping) {
                (Some(value), _) => ValueFact::Constant(PlanConst::I32(value as u32)),
                (None, WrappingArithmetic::Allowed) => {
                    let wrapped = match operator {
                        "I32Add" => a.wrapping_add(b),
                        "I32Sub" => a.wrapping_sub(b),
                        "I32Mul" => a.wrapping_mul(b),
                        _ => return ValueFact::Unknown,
                    };
                    ValueFact::Constant(PlanConst::I32(wrapped as u32))
                }
                (None, WrappingArithmetic::FailClosed) => ValueFact::Unknown,
            }
        }
        (PlanConst::I64(a), PlanConst::I64(b)) if !is_32 => {
            let (a, b) = (a as i64, b as i64);
            let result = match operator {
                "I64Add" => a.checked_add(b),
                "I64Sub" => a.checked_sub(b),
                "I64Mul" => a.checked_mul(b),
                _ => None,
            };
            match (result, wrapping) {
                (Some(value), _) => ValueFact::Constant(PlanConst::I64(value as u64)),
                (None, WrappingArithmetic::Allowed) => {
                    let wrapped = match operator {
                        "I64Add" => a.wrapping_add(b),
                        "I64Sub" => a.wrapping_sub(b),
                        "I64Mul" => a.wrapping_mul(b),
                        _ => return ValueFact::Unknown,
                    };
                    ValueFact::Constant(PlanConst::I64(wrapped as u64))
                }
                (None, WrappingArithmetic::FailClosed) => ValueFact::Unknown,
            }
        }
        _ => ValueFact::Unknown,
    }
}

/// Evaluate a direct call in a nested environment: the callee's parameters
/// are bound to the caller-side argument facts, and the callee's `Return`
/// values are evaluated under the nested frame. Bounded by the shared step
/// budget and [`MAX_ENV_DEPTH`].
#[allow(clippy::too_many_arguments)]
fn eval_nested_call(
    plan: &BackendPlan,
    callee: PlanFuncId,
    args: &[PlanValueId],
    caller: PlanFuncId,
    env: &CallSiteParamEnv,
    preds: &BTreeMap<u32, Vec<(u32, Vec<PlanValueId>)>>,
    wrapping: WrappingArithmetic,
    state: &mut EvalState,
) -> Option<ValueFact> {
    if !state.step() {
        return Some(ValueFact::Unknown);
    }
    let callee_func = plan.functions.get(callee.index())?;
    if callee_func.blocks.is_empty() {
        return None; // imports and opaque functions stay unknown
    }
    // Bind callee entry parameters to caller-side argument facts.
    let mut frame: BTreeMap<u32, ValueFact> = BTreeMap::new();
    for (index, arg) in args.iter().enumerate() {
        let fact = eval_value(plan, caller, *arg, env, preds, wrapping, state);
        frame.insert(index as u32, fact);
    }
    let nested = env.push(frame)?;
    let callee_preds = predecessor_edges(plan, callee);
    // Find the callee's return values (canonical: first Return terminator).
    let mut returns: Option<Vec<PlanValueId>> = None;
    for block in &callee_func.blocks {
        if let PlanTerminator::Return { values } = &block.terminator {
            returns = Some(values.clone());
            break;
        }
    }
    let returns = returns?;
    if returns.len() != 1 {
        // Multi-result facts are not projected through nested environments.
        return None;
    }
    Some(eval_value(
        plan,
        callee,
        returns[0],
        &nested,
        &callee_preds,
        wrapping,
        state,
    ))
}

// ---------------------------------------------------------------------------
// Per-function facts
// ---------------------------------------------------------------------------

/// Session-local Waffle identities attached to facts. These are never part of
/// keys, fingerprints, or manifests; they exist so a compilation session can
/// correlate facts back to Waffle entities (including flattened fields).
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct WaffleIdentity {
    pub func: u32,
    pub values: BTreeMap<u32, u32>,
}

/// All facts gathered for one function.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionFacts {
    pub function: PlanFuncId,
    /// Facts for every entry parameter, in canonical order.
    pub params: Vec<ValueFact>,
    /// Canonical indices of parameters that are used at all.
    pub used_params: Vec<u32>,
    pub boundary: BoundaryLiveness,
    pub results: Vec<ValueFact>,
    pub completions: Vec<CompletionKind>,
    /// Proven direct/identity callees by (block, inst) position.
    pub proven_callees: BTreeMap<(u32, u32), PlanFuncId>,
    /// Session-local value fact table.
    pub values: BTreeMap<u32, ValueFact>,
    /// Session-local Waffle identity (never enters keys or manifests).
    pub waffle: Option<WaffleIdentity>,
    pub eval: EvalReport,
}

impl Default for FunctionFacts {
    fn default() -> Self {
        Self {
            function: PlanFuncId(u32::MAX),
            params: Vec::new(),
            used_params: Vec::new(),
            boundary: BoundaryLiveness::default(),
            results: Vec::new(),
            completions: Vec::new(),
            proven_callees: BTreeMap::new(),
            values: BTreeMap::new(),
            waffle: None,
            eval: EvalReport::default(),
        }
    }
}

/// Used-parameter analysis: entry parameters referenced (transitively through
/// aliases) by any instruction argument or terminator operand.
fn used_parameters(plan: &BackendPlan, function: PlanFuncId) -> Vec<u32> {
    let Some(func) = plan.functions.get(function.index()) else {
        return Vec::new();
    };
    let Some(entry) = func.entry else {
        return Vec::new();
    };
    let params: Vec<PlanValueId> = func.blocks[entry.index()].params.clone();
    let mut used: BTreeSet<PlanValueId> = BTreeSet::new();
    for block in &func.blocks {
        for inst in &block.insts {
            used.extend(inst.args.iter().copied());
        }
        let mut edge_uses: Vec<PlanValueId> = Vec::new();
        match &block.terminator {
            PlanTerminator::Br { target } => edge_uses.extend(target.args.iter().copied()),
            PlanTerminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                edge_uses.push(*cond);
                edge_uses.extend(if_true.args.iter().copied());
                edge_uses.extend(if_false.args.iter().copied());
            }
            PlanTerminator::Select {
                value,
                targets,
                default,
            } => {
                edge_uses.push(*value);
                for target in targets {
                    edge_uses.extend(target.args.iter().copied());
                }
                edge_uses.extend(default.args.iter().copied());
            }
            PlanTerminator::Return { values } => {
                edge_uses.extend(values.iter().copied());
            }
            PlanTerminator::TailCall { args, .. } => {
                edge_uses.extend(args.iter().copied());
            }
            PlanTerminator::Trap | PlanTerminator::Ub => {}
        }
        used.extend(edge_uses);
    }
    // Transitively close through alias pseudo-instructions: if an alias is
    // used, its target is used.
    let mut changed = true;
    while changed {
        changed = false;
        for block in &func.blocks {
            for inst in &block.insts {
                if inst.operator == "alias" && inst.results.len() == 1 && inst.args.len() == 1 {
                    if used.contains(&inst.results[0]) && !used.contains(&inst.args[0]) {
                        used.insert(inst.args[0]);
                        changed = true;
                    }
                }
            }
        }
    }
    params
        .iter()
        .enumerate()
        .filter(|(_, value)| used.contains(*value))
        .map(|(index, _)| index as u32)
        .collect()
}

/// Gather facts for one function under the given (root) parameter
/// environment.
pub fn analyze_function(
    plan: &BackendPlan,
    function: PlanFuncId,
    root_env: BTreeMap<u32, ValueFact>,
    boundary: BoundaryLiveness,
    wrapping: WrappingArithmetic,
) -> FunctionFacts {
    let mut facts = FunctionFacts {
        function,
        boundary,
        ..FunctionFacts::default()
    };
    let Some(func) = plan.functions.get(function.index()) else {
        return facts;
    };
    facts.completions = func.completions.kinds.clone();
    facts.used_params = used_parameters(plan, function);
    facts.params = func
        .entry
        .map(|entry| {
            func.blocks[entry.index()]
                .params
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    root_env.get(&(index as u32)).cloned().unwrap_or(ValueFact::Unknown)
                })
                .collect()
        })
        .unwrap_or_default();

    let env = CallSiteParamEnv::root(root_env);
    let preds = predecessor_edges(plan, function);
    let mut state = EvalState::new();
    for (index, _) in func.values.iter().enumerate() {
        let fact = eval_value(plan, function, PlanValueId(index as u32), &env, &preds, wrapping, &mut state);
        facts.values.insert(index as u32, fact);
    }
    facts.eval = state.report();

    // Proven callees: direct calls and ref.func identities.
    for (block_index, block) in func.blocks.iter().enumerate() {
        for (inst_index, inst) in block.insts.iter().enumerate() {
            if let Some(call) = &inst.call {
                if let PlanCallKind::Direct { callee } = call.kind {
                    facts
                        .proven_callees
                        .insert((block_index as u32, inst_index as u32), callee);
                }
            }
        }
    }

    // Result facts: evaluate returned values.
    for block in &func.blocks {
        if let PlanTerminator::Return { values } = &block.terminator {
            let mut state = EvalState::new();
            facts.results = values
                .iter()
                .map(|value| eval_value(plan, function, *value, &env, &preds, wrapping, &mut state))
                .collect();
            break;
        }
    }
    facts
}

// ---------------------------------------------------------------------------
// Budgets, variants, and coarsening
// ---------------------------------------------------------------------------

/// Deterministic, bounded variant budgets.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariantBudget {
    pub max_variants_per_function: u32,
    pub max_variants_per_identity: u32,
    pub max_global_variants: u32,
    pub max_work_units: u32,
    /// Logical code-size bound: total instruction count across variants.
    pub max_code_size: u32,
}

impl Default for VariantBudget {
    fn default() -> Self {
        Self {
            max_variants_per_function: 8,
            max_variants_per_identity: 8,
            max_global_variants: 256,
            max_work_units: 4096,
            max_code_size: 65536,
        }
    }
}

/// Observable budget accounting.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BudgetUsage {
    pub variants: u32,
    pub work_units: u32,
    pub code_size: u32,
    pub coarsened_requests: u32,
}

/// One materialized specialization variant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Variant {
    pub id: u32,
    pub function: PlanFuncId,
    pub key_fingerprint: String,
    pub generic: bool,
}

/// The specialization mode: CoreGC preserves facts only; nano may request
/// bounded variants.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SpecializationMode {
    /// Facts are analysis metadata for runtime fast paths; no variants are
    /// materialized and no function is cloned.
    #[default]
    PreserveFactsOnly,
    /// Nano may request bounded specialized variants.
    NanoVariants,
}

/// Structured specialization diagnostics.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SpecializationDiagnosticKind {
    EvaluationTruncated,
    BudgetExceeded,
    Coarsened,
    GenericFallback,
    InvalidRequest,
}

/// One structured specialization diagnostic.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpecializationDiagnostic {
    pub kind: SpecializationDiagnosticKind,
    pub function: Option<PlanFuncId>,
    pub message: String,
}

impl fmt::Display for SpecializationDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.message)
    }
}

/// The result of a variant request.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariantRequest {
    /// A new variant was materialized.
    Created(u32),
    /// An identical variant already exists.
    Existing(u32),
    /// The request exceeded a budget and was coarsened into the generic
    /// variant (or, in facts-only mode, into no variant at all).
    Coarsened { generic: Option<u32> },
}

/// A recorded coarsening/fallback decision for the manifest.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CoarseningRecord {
    pub function: PlanFuncId,
    pub requested_fingerprint: String,
    pub merged_fingerprint: Option<String>,
    pub reason: String,
}

/// Bounded variant allocator with deterministic coarsening.
pub struct Specializer<'p> {
    plan: &'p BackendPlan,
    assumption_digest: String,
    budget: VariantBudget,
    mode: SpecializationMode,
    variants: Vec<Variant>,
    by_fingerprint: BTreeMap<String, u32>,
    per_function: BTreeMap<PlanFuncId, Vec<u32>>,
    per_identity: BTreeMap<PlanFuncId, Vec<u32>>,
    identities: BTreeMap<u32, Option<PlanFuncId>>,
    coarsenings: Vec<CoarseningRecord>,
    diagnostics: Vec<SpecializationDiagnostic>,
    usage: BudgetUsage,
}

impl<'p> Specializer<'p> {
    pub fn new(
        plan: &'p BackendPlan,
        assumptions: &AssumptionSet,
        budget: VariantBudget,
        mode: SpecializationMode,
    ) -> Self {
        Self {
            plan,
            assumption_digest: assumptions.digest(),
            budget,
            mode,
            variants: Vec::new(),
            by_fingerprint: BTreeMap::new(),
            per_function: BTreeMap::new(),
            per_identity: BTreeMap::new(),
            identities: BTreeMap::new(),
            coarsenings: Vec::new(),
            diagnostics: Vec::new(),
            usage: BudgetUsage::default(),
        }
    }

    pub fn assumption_digest(&self) -> &str {
        &self.assumption_digest
    }

    fn generic_key(&self, function: PlanFuncId) -> SpecializationKey {
        let results = self
            .plan
            .functions
            .get(function.index())
            .map(|func| {
                func.completions
                    .results
                    .iter()
                    .map(|_| ValueFact::Unknown)
                    .collect()
            })
            .unwrap_or_default();
        let completions = self
            .plan
            .functions
            .get(function.index())
            .map(|func| func.completions.kinds.clone())
            .unwrap_or_default();
        // The generic key has no parameter facts at all: it is distinct from
        // any specialized key, including one for a zero-parameter function.
        SpecializationKey {
            schema: SPECIALIZATION_SCHEMA.to_owned(),
            function,
            params: vec![ValueFact::Unknown],
            boundary: BoundaryLiveness::default(),
            results,
            completions,
            callee_identity: None,
            assumption_digest: self.assumption_digest.clone(),
            strategy_extension: Some("generic".to_owned()),
        }
    }

    fn materialize(&mut self, function: PlanFuncId, key: &SpecializationKey, generic: bool) -> u32 {
        let fingerprint = key.fingerprint();
        if let Some(existing) = self.by_fingerprint.get(&fingerprint) {
            return *existing;
        }
        let id = self.variants.len() as u32;
        self.variants.push(Variant {
            id,
            function,
            key_fingerprint: fingerprint.clone(),
            generic,
        });
        self.by_fingerprint.insert(fingerprint, id);
        self.per_function.entry(function).or_default().push(id);
        self.identities.insert(id, key.callee_identity);
        if let Some(identity) = key.callee_identity {
            self.per_identity.entry(identity).or_default().push(id);
        }
        let code_size: u32 = self
            .plan
            .functions
            .get(function.index())
            .map(|func| {
                func.blocks
                    .iter()
                    .map(|block| block.insts.len() as u32 + 1)
                    .sum()
            })
            .unwrap_or(0);
        self.usage.variants += 1;
        self.usage.code_size = self.usage.code_size.saturating_add(code_size);
        id
    }

    /// The generic variant for a function (the universal fallback).
    pub fn generic_variant(&mut self, function: PlanFuncId) -> Option<u32> {
        if self.mode == SpecializationMode::PreserveFactsOnly {
            return None;
        }
        if self.plan.functions.get(function.index()).is_none() {
            self.diagnostics.push(SpecializationDiagnostic {
                kind: SpecializationDiagnosticKind::InvalidRequest,
                function: Some(function),
                message: format!("function {function} does not exist in the plan"),
            });
            return None;
        }
        let key = self.generic_key(function);
        Some(self.materialize(function, &key, true))
    }

    /// Request a specialized variant. Budgets are deterministic: when any
    /// budget would be exceeded, the request coarsens to the generic variant
    /// and the decision is recorded in diagnostics and the manifest.
    pub fn request_variant(
        &mut self,
        key: &SpecializationKey,
    ) -> Result<VariantRequest, SpecializationDiagnostic> {
        if key.assumption_digest != self.assumption_digest {
            return Err(SpecializationDiagnostic {
                kind: SpecializationDiagnosticKind::InvalidRequest,
                function: Some(key.function),
                message: "variant key assumption digest does not match the active assumptions"
                    .to_owned(),
            });
        }
        if self.plan.functions.get(key.function.index()).is_none() {
            return Err(SpecializationDiagnostic {
                kind: SpecializationDiagnosticKind::InvalidRequest,
                function: Some(key.function),
                message: format!("function {} does not exist in the plan", key.function),
            });
        }
        self.usage.work_units += 1;
        let fingerprint = key.fingerprint();
        if let Some(existing) = self.by_fingerprint.get(&fingerprint) {
            return Ok(VariantRequest::Existing(*existing));
        }
        if self.mode == SpecializationMode::PreserveFactsOnly {
            // CoreGC never materializes variants.
            self.usage.coarsened_requests += 1;
            self.coarsenings.push(CoarseningRecord {
                function: key.function,
                requested_fingerprint: fingerprint,
                merged_fingerprint: None,
                reason: "facts-only mode: variant requests are never materialized".to_owned(),
            });
            self.diagnostics.push(SpecializationDiagnostic {
                kind: SpecializationDiagnosticKind::GenericFallback,
                function: Some(key.function),
                message: "facts-only mode retains the generic path".to_owned(),
            });
            return Ok(VariantRequest::Coarsened { generic: None });
        }

        let generic = self.generic_variant(key.function);
        let exceeded = if self.usage.work_units > self.budget.max_work_units {
            Some("work-unit budget")
        } else if self.usage.variants >= self.budget.max_global_variants {
            Some("global variant budget")
        } else if self
            .per_function
            .get(&key.function)
            .map(|variants| variants.len() as u32)
            .unwrap_or(0)
            >= self.budget.max_variants_per_function
        {
            Some("per-function variant budget")
        } else if key
            .callee_identity
            .and_then(|identity| self.per_identity.get(&identity))
            .map(|variants| variants.len() as u32)
            .unwrap_or(0)
            >= self.budget.max_variants_per_identity
        {
            Some("per-identity variant budget")
        } else if self.usage.code_size >= self.budget.max_code_size {
            Some("code-size budget")
        } else {
            None
        };
        if let Some(reason) = exceeded {
            self.usage.coarsened_requests += 1;
            let merged = generic.map(|id| self.variants[id as usize].key_fingerprint.clone());
            self.coarsenings.push(CoarseningRecord {
                function: key.function,
                requested_fingerprint: fingerprint,
                merged_fingerprint: merged,
                reason: format!("{reason} exceeded; coarsened to the generic variant"),
            });
            self.diagnostics.push(SpecializationDiagnostic {
                kind: SpecializationDiagnosticKind::BudgetExceeded,
                function: Some(key.function),
                message: format!("{reason} exceeded"),
            });
            return Ok(VariantRequest::Coarsened { generic });
        }
        Ok(VariantRequest::Created(self.materialize(key.function, key, false)))
    }

    /// Finish, producing variants, diagnostics, usage, and coarsening records.
    pub fn finish(
        self,
    ) -> (
        Vec<Variant>,
        Vec<SpecializationDiagnostic>,
        BudgetUsage,
        Vec<CoarseningRecord>,
    ) {
        (
            self.variants,
            self.diagnostics,
            self.usage,
            self.coarsenings,
        )
    }
}

// ---------------------------------------------------------------------------
// Manifest
// ---------------------------------------------------------------------------

/// One manifest entry: a function's specialization identity and decisions.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestEntry {
    pub function: PlanFuncId,
    pub key_fingerprint: Option<String>,
    pub generic: bool,
    pub coarsenings: Vec<CoarseningRecord>,
    /// Recorded fallbacks (e.g. "generic adapter retained").
    pub fallbacks: Vec<String>,
}

/// The deterministic specialization manifest handed to certification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpecializationManifest {
    pub schema: String,
    pub plan_fingerprint: String,
    pub assumption_digest: String,
    pub entries: Vec<ManifestEntry>,
    pub budget_usage: BudgetUsage,
}

impl SpecializationManifest {
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("SpecializationManifest serialization is infallible")
    }

    pub fn canonical_bytes(&self) -> Vec<u8> {
        let json = self.to_json();
        let mut bytes = Vec::with_capacity(MANIFEST_DOMAIN.len() + 8 + json.len());
        bytes.extend_from_slice(MANIFEST_DOMAIN);
        bytes.extend_from_slice(&(json.len() as u64).to_be_bytes());
        bytes.extend_from_slice(json.as_bytes());
        bytes
    }

    pub fn fingerprint(&self) -> String {
        let digest = Sha3_256::digest(self.canonical_bytes());
        digest.iter().map(|byte| format!("{byte:02x}")).collect()
    }
}

// ---------------------------------------------------------------------------
// CoreGC facts-only adapter
// ---------------------------------------------------------------------------

/// A runtime fast-path hint derived from facts (never a forced variant).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FastPathHint {
    Constant(PlanConst),
    NonNullObject(PlanSigId),
    KnownCallee(PlanFuncId),
}

/// One fast-path record for CoreGC's runtime.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CoreGcFastPath {
    pub function: PlanFuncId,
    pub value: u32,
    pub hint: FastPathHint,
}

/// CoreGC's view of specialization: facts only, as optional runtime
/// fast-path metadata. No variants, no forced specialization.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct CoreGcFacts {
    pub fast_paths: Vec<CoreGcFastPath>,
}

impl CoreGcFacts {
    /// Project fast-path hints from gathered facts. Only single-representation
    /// facts (constants, non-null shaped objects, proven callees) become
    /// hints; everything else stays generic.
    pub fn from_facts(facts: &[FunctionFacts]) -> Self {
        let mut fast_paths = Vec::new();
        for function in facts {
            for (value, fact) in &function.values {
                let hint = match fact {
                    ValueFact::Constant(constant) => Some(FastPathHint::Constant(*constant)),
                    ValueFact::Object(object)
                        if object.alias == AliasClass::Unaliased
                            || object.mutation != MutationClass::Unknown =>
                    {
                        Some(FastPathHint::NonNullObject(object.shape))
                    }
                    ValueFact::FuncIdentity(callee) => Some(FastPathHint::KnownCallee(*callee)),
                    _ => None,
                };
                if let Some(hint) = hint {
                    fast_paths.push(CoreGcFastPath {
                        function: function.function,
                        value: *value,
                        hint,
                    });
                }
            }
        }
        Self { fast_paths }
    }
}

/// The complete outcome of a specialization analysis pass.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpecializationOutcome {
    pub schema: String,
    pub facts: Vec<FunctionFacts>,
    pub manifest: SpecializationManifest,
}

/// Analyze every function in the plan, gathering canonical facts and building
/// the deterministic manifest. This pass never materializes variants; nano
/// requests those separately through [`Specializer`].
pub fn analyze(plan: &BackendPlan, assumptions: &AssumptionSet) -> SpecializationOutcome {
    let mut facts = Vec::new();
    for (index, _) in plan.functions.iter().enumerate() {
        let function = PlanFuncId(index as u32);
        facts.push(analyze_function(
            plan,
            function,
            BTreeMap::new(),
            BoundaryLiveness::default(),
            WrappingArithmetic::FailClosed,
        ));
    }
    let mut entries: Vec<ManifestEntry> = facts
        .iter()
        .map(|function| ManifestEntry {
            function: function.function,
            key_fingerprint: None,
            generic: true,
            coarsenings: Vec::new(),
            fallbacks: Vec::new(),
        })
        .collect();
    entries.sort_by_key(|entry| entry.function);
    SpecializationOutcome {
        schema: SPECIALIZATION_SCHEMA.to_owned(),
        facts,
        manifest: SpecializationManifest {
            schema: SPECIALIZATION_SCHEMA.to_owned(),
            plan_fingerprint: plan.fingerprint(),
            assumption_digest: assumptions.digest(),
            entries,
            budget_usage: BudgetUsage::default(),
        },
    }
}

#[cfg(test)]
mod tests;
