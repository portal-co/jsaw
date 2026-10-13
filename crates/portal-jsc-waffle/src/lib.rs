pub mod backend_plan;
pub mod conv;
pub mod coregc;
pub mod coregc_emit;
pub mod coregc_layout;
pub mod coregc_lower;
pub mod coregc_roots;
pub mod coregc_runtime;
pub mod ingest;
pub mod linker;
pub mod nano;
pub mod nano_mvp;
pub mod nano_native;
pub mod repr;
pub mod revision;
pub mod specialize;
mod wasm_boundary;

pub use backend_plan::{
    BACKEND_PLAN_SCHEMA, BackendPlan, BackendPlanDiagnostic, BoundaryAbi, BoundaryDirection,
    BoundaryRecord, BoundaryTarget, CompletionKind, CompletionShape, DiagnosticKind, PlanArenaOp,
    PlanBlock, PlanBlockId, PlanCall, PlanCallKind, PlanConst, PlanField, PlanFuncId,
    PlanFunction, PlanFunctionKind, PlanGlobal, PlanHeapType, PlanInst, PlanLayout, PlanMemory,
    PlanRefTarget, PlanSigId, PlanSignature, PlanStorage, PlanTable, PlanTarget, PlanTerminator,
    PlanType, PlanValue, PlanValueDef, PlanValueId, RuntimeRequirements, TypeId,
};
pub use conv::{
    ComponentFunctions, ComponentHandle, ConvertOptions, IncrementalConverter, convert,
    convert_module, convert_modules,
};
pub use coregc::{
    COREGC_INVENTORY_SCHEMA, CoreGcError, CoreGcInventory, CoreGcOperation, CoreGcStorage,
    CoreGcType, CoreGcTypeId, CoreGcTypeKind,
};
pub use coregc_emit::{
    CoreGcArtifact, CoreGcHandleAbi, CoreGcHandleDirection, CoreGcHandleKind, CoreGcOptions,
    emit_coregc,
};
pub use coregc_layout::{
    COREGC_DESCRIPTOR_MAGIC, COREGC_DESCRIPTOR_VERSION, CoreGcDescriptorTable, CoreGcPayloadLayout,
    CoreGcSlotLayout,
};
pub use ingest::{
    module_set_from_sources, module_set_from_sources_lazy, parse_module_source,
    parse_module_source_lazy, with_globals,
};
pub use linker::ModuleSet;
pub use nano::{ArenaRef, NanoArena, NanoArenaError, NanoArenaHost, NanoLifetime, NanoPlan, NanoValueRepresentation};
pub use nano_mvp::{
    NANO_MVP_SCHEMA, NanoMvpConfig, NanoMvpError, emit_nano_mvp, emit_nano_mvp_fingerprint,
    emit_nano_mvp_with_config,
};
pub use nano_native::{
    AdapterAbiRecord, CapabilityCheck, InlineBudget, InlineRecord, InliningMode,
    NANO_NATIVE_SCHEMA, NanoCallDecision, NanoCallStrategy, NanoNativeConfig,
    NanoNativeDiagnostic, NanoNativeDiagnosticKind, NanoNativeOutcome, NanoNativePlan,
    ParamElision, RequiredCapabilities, TargetCapabilities, plan_nano_native,
};
pub use repr::ConvertError;
pub use specialize::{
    AliasClass, BoundaryLiveness, CallSiteParamEnv, CoarseningRecord, CoreGcFacts,
    CoreGcFastPath, EVAL_STEP_BOUND, EvalReport, FastPathHint, FlattenedFieldFact, FunctionFacts,
    MAX_ENV_DEPTH, ManifestEntry, MutationClass, Nullability, ObjectFact, SPECIALIZATION_SCHEMA,
    SpecializationDiagnostic, SpecializationDiagnosticKind, SpecializationKey,
    SpecializationManifest, SpecializationMode, SpecializationOutcome, Specializer, ValueFact,
    Variant, VariantBudget, VariantRequest, WaffleIdentity, WrappingArithmetic,
};
pub use revision::{
    ActivationError, ActiveRevision, ContentRevisionId, DeltaOperation, DispatchDeltaActivator,
    FragmentCache, FullRevisionReason, MemoryFragmentCache, ModuleDelta, ReinstantiatingActivator,
    ReloadPlan, ReloadProfile, RevisionArtifact, RevisionCompiler, RevisionManifest,
    RevisionOutput, RevisionRequest,
};
