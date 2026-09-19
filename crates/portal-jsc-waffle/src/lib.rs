pub mod conv;
pub mod coregc;
pub mod coregc_emit;
pub mod coregc_layout;
pub mod coregc_lower;
pub mod coregc_roots;
pub mod coregc_runtime;
pub mod ingest;
pub mod linker;
pub mod repr;
pub mod revision;
mod wasm_boundary;

pub use conv::{
    ComponentFunctions, ComponentHandle, ConvertOptions, IncrementalConverter, convert,
    convert_module, convert_modules,
};
pub use coregc::{
    COREGC_INVENTORY_SCHEMA, CoreGcError, CoreGcInventory, CoreGcOperation, CoreGcStorage,
    CoreGcType, CoreGcTypeId, CoreGcTypeKind,
};
pub use coregc_emit::{CoreGcArtifact, CoreGcOptions, emit_coregc};
pub use coregc_layout::{
    COREGC_DESCRIPTOR_MAGIC, COREGC_DESCRIPTOR_VERSION, CoreGcDescriptorTable, CoreGcPayloadLayout,
    CoreGcSlotLayout,
};
pub use ingest::{
    module_set_from_sources, module_set_from_sources_lazy, parse_module_source,
    parse_module_source_lazy, with_globals,
};
pub use linker::ModuleSet;
pub use repr::ConvertError;
pub use revision::{
    ActivationError, ActiveRevision, ContentRevisionId, DeltaOperation, DispatchDeltaActivator,
    FragmentCache, FullRevisionReason, MemoryFragmentCache, ModuleDelta, ReinstantiatingActivator,
    ReloadPlan, ReloadProfile, RevisionArtifact, RevisionCompiler, RevisionManifest,
    RevisionOutput, RevisionRequest,
};
