//! Stable, artifact-owned storage and descriptors for the native boundary.
//!
//! [`ArenaRef`] is deliberately a process-local capability.  It contains no
//! address and is not serializable: an arena reference must never become part
//! of an artifact or an interchange format.

use serde::{Deserialize, Serialize};
use std::fmt;

/// Owner of arena identity for one artifact instance.
///
/// Arena IDs are scoped to their host, not process-global: two artifact
/// instances in the same process independently start from the same first ID,
/// and identity never depends on a shared counter. This keeps compiled
/// artifacts hermetic (no hidden cross-instance coupling) and keeps
/// `ArenaRef` a per-instance capability.
///
/// Hosts are expected to live as long as the arenas they vend; refs from one
/// host's arenas are invalid in any other host's arenas.
#[derive(Debug, Default)]
pub struct NanoArenaHost {
    next: u64,
}

impl NanoArenaHost {
    /// Create a fresh host. The first arena vended gets ID 1, so a zeroed or
    /// default-constructed value can never collide with a real arena.
    pub fn new() -> Self {
        Self { next: 1 }
    }

    /// Vend a new arena with a host-unique identity.
    pub fn arena(&mut self, limit: usize) -> NanoArena {
        let id = self.next;
        // u64 exhaustion is unreachable in practice (one arena per artifact
        // per #21); saturating at u64::MAX still keeps IDs distinct for any
        // real host lifetime.
        self.next = self.next.saturating_add(1);
        // The host token is an in-process identity only: it distinguishes
        // refs from different hosts without any process-global counter. It
        // is never dereferenced, never serialized, and has no meaning
        // outside this process.
        let host = self as *const NanoArenaHost as usize;
        NanoArena::with_id(host, id, limit)
    }
}

/// Errors returned by [`NanoArena`].
///
/// The variants intentionally carry no addresses or allocator details.  This
/// makes failures stable to inspect and safe to expose at the boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NanoArenaError {
    /// The configured capacity cannot satisfy the allocation.
    Exhausted,
    /// The requested allocation size or its layout arithmetic overflowed.
    SizeOverflow,
    /// The allocation slot number cannot be represented by [`ArenaRef`].
    SlotOverflow,
    /// The alignment is zero or is not a power of two.
    Misaligned,
    /// The reference does not belong to this arena generation.
    InvalidRef,
    /// A read or write extends beyond its allocation.
    OutOfBounds,
    /// Reconfiguration would make already-used storage unavailable.
    ConfigurationTooSmall,
    /// The arena generation can no longer be advanced safely.
    GenerationExhausted,
}

impl fmt::Display for NanoArenaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Exhausted => "arena exhausted",
            Self::SizeOverflow => "allocation size overflow",
            Self::SlotOverflow => "allocation slot overflow",
            Self::Misaligned => "invalid alignment",
            Self::InvalidRef => "invalid arena reference",
            Self::OutOfBounds => "arena access out of bounds",
            Self::ConfigurationTooSmall => "arena configuration is smaller than used storage",
            Self::GenerationExhausted => "arena generation exhausted",
        };
        f.write_str(name)
    }
}

impl std::error::Error for NanoArenaError {}

/// An opaque logical allocation.
///
/// This is a capability, not an address.  Its arena identity and generation
/// are private and intentionally have no `Serialize`/`Deserialize` impl; in
/// particular, serialized data cannot contain the process-local arena id.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArenaRef {
    /// In-process identity token of the owning host. Not an address that may
    /// be used, and never serialized — it only makes cross-host refs invalid.
    host: usize,
    arena: u64,
    generation: u64,
    slot: u32,
}

impl fmt::Debug for ArenaRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Do not expose even the slot: this is an opaque capability, not a
        // stable descriptor or an address that may be persisted.
        f.write_str("ArenaRef(..)")
    }
}

struct Allocation {
    bytes: Vec<u8>,
    /// Logical offset used for alignment accounting.  No address escapes.
    offset: usize,
    alignment: usize,
}

/// A bounded logical arena whose references survive allocation-vector growth.
pub struct NanoArena {
    host: usize,
    id: u64,
    generation: u64,
    limit: usize,
    used: usize,
    allocations: Vec<Allocation>,
}

impl NanoArena {
    /// Create an arena with a host-assigned identity. Construct via
    /// [`NanoArenaHost::arena`] so identity is scoped to the owning artifact
    /// instance rather than a process-global counter.
    fn with_id(host: usize, id: u64, limit: usize) -> Self {
        Self {
            host,
            id,
            generation: 1,
            limit,
            used: 0,
            allocations: Vec::new(),
        }
    }

    /// Change the capacity without invalidating existing allocations.
    ///
    /// Shrinking below the logical high-water mark is rejected, rather than
    /// making a live allocation appear to exceed the arena's capacity.
    pub fn configure(&mut self, limit: usize) -> Result<(), NanoArenaError> {
        if limit < self.used {
            return Err(NanoArenaError::ConfigurationTooSmall);
        }
        self.limit = limit;
        Ok(())
    }

    pub fn capacity(&self) -> usize {
        self.limit
    }

    pub fn used(&self) -> usize {
        self.used
    }

    /// Allocate a zero-initialized logical region.
    pub fn alloc(&mut self, size: usize, align: usize) -> Result<ArenaRef, NanoArenaError> {
        if size > u32::MAX as usize {
            return Err(NanoArenaError::SizeOverflow);
        }
        if align == 0 || !align.is_power_of_two() {
            return Err(NanoArenaError::Misaligned);
        }
        // A u32 slot is deliberately not allowed to wrap.  Rejecting at the
        // boundary also avoids a vector whose length cannot be represented by
        // the next logical slot.
        if self.allocations.len() >= u32::MAX as usize {
            return Err(NanoArenaError::SlotOverflow);
        }

        let aligned = self
            .used
            .checked_add(align - 1)
            .ok_or(NanoArenaError::SizeOverflow)?
            & !(align - 1);
        let end = aligned
            .checked_add(size)
            .ok_or(NanoArenaError::SizeOverflow)?;
        if end > self.limit {
            return Err(NanoArenaError::Exhausted);
        }

        // Fallible reservation prevents a huge configured limit from turning
        // a rejected allocation into an allocator panic.
        let mut bytes = Vec::new();
        bytes
            .try_reserve_exact(size)
            .map_err(|_| NanoArenaError::Exhausted)?;
        bytes.resize(size, 0);

        let slot = self.allocations.len() as u32;
        self.allocations.push(Allocation {
            bytes,
            offset: aligned,
            alignment: align,
        });
        self.used = end;
        Ok(ArenaRef {
            host: self.host,
            arena: self.id,
            generation: self.generation,
            slot,
        })
    }

    fn get(&self, reference: ArenaRef) -> Result<&Allocation, NanoArenaError> {
        if reference.host != self.host
            || reference.arena != self.id
            || reference.generation != self.generation
        {
            return Err(NanoArenaError::InvalidRef);
        }
        self.allocations
            .get(reference.slot as usize)
            .ok_or(NanoArenaError::InvalidRef)
    }

    fn get_mut(&mut self, reference: ArenaRef) -> Result<&mut Allocation, NanoArenaError> {
        if reference.host != self.host
            || reference.arena != self.id
            || reference.generation != self.generation
        {
            return Err(NanoArenaError::InvalidRef);
        }
        self.allocations
            .get_mut(reference.slot as usize)
            .ok_or(NanoArenaError::InvalidRef)
    }

    pub fn len(&self, reference: ArenaRef) -> Result<usize, NanoArenaError> {
        Ok(self.get(reference)?.bytes.len())
    }

    pub fn read(
        &self,
        reference: ArenaRef,
        offset: usize,
        out: &mut [u8],
    ) -> Result<(), NanoArenaError> {
        let allocation = self.get(reference)?;
        let end = offset
            .checked_add(out.len())
            .ok_or(NanoArenaError::OutOfBounds)?;
        if end > allocation.bytes.len() {
            return Err(NanoArenaError::OutOfBounds);
        }
        out.copy_from_slice(&allocation.bytes[offset..end]);
        Ok(())
    }

    pub fn write(
        &mut self,
        reference: ArenaRef,
        offset: usize,
        data: &[u8],
    ) -> Result<(), NanoArenaError> {
        let allocation = self.get_mut(reference)?;
        let end = offset
            .checked_add(data.len())
            .ok_or(NanoArenaError::OutOfBounds)?;
        if end > allocation.bytes.len() {
            return Err(NanoArenaError::OutOfBounds);
        }
        allocation.bytes[offset..end].copy_from_slice(data);
        Ok(())
    }

    /// Drop all allocations and invalidate every reference from this epoch.
    pub fn reset(&mut self) -> Result<(), NanoArenaError> {
        let generation = self
            .generation
            .checked_add(1)
            .ok_or(NanoArenaError::GenerationExhausted)?;
        self.allocations.clear();
        self.used = 0;
        self.generation = generation;
        Ok(())
    }
}

/// The representation used at a Nano boundary.  This is separate from
/// [`NanoLifetime`], which describes ownership/validity rather than encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
#[serde(rename_all = "snake_case")]
pub enum NanoValueRepresentation {
    Scalar = 0,
    InlineAggregate = 1,
    ArenaRef = 2,
    Boundary = 3,
}

/// The validity interval of a Nano boundary value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
#[serde(rename_all = "snake_case")]
pub enum NanoLifetime {
    Call = 0,
    Return = 1,
    Throw = 2,
    ExportRetained = 3,
    Artifact = 4,
}

/// A deterministic, serializable Nano boundary descriptor.
///
/// Only the descriptor is serializable.  In particular, an [`ArenaRef`] is a
/// runtime capability and is never embedded in this plan representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NanoPlan {
    pub schema_version: u16,
    pub representation: NanoValueRepresentation,
    pub lifetime: NanoLifetime,
}

impl NanoPlan {
    pub const SCHEMA_VERSION: u16 = 1;

    pub const fn new(representation: NanoValueRepresentation, lifetime: NanoLifetime) -> Self {
        Self {
            schema_version: Self::SCHEMA_VERSION,
            representation,
            lifetime,
        }
    }

    /// Stable numeric descriptor for deterministic tables and cache keys.
    pub const fn descriptor_key(&self) -> u32 {
        ((self.schema_version as u32) << 16)
            | ((self.representation as u32) << 8)
            | self.lifetime as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocation_read_write() {
        let mut host = NanoArenaHost::new();
        let mut arena = host.arena(32);
        let reference = arena.alloc(8, 8).unwrap();
        assert_eq!(arena.len(reference), Ok(8));

        let mut output = [0_u8; 8];
        arena.read(reference, 0, &mut output).unwrap();
        assert_eq!(output, [0; 8]);
        arena.write(reference, 2, &[1, 2, 3]).unwrap();
        arena.read(reference, 0, &mut output).unwrap();
        assert_eq!(output, [0, 0, 1, 2, 3, 0, 0, 0]);
    }

    #[test]
    fn growth_preserves_logical_refs() {
        let mut host = NanoArenaHost::new();
        let mut arena = host.arena(128);
        let first = arena.alloc(4, 1).unwrap();
        arena.write(first, 0, &[9, 8, 7, 6]).unwrap();
        for _ in 0..16 {
            let _ = arena.alloc(4, 1).unwrap();
        }
        let mut output = [0; 4];
        arena.read(first, 0, &mut output).unwrap();
        assert_eq!(output, [9, 8, 7, 6]);
    }

    #[test]
    fn stale_and_cross_arena_refs_fail() {
        let mut host = NanoArenaHost::new();
        let mut arena = host.arena(16);
        let reference = arena.alloc(4, 1).unwrap();
        let other = host.arena(16);
        let mut output = [0; 4];
        assert_eq!(arena.reset(), Ok(()));
        assert_eq!(arena.read(reference, 0, &mut output), Err(NanoArenaError::InvalidRef));
        assert_eq!(other.read(reference, 0, &mut output), Err(NanoArenaError::InvalidRef));
    }

    #[test]
    fn exhaustion_and_configuration_cannot_shrink_used_storage() {
        let mut host = NanoArenaHost::new();
        let mut arena = host.arena(8);
        let _ = arena.alloc(4, 1).unwrap();
        assert_eq!(arena.configure(3), Err(NanoArenaError::ConfigurationTooSmall));
        assert_eq!(arena.capacity(), 8);
        assert_eq!(arena.alloc(5, 1), Err(NanoArenaError::Exhausted));
        assert!(arena.alloc(4, 1).is_ok());
    }

    #[test]
    fn invalid_alignment_and_bounds_are_typed() {
        let mut host = NanoArenaHost::new();
        let mut arena = host.arena(16);
        assert_eq!(arena.alloc(1, 0), Err(NanoArenaError::Misaligned));
        assert_eq!(arena.alloc(1, 3), Err(NanoArenaError::Misaligned));
        let reference = arena.alloc(4, 1).unwrap();
        let mut output = [0; 2];
        assert_eq!(arena.read(reference, 3, &mut output), Err(NanoArenaError::OutOfBounds));
        assert_eq!(arena.write(reference, usize::MAX, &[1]), Err(NanoArenaError::OutOfBounds));
        if usize::BITS > 32 {
            assert_eq!(arena.alloc(u32::MAX as usize + 1, 1), Err(NanoArenaError::SizeOverflow));
        }
    }

    #[test]
    fn reset_invalidates_and_restarts_storage() {
        let mut host = NanoArenaHost::new();
        let mut arena = host.arena(8);
        let old = arena.alloc(8, 1).unwrap();
        arena.reset().unwrap();
        let new = arena.alloc(8, 1).unwrap();
        assert_ne!(old, new);
        assert_eq!(arena.used(), 8);
    }

    #[test]
    fn plan_descriptor_is_deterministic_and_separate() {
        let plan = NanoPlan::new(NanoValueRepresentation::ArenaRef, NanoLifetime::Return);
        assert_eq!(plan.schema_version, NanoPlan::SCHEMA_VERSION);
        assert_eq!(plan.descriptor_key(), 0x0001_0201);
        assert_ne!(
            NanoPlan::new(NanoValueRepresentation::ArenaRef, NanoLifetime::Call),
            NanoPlan::new(NanoValueRepresentation::Scalar, NanoLifetime::Return)
        );
    }
}
