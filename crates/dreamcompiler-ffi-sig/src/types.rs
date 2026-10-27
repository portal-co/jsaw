//! FFI signature AST types (parse-only; no pit dependency).

/// 32-byte PIT resource interface id.
pub type PitRid = [u8; 32];

/// C ABI scalar / pointer types in hybrid signatures.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FfiAbiTy {
    Slot,
    Ptr,
    I32,
    Void,
}

/// PIT interface reference `P<rid>`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FfiPitTy {
    pub rid: PitRid,
}

/// A type slot in a hybrid signature.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FfiTy {
    Abi(FfiAbiTy),
    Pit(FfiPitTy),
}

impl FfiTy {
    pub fn is_pit(&self) -> bool {
        matches!(self, FfiTy::Pit(_))
    }

    pub fn pit_rid(&self) -> Option<PitRid> {
        match self {
            FfiTy::Pit(p) => Some(p.rid),
            _ => None,
        }
    }
}

/// One method entry on a PIT interface (trailing method table).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FfiMethodSig {
    pub name: String,
    pub args: Vec<FfiTy>,
    pub ret: FfiTy,
}

/// Callable signature for `ffi(sym, sig)` — always a function type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FfiCallableSig {
    pub args: Vec<FfiTy>,
    pub ret: FfiTy,
    pub methods: Vec<FfiMethodSig>,
}

impl FfiCallableSig {
    pub fn return_pit_rid(&self) -> Option<PitRid> {
        self.ret.pit_rid()
    }
}

pub fn rid_hex(rid: &PitRid) -> String {
    rid.iter().map(|b| format!("{b:02x}")).collect()
}
