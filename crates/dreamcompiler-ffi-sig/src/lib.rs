//! Parse hybrid FFI signature strings (PIT + C ABI) into structured metadata.

mod parse;
mod types;

pub use parse::{ParseError, parse_sig};
pub use types::{FfiAbiTy, FfiCallableSig, FfiMethodSig, FfiPitTy, FfiTy, PitRid, rid_hex};
