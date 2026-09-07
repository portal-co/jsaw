//! Shared core for the mobile emitters.
//!
//! The jsaw pipeline lowers ECMAScript all the way to a `portal-pc-waffle`
//! IR module (a WasmGC module 1:1) before the Wasm backend encodes it. The
//! mobile emitters fork off at the same point: they consume the same IR —
//! and therefore share the frontend and every optimization (return-kind
//! analysis, tail dispatch, dedicated multi-return layouts, primordial fast
//! cores, shapes) by construction.
//!
//! This crate holds the target-independent half of that fork:
//!
//! - [`audit`] — the feature-closure contract. Only modules inside the
//!   closure the jsaw compiler actually emits are accepted; anything else
//!   is a hard error, never a silent fallback.
//! - [`sir`] — a structured, target-neutral function IR built from the
//!   same post-Stackify/Localify artifacts the Wasm opcode encoder
//!   consumes (blocks are labels, block params are locals, values are
//!   expression trees).
//! - [`lower`] — the IR -> SIR walker.
//! - [`names`] — identifier sanitization shared by both backends.
//! - [`tail`] — the tail-callable-set computation used to plan O(1)-stack
//!   trampolining for `ReturnCall`/`ReturnCallRef`.

pub mod audit;
pub mod lower;
pub mod names;
pub mod sir;
pub mod tail;
