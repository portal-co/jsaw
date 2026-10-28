//! Fixed Rust runtime support for the Rust backend (DreamComp #40).
//!
//! Same contract as the C runtime ([`super::c_runtime`]), in generated-Rust
//! form: `unsafe` is confined to the documented boundaries (raw-pointer heap
//! access, function-table transmutes, `static mut` arena/root state).

/// Shared prelude: crate attributes, types, trap, arithmetic helpers.
pub const RUST_PRELUDE: &str = r#"// jsaw Rust backend artifact (DreamComp #40).
// Generated code: warnings are expected and not meaningful.
#![allow(dead_code, unused_mut, unused_assignments, unused_variables, unused_parens, clippy::all)]

pub type JsRef = usize;
pub type JsFnPtr = usize;

pub fn js_trap() -> ! {
    eprintln!("jsaw: trap");
    std::process::abort();
}

#[inline]
pub fn js_div_s32(a: i32, b: i32) -> i32 {
    if b == 0 || (a == i32::MIN && b == -1) {
        js_trap();
    }
    a.wrapping_div(b)
}
#[inline]
pub fn js_rem_s32(a: i32, b: i32) -> i32 {
    if b == 0 {
        js_trap();
    }
    if a == i32::MIN && b == -1 {
        return 0;
    }
    a.wrapping_rem(b)
}
#[inline]
pub fn js_div_u32(a: u32, b: u32) -> i32 {
    if b == 0 {
        js_trap();
    }
    (a / b) as i32
}
#[inline]
pub fn js_rem_u32(a: u32, b: u32) -> i32 {
    if b == 0 {
        js_trap();
    }
    (a % b) as i32
}
#[inline]
pub fn js_div_s64(a: i64, b: i64) -> i64 {
    if b == 0 || (a == i64::MIN && b == -1) {
        js_trap();
    }
    a.wrapping_div(b)
}
#[inline]
pub fn js_rem_s64(a: i64, b: i64) -> i64 {
    if b == 0 {
        js_trap();
    }
    if a == i64::MIN && b == -1 {
        return 0;
    }
    a.wrapping_rem(b)
}
#[inline]
pub fn js_div_u64(a: u64, b: u64) -> i64 {
    if b == 0 {
        js_trap();
    }
    (a / b) as i64
}
#[inline]
pub fn js_rem_u64(a: u64, b: u64) -> i64 {
    if b == 0 {
        js_trap();
    }
    (a % b) as i64
}

/// wasm f64 min: NaN propagates (canonical), -0 is smaller than +0.
#[inline]
pub fn js_wmin(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        return f64::from_bits(0x7ff8000000000000);
    }
    if a == b {
        return if a.is_sign_negative() { a } else { b };
    }
    if a < b { a } else { b }
}
/// wasm f64 max: NaN propagates (canonical), +0 is larger than -0.
#[inline]
pub fn js_wmax(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        return f64::from_bits(0x7ff8000000000000);
    }
    if a == b {
        return if a.is_sign_negative() { b } else { a };
    }
    if a > b { a } else { b }
}
"#;

/// Nano lane runtime: arena buffer + accessors.
pub const RUST_RUNTIME_NANO: &str = r#"// ---- nano lane runtime (arena-offset references; no collector) ----
static mut JS_NANO_ARENA: *mut u8 = std::ptr::null_mut();
static mut JS_NANO_ARENA_CAP: u64 = 0;

/// Contract: zero-fill any fresh region; return 0 on success, nonzero on
/// exhaustion (the caller traps deterministically).
pub fn js_nano_arena_ensure(end: i64) -> i32 {
    if end < 0 {
        return 1;
    }
    unsafe {
        if end as u64 <= JS_NANO_ARENA_CAP {
            return 0;
        }
        let mut cap: u64 = if JS_NANO_ARENA_CAP == 0 { 65536 } else { JS_NANO_ARENA_CAP };
        while cap < end as u64 {
            if cap > u64::MAX / 2 {
                return 1;
            }
            cap *= 2;
        }
        let new_layout =
            std::alloc::Layout::from_size_align(cap as usize, 16).unwrap_or_else(|_| js_trap());
        let fresh = if JS_NANO_ARENA.is_null() {
            std::alloc::alloc_zeroed(new_layout)
        } else {
            let old_layout = std::alloc::Layout::from_size_align(JS_NANO_ARENA_CAP as usize, 16)
                .unwrap_or_else(|_| js_trap());
            let p = std::alloc::realloc(JS_NANO_ARENA, old_layout, cap as usize);
            if !p.is_null() {
                std::ptr::write_bytes(p.add(JS_NANO_ARENA_CAP as usize), 0, (cap - JS_NANO_ARENA_CAP) as usize);
            }
            p
        };
        if fresh.is_null() {
            return 1;
        }
        JS_NANO_ARENA = fresh;
        JS_NANO_ARENA_CAP = cap;
        0
    }
}

macro_rules! js_ld {
    ($name:ident, $ty:ty) => {
        #[inline]
        pub fn $name(a: u64) -> $ty {
            unsafe { (JS_NANO_ARENA.add(a as usize) as *const $ty).read_unaligned() }
        }
    };
}
macro_rules! js_st {
    ($name:ident, $ty:ty) => {
        #[inline]
        pub fn $name(a: u64, v: $ty) {
            unsafe { (JS_NANO_ARENA.add(a as usize) as *mut $ty).write_unaligned(v) }
        }
    };
}
js_ld!(js_ld_i8, i8);
js_ld!(js_ld_i16, i16);
js_ld!(js_ld_i32, i32);
js_ld!(js_ld_i64, i64);
js_ld!(js_ld_f32, f32);
js_ld!(js_ld_f64, f64);
js_ld!(js_ld_ref, JsRef);
js_st!(js_st_i8, i8);
js_st!(js_st_i16, i16);
js_st!(js_st_i32, i32);
js_st!(js_st_i64, i64);
js_st!(js_st_f32, f32);
js_st!(js_st_f64, f64);
js_st!(js_st_ref, JsRef);

pub fn js_boundary_reset() {}
"#;

/// CoreGC lane runtime: root stack, collector, allocation, array copy.
///
/// The module glue provides `js_layout_for` (per-type sizes and reference
/// offsets) and the layout structs; this text is lane-fixed.
pub const RUST_RUNTIME_COREGC: &str = r#"// ---- CoreGC lane runtime (thin header'd pointers, precise mark/sweep) ----
#[repr(C)]
pub struct JsHd {
    pub type_id: i32,
    pub flags: i32, // bit 0: collector mark bit
    pub len: i32,
    pub pad: i32,
    pub next: *mut JsHd, // all-allocations list (runtime-private)
}

/// Rendered by the Rust backend from the skeleton's layouts.
pub struct JsLayoutInfo {
    pub type_id: u32,
    pub is_array: bool,
    pub elem_size: u32,      // arrays only
    pub elem_is_ref: bool,   // arrays only
    pub ref_offsets: &'static [u32], // structs only: offsetof each ref field
    pub size: u32,           // structs: size_of; arrays: unused
    pub data_off: u32,       // arrays: offsetof data
}
// `js_layout_for` is emitted by the renderer later in the same module (item
// order is insignificant in Rust).

/// Byte offset of the element payload of a headered array of `T`.
pub const fn js_data_off<T>() -> usize {
    let s = core::mem::size_of::<JsHd>();
    let a = core::mem::align_of::<T>();
    (s + a - 1) / a * a
}

static mut JS_GC_ALL: *mut JsHd = std::ptr::null_mut();
static mut JS_GC_SINCE: u64 = 0;
static mut JS_GC_THRESHOLD: u64 = 1 << 20;
#[allow(dead_code)]
pub fn js_coregc_set_threshold(t: u64) {
    unsafe { JS_GC_THRESHOLD = t };
}

static mut JS_ROOTS: Vec<JsRef> = Vec::new();
static mut JS_BASES: Vec<u32> = Vec::new();

pub fn js_coregc_root_enter(slots: i32) {
    if slots < 0 {
        js_trap();
    }
    unsafe {
        JS_BASES.push(JS_ROOTS.len() as u32);
        JS_ROOTS.resize(JS_ROOTS.len() + slots as usize, 0);
    }
}
pub fn js_coregc_root_store(slot: i32, v: JsRef) {
    unsafe {
        let Some(&base) = JS_BASES.last() else { js_trap() };
        JS_ROOTS[base as usize + slot as usize] = v;
    }
}
pub fn js_coregc_root_pop() {
    unsafe {
        let Some(base) = JS_BASES.pop() else { js_trap() };
        JS_ROOTS.truncate(base as usize);
    }
}

unsafe fn js_coregc_mark(w: JsRef) {
    if w == 0 || (w & 1) != 0 {
        return; // null or i31 immediate
    }
    let h = w as *mut JsHd;
    unsafe {
        if (*h).flags & 1 != 0 {
            return;
        }
        (*h).flags |= 1;
    }
    let li = unsafe { js_layout_for((*h).type_id) }.unwrap_or_else(|| js_trap());
    unsafe {
        if li.is_array {
            if li.elem_is_ref {
                for i in 0..(*h).len as usize {
                    js_coregc_mark(
                        *((w as *const u8).add(li.data_off as usize + i * li.elem_size as usize)
                            as *const JsRef),
                    );
                }
            }
        } else {
            for &off in li.ref_offsets {
                js_coregc_mark(*((w as *const u8).add(off as usize) as *const JsRef));
            }
        }
    }
}

unsafe fn js_coregc_collect() {
    unsafe {
        for i in 0..JS_ROOTS.len() {
            js_coregc_mark(JS_ROOTS[i]);
        }
        let mut pp: *mut *mut JsHd = &raw mut JS_GC_ALL;
        while !(*pp).is_null() {
            let h = *pp;
            if (*h).flags & 1 != 0 {
                (*h).flags &= !1;
                pp = &raw mut (*h).next;
            } else {
                *pp = (*h).next;
                let li = js_layout_for((*h).type_id).unwrap_or_else(|| js_trap());
                let size = if li.is_array {
                    li.data_off as usize + (*h).len as usize * li.elem_size as usize
                } else {
                    li.size as usize
                };
                std::alloc::dealloc(
                    h as *mut u8,
                    std::alloc::Layout::from_size_align(size, 16).unwrap_or_else(|_| js_trap()),
                );
            }
        }
        JS_GC_SINCE = 0;
    }
}

pub fn js_coregc_alloc(type_id: i32, len: i32) -> JsRef {
    let li = unsafe { js_layout_for(type_id) }.unwrap_or_else(|| js_trap());
    if len < 0 {
        js_trap();
    }
    let size = if li.is_array {
        li.data_off as usize + len as usize * li.elem_size as usize
    } else {
        li.size as usize
    };
    unsafe {
        JS_GC_SINCE += size as u64;
        if JS_GC_SINCE > JS_GC_THRESHOLD {
            js_coregc_collect();
        }
        let h = std::alloc::alloc_zeroed(
            std::alloc::Layout::from_size_align(size, 16).unwrap_or_else(|_| js_trap()),
        ) as *mut JsHd;
        if h.is_null() {
            js_trap();
        }
        (*h).type_id = type_id;
        (*h).len = len;
        (*h).next = JS_GC_ALL;
        JS_GC_ALL = h;
        h as JsRef
    }
}

pub fn js_coregc_array_copy(d: JsRef, di: i32, s: JsRef, si: i32, len: i32, elem_log2: i32) {
    if d == 0 || s == 0 {
        js_trap();
    }
    unsafe {
        let dh = d as *mut JsHd;
        let sh = s as *mut JsHd;
        if di < 0 || si < 0 || len < 0 || di as i64 + len as i64 > (*dh).len as i64
            || si as i64 + len as i64 > (*sh).len as i64
        {
            js_trap();
        }
        let li = js_layout_for((*dh).type_id).unwrap_or_else(|| js_trap());
        std::ptr::copy(
            (s as *const u8).add(li.data_off as usize + ((si as usize) << elem_log2)),
            (d as *mut u8).add(li.data_off as usize + ((di as usize) << elem_log2)),
            (len as usize) << elem_log2,
        );
    }
}

pub fn js_boundary_reset() {
    unsafe {
        JS_ROOTS.truncate(0);
        JS_BASES.truncate(0);
    }
}
"#;
