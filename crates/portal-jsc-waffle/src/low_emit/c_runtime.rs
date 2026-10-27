//! Fixed C runtime support for the C backend (DreamComp #38).
//!
//! The #36 design ships per-target GC runtimes as hand-written support code
//! rather than porting them through the [`Representation`](super::Representation)
//! trait. This module holds that code as three text blocks:
//!
//! - [`C_PRELUDE`]: shared types, trap, bit-exact float/reinterpret helpers,
//!   and guarded arithmetic helpers (division/remainder trap semantics,
//!   saturating truncations, wasm float min/max).
//! - [`C_RUNTIME_NANO`]: the arena buffer, `js_nano_arena_ensure`, and
//!   alias-safe `js_ld_*`/`js_st_*` accessors.
//! - [`C_RUNTIME_COREGC`]: the shadow root stack, the precise mark/sweep
//!   collector (driven by the module-emitted `js_layout_for` table),
//!   `js_coregc_alloc`, and `js_coregc_array_copy`.
//!
//! Every artifact also gets `js_boundary_reset()`: the runtime resets any
//! cross-call state (the CoreGC shadow root stack) at artifact entry, since
//! trap exits may leak root frames.

/// Shared prelude: always first in the emitted file.
pub const C_PRELUDE: &str = r#"/* jsaw C backend artifact (DreamComp #38).
 * C99. One documented extension: function tables are js_fnptr_t arrays and
 * indirect calls cast back to the callee's signature (POSIX dlsym-style;
 * universal on hosted targets). All integer arithmetic is unsigned-wrapped;
 * division/remainder and float truncations go through deterministic helpers.
 */
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

typedef uintptr_t js_ref;
typedef void (*js_fnptr_t)(void);

static void js_trap(void) {
  fprintf(stderr, "jsaw: trap\n");
  abort();
}

static inline float js_f32bits(uint32_t b) { float f; memcpy(&f, &b, 4); return f; }
static inline double js_f64bits(uint64_t b) { double d; memcpy(&d, &b, 8); return d; }
static inline uint32_t js_bits_f32(float f) { uint32_t b; memcpy(&b, &f, 4); return b; }
static inline uint64_t js_bits_f64(double d) { uint64_t b; memcpy(&b, &d, 8); return b; }

/* f32 arithmetic out-of-line so the ABI forces single-precision rounding
 * regardless of FLT_EVAL_METHOD. */
static float js_f32add(float a, float b) { return a + b; }
static float js_f32sub(float a, float b) { return a - b; }
static float js_f32mul(float a, float b) { return a * b; }
static float js_f32div(float a, float b) { return a / b; }

static int32_t js_div_s32(int32_t a, int32_t b) {
  if (b == 0) js_trap();
  if (a == INT32_MIN && b == -1) js_trap();
  return a / b;
}
static int32_t js_rem_s32(int32_t a, int32_t b) {
  if (b == 0) js_trap();
  if (a == INT32_MIN && b == -1) return 0;
  return a % b;
}
static int32_t js_div_u32(uint32_t a, uint32_t b) {
  if (b == 0) js_trap();
  return (int32_t)(a / b);
}
static int32_t js_rem_u32(uint32_t a, uint32_t b) {
  if (b == 0) js_trap();
  return (int32_t)(a % b);
}
static int64_t js_div_s64(int64_t a, int64_t b) {
  if (b == 0) js_trap();
  if (a == INT64_MIN && b == -1) js_trap();
  return a / b;
}
static int64_t js_rem_s64(int64_t a, int64_t b) {
  if (b == 0) js_trap();
  if (a == INT64_MIN && b == -1) return 0;
  return a % b;
}
static int64_t js_div_u64(uint64_t a, uint64_t b) {
  if (b == 0) js_trap();
  return (int64_t)(a / b);
}
static int64_t js_rem_u64(uint64_t a, uint64_t b) {
  if (b == 0) js_trap();
  return (int64_t)(a % b);
}

static int32_t js_trunc_sat_s32(double x) {
  if (isnan(x)) return 0;
  if (x >= 2147483648.0) return INT32_MAX;
  if (x < -2147483648.0) return INT32_MIN;
  return (int32_t)x;
}
static int32_t js_trunc_sat_u32(double x) {
  if (isnan(x)) return 0;
  if (x >= 4294967296.0) return (int32_t)UINT32_MAX;
  if (x <= 0.0) return 0;
  return (int32_t)(uint32_t)x;
}
static int64_t js_trunc_sat_s64(double x) {
  if (isnan(x)) return 0;
  if (x >= 9223372036854775808.0) return INT64_MAX;
  if (x < -9223372036854775808.0) return INT64_MIN;
  return (int64_t)x;
}
static int64_t js_trunc_sat_u64(double x) {
  if (isnan(x)) return 0;
  if (x >= 18446744073709551616.0) return (int64_t)UINT64_MAX;
  if (x <= 0.0) return 0;
  return (int64_t)(uint64_t)x;
}

/* wasm f64 min/max: NaN propagates, -0 is smaller than +0. */
static double js_wmin(double a, double b) {
  if (isnan(a) || isnan(b)) return js_f64bits(UINT64_C(0x7ff8000000000000));
  if (a == b) return signbit(a) ? a : b;
  return a < b ? a : b;
}
static double js_wmax(double a, double b) {
  if (isnan(a) || isnan(b)) return js_f64bits(UINT64_C(0x7ff8000000000000));
  if (a == b) return signbit(a) ? b : a;
  return a > b ? a : b;
}
"#;

/// Nano lane runtime: arena buffer + alias-safe accessors.
pub const C_RUNTIME_NANO: &str = r#"/* ---- nano lane runtime (arena-offset references; no collector) ---- */
static uint8_t *js_nano_arena = NULL;
static uint64_t js_nano_arena_cap = 0;

/* Contract: zero-fill any fresh region; return 0 on success, nonzero on
 * exhaustion (the caller traps deterministically). */
int32_t js_nano_arena_ensure(int64_t end) {
  if (end < 0) return 1;
  if ((uint64_t)end <= js_nano_arena_cap) return 0;
  uint64_t cap = js_nano_arena_cap ? js_nano_arena_cap : 65536;
  while (cap < (uint64_t)end) {
    if (cap > UINT64_MAX / 2) return 1;
    cap *= 2;
  }
  uint8_t *p = (uint8_t *)realloc(js_nano_arena, (size_t)cap);
  if (!p) return 1;
  memset(p + js_nano_arena_cap, 0, (size_t)(cap - js_nano_arena_cap));
  js_nano_arena = p;
  js_nano_arena_cap = cap;
  return 0;
}

static inline int8_t js_ld_i8(uint64_t a) { int8_t v; memcpy(&v, js_nano_arena + (size_t)a, 1); return v; }
static inline int16_t js_ld_i16(uint64_t a) { int16_t v; memcpy(&v, js_nano_arena + (size_t)a, 2); return v; }
static inline int32_t js_ld_i32(uint64_t a) { int32_t v; memcpy(&v, js_nano_arena + (size_t)a, 4); return v; }
static inline int64_t js_ld_i64(uint64_t a) { int64_t v; memcpy(&v, js_nano_arena + (size_t)a, 8); return v; }
static inline float js_ld_f32(uint64_t a) { float v; memcpy(&v, js_nano_arena + (size_t)a, 4); return v; }
static inline double js_ld_f64(uint64_t a) { double v; memcpy(&v, js_nano_arena + (size_t)a, 8); return v; }
static inline js_ref js_ld_ref(uint64_t a) { js_ref v; memcpy(&v, js_nano_arena + (size_t)a, sizeof(js_ref)); return v; }
static inline void js_st_i8(uint64_t a, int8_t v) { memcpy(js_nano_arena + (size_t)a, &v, 1); }
static inline void js_st_i16(uint64_t a, int16_t v) { memcpy(js_nano_arena + (size_t)a, &v, 2); }
static inline void js_st_i32(uint64_t a, int32_t v) { memcpy(js_nano_arena + (size_t)a, &v, 4); }
static inline void js_st_i64(uint64_t a, int64_t v) { memcpy(js_nano_arena + (size_t)a, &v, 8); }
static inline void js_st_f32(uint64_t a, float v) { memcpy(js_nano_arena + (size_t)a, &v, 4); }
static inline void js_st_f64(uint64_t a, double v) { memcpy(js_nano_arena + (size_t)a, &v, 8); }
static inline void js_st_ref(uint64_t a, js_ref v) { memcpy(js_nano_arena + (size_t)a, &v, sizeof(js_ref)); }

static void js_boundary_reset(void) {}
"#;

/// CoreGC lane runtime: root stack, collector, allocation, array copy.
///
/// The module glue provides `js_layout_for` (per-type sizes and reference
/// offsets) and the real C structs; this text is lane-fixed.
pub const C_RUNTIME_COREGC: &str = r#"/* ---- CoreGC lane runtime (thin header'd pointers, precise mark/sweep) ---- */
typedef struct js_hd js_hd;
struct js_hd {
  int32_t type_id;
  int32_t flags; /* bit 0: collector mark bit */
  int32_t len;
  int32_t pad;
  js_hd *next;   /* all-allocations list (runtime-private) */
};

/* Rendered by the C backend from the skeleton's layouts. */
typedef struct {
  uint32_t type_id;
  uint32_t is_array;
  uint32_t elem_size;    /* arrays only */
  uint32_t elem_is_ref;  /* arrays only */
  uint32_t n_ref;        /* structs only */
  const uint32_t *ref_offsets; /* structs only: offsetof each ref field */
  uint32_t size;         /* structs: sizeof; arrays: unused */
  uint32_t data_off;     /* arrays: offsetof data */
} js_layout_info;
extern const js_layout_info *js_layout_for(int32_t type_id);

static js_hd *js_gc_all = NULL;
static uint64_t js_gc_since = 0;
static uint64_t js_gc_threshold = UINT64_C(1) << 20;
void js_coregc_set_threshold(uint64_t t) { js_gc_threshold = t; }

static js_ref *js_roots = NULL;
static uint32_t js_roots_len = 0, js_roots_cap = 0;
static uint32_t *js_bases = NULL;
static uint32_t js_bases_len = 0, js_bases_cap = 0;

void js_coregc_root_enter(int32_t slots) {
  if (slots < 0) js_trap();
  if (js_bases_len == js_bases_cap) {
    uint32_t cap = js_bases_cap ? js_bases_cap * 2 : 64;
    uint32_t *p = (uint32_t *)realloc(js_bases, (size_t)cap * sizeof(uint32_t));
    if (!p) js_trap();
    js_bases = p;
    js_bases_cap = cap;
  }
  js_bases[js_bases_len++] = js_roots_len;
  uint32_t need = js_roots_len + (uint32_t)slots;
  if (need > js_roots_cap) {
    uint32_t cap = js_roots_cap ? js_roots_cap : 256;
    while (cap < need) cap *= 2;
    js_ref *p = (js_ref *)realloc(js_roots, (size_t)cap * sizeof(js_ref));
    if (!p) js_trap();
    js_roots = p;
    js_roots_cap = cap;
  }
  for (uint32_t i = js_roots_len; i < need; i++) js_roots[i] = 0;
  js_roots_len = need;
}
void js_coregc_root_store(int32_t slot, js_ref v) {
  if (js_bases_len == 0) js_trap();
  js_roots[js_bases[js_bases_len - 1] + (uint32_t)slot] = v;
}
void js_coregc_root_pop(void) {
  if (js_bases_len == 0) js_trap();
  js_roots_len = js_bases[--js_bases_len];
}

static void js_coregc_mark(js_ref w) {
  if (w == 0 || (w & 1)) return; /* null or i31 immediate */
  js_hd *h = (js_hd *)(uintptr_t)w;
  if (h->flags & 1) return;
  h->flags |= 1;
  const js_layout_info *li = js_layout_for(h->type_id);
  if (!li) js_trap();
  if (li->is_array) {
    if (li->elem_is_ref) {
      for (int32_t i = 0; i < h->len; i++)
        js_coregc_mark(*(js_ref *)((char *)h + li->data_off + (size_t)i * li->elem_size));
    }
  } else {
    for (uint32_t i = 0; i < li->n_ref; i++)
      js_coregc_mark(*(js_ref *)((char *)h + li->ref_offsets[i]));
  }
}

static void js_coregc_collect(void) {
  for (uint32_t i = 0; i < js_roots_len; i++) js_coregc_mark(js_roots[i]);
  js_hd **pp = &js_gc_all;
  while (*pp) {
    js_hd *h = *pp;
    if (h->flags & 1) {
      h->flags &= ~1;
      pp = &h->next;
    } else {
      *pp = h->next;
      free(h);
    }
  }
  js_gc_since = 0;
}

js_ref js_coregc_alloc(int32_t type_id, int32_t len) {
  const js_layout_info *li = js_layout_for(type_id);
  if (!li || len < 0) js_trap();
  size_t size = li->is_array
      ? (size_t)li->data_off + (size_t)len * li->elem_size
      : (size_t)li->size;
  js_gc_since += size;
  if (js_gc_since > js_gc_threshold) js_coregc_collect();
  js_hd *h = (js_hd *)calloc(1, size);
  if (!h) js_trap();
  h->type_id = type_id;
  h->len = len;
  h->next = js_gc_all;
  js_gc_all = h;
  return (js_ref)(uintptr_t)h;
}

void js_coregc_array_copy(js_ref d, int32_t di, js_ref s, int32_t si,
                          int32_t len, int32_t elem_log2) {
  if (d == 0 || s == 0) js_trap();
  js_hd *dh = (js_hd *)(uintptr_t)d;
  js_hd *sh = (js_hd *)(uintptr_t)s;
  if (di < 0 || si < 0 || len < 0 || (int64_t)di + len > dh->len ||
      (int64_t)si + len > sh->len)
    js_trap();
  const js_layout_info *li = js_layout_for(dh->type_id);
  if (!li) js_trap();
  memmove((char *)d + li->data_off + ((size_t)di << elem_log2),
          (char *)s + li->data_off + ((size_t)si << elem_log2),
          (size_t)len << elem_log2);
}

static void js_boundary_reset(void) {
  js_roots_len = 0;
  js_bases_len = 0;
}
"#;
