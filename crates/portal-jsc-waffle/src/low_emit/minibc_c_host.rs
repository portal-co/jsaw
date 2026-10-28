//! C host for the MiniBC backend's acceptance test (DreamComp #39): a fixed
//! C harness that drives the **generated** MiniBC C interpreter header
//! (`dreamcompiler-minibc-c-interp`) over a jsaw artifact, implementing the
//! jsaw `0x9B` host-op family (nano arena, CoreGC handle heap with precise
//! mark/sweep, globals) and the `StdExtOp` control-flow exits exactly as
//! `dreamcompiler_minibc::interp::MiniInterp` does. Together with
//! [`c_meta_source`] (per-case layout/global/entry tables) this proves the
//! generated C interpreter runs jsaw artifacts unmodified.

use super::minibc_render::MinibcArtifact;
use super::LeModule;
use std::fmt::Write as _;

/// The fixed C harness (host state, host ops, GC, ext-op driver loop,
/// `main`). `#include`s `minii.h` (the generated interpreter header) and the
/// per-case metadata (via `-include meta.c` or textual concatenation).
pub const C_HOST_HARNESS: &str = r#"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ── host state ─────────────────────────────────────────────────────────── */
static uint8_t *js_arena = NULL;
static uint64_t js_arena_len = 0, js_arena_cap = 0;

typedef struct {
    uint32_t type_id;
    int marked;
    uint8_t *data;
} JsObj;
static JsObj *js_objects = NULL;
static uint64_t js_n_objects = 0, js_cap_objects = 0;
static uint32_t *js_free = NULL;
static uint64_t js_n_free = 0, js_cap_free = 0;

static uint64_t **js_roots = NULL;      /* frame base pointers */
static uint64_t *js_root_bufs = NULL;   /* frame storage (contiguous stack) */
static uint64_t *js_root_counts = NULL;
static uint64_t js_n_frames = 0, js_cap_frames = 0, js_root_buf_len = 0, js_root_buf_cap = 0;

static uint64_t js_since_gc = 0;
#define JS_GC_THRESHOLD (1u << 20)

/* ── helpers ────────────────────────────────────────────────────────────── */
static uint64_t js_read(const uint8_t *d, uint64_t off, uint32_t n) {
    uint64_t v = 0;
    memcpy(&v, d + off, n);
    return v;
}
static void js_write(uint8_t *d, uint64_t off, uint64_t v, uint32_t n) {
    memcpy(d + off, &v, n);
}

/* handle word: odd = i31 immediate (not an object), even = (index+1)*2. */
static int64_t js_handle_of(uint64_t w) {
    if (w == 0 || (w & 1)) return -1;
    uint64_t idx = w / 2 - 1;
    if (idx >= js_n_objects) return -1;
    if (!js_objects[idx].data) return -1;
    return (int64_t)idx;
}

static void js_gc_collect(void) {
    uint64_t *work = NULL;
    uint64_t nw = 0, capw = 0;
#define JS_PUSH(x) do { \
        if (nw == capw) { capw = capw ? capw * 2 : 256; work = realloc(work, capw * 8); } \
        work[nw++] = (x); \
    } while (0)
    for (uint64_t f = 0; f < js_n_frames; f++)
        for (uint64_t i = 0; i < js_root_counts[f]; i++)
            JS_PUSH(js_roots[f][i]);
    while (nw) {
        uint64_t w = work[--nw];
        int64_t idx = js_handle_of(w);
        if (idx < 0) continue;
        JsObj *o = &js_objects[idx];
        if (o->marked) continue;
        o->marked = 1;
        uint32_t tid = o->type_id;
        if (tid >= JS_N_TYPES) continue;
        int32_t li_i = JS_TYPE_TO_LAYOUT[tid];
        if (li_i < 0) continue;
        const JsLayout *li = &JS_LAYOUTS[li_i];
        if (li->elem_size) { /* array */
            if (li->elem_is_ref) {
                uint32_t len = (uint32_t)js_read(o->data, 8, 4);
                for (uint32_t k = 0; k < len; k++)
                    JS_PUSH(js_read(o->data, li->data_off + (uint64_t)k * li->elem_size, 8));
            }
        } else {
            for (uint32_t r = 0; r < li->n_refs; r++)
                JS_PUSH(js_read(o->data, li->ref_off[r], 8));
        }
    }
    for (uint64_t i = 0; i < js_n_objects; i++) {
        if (!js_objects[i].data) continue;
        if (js_objects[i].marked) {
            js_objects[i].marked = 0;
        } else {
            free(js_objects[i].data);
            js_objects[i].data = NULL;
            if (js_n_free == js_cap_free) { js_cap_free = js_cap_free ? js_cap_free * 2 : 64; js_free = realloc(js_free, js_cap_free * 4); }
            js_free[js_n_free++] = (uint32_t)i;
        }
    }
    free(work);
    js_since_gc = 0;
#undef JS_PUSH
}

static JsObj *js_obj(uint64_t w) {
    int64_t idx = js_handle_of(w);
    if (idx < 0) { fprintf(stderr, "jsaw C host: bad handle %llu\n", (unsigned long long)w); abort(); }
    return &js_objects[idx];
}

/* ── host ops (the jsaw 0x9B family; indices per minibc_render::host_idx) ── */
static uint64_t js_host(uint32_t idx, uint64_t *args) {
    switch (idx) {
    case 0: { /* NANO_ARENA_ENSURE(end) */
        uint64_t end = args[0];
        if (js_arena_len < end) {
            if (js_arena_cap < end) {
                uint64_t nc = js_arena_cap ? js_arena_cap : 4096;
                while (nc < end) nc *= 2;
                js_arena = realloc(js_arena, nc);
                js_arena_cap = nc;
            }
            memset(js_arena + js_arena_len, 0, end - js_arena_len);
            js_arena_len = end;
        }
        return 0;
    }
    case 1: /* NANO_LOAD(addr, size_log2) -> word */
        return js_read(js_arena, args[0], 1u << (uint32_t)args[1]);
    case 2: /* NANO_STORE(addr, size_log2, value) */
        js_write(js_arena, args[0], args[2], 1u << (uint32_t)args[1]);
        return 0;
    case 3: { /* COREGC_ALLOC(type_id, len) -> handle */
        uint32_t tid = (uint32_t)args[0];
        uint32_t len = (uint32_t)args[1];
        if (tid >= JS_N_TYPES) { fprintf(stderr, "alloc: bad type id\n"); abort(); }
        int32_t li_i = JS_TYPE_TO_LAYOUT[tid];
        if (li_i < 0) { fprintf(stderr, "alloc: unknown type id\n"); abort(); }
        const JsLayout *li = &JS_LAYOUTS[li_i];
        uint64_t size = li->elem_size ? li->data_off + (uint64_t)len * li->elem_size : li->size;
        js_since_gc += size;
        if (js_since_gc > JS_GC_THRESHOLD) js_gc_collect();
        uint8_t *data = calloc(1, size ? size : 1);
        js_write(data, 0, tid, 4);
        js_write(data, 8, len, 4);
        uint32_t oi;
        if (js_n_free) {
            oi = js_free[--js_n_free];
            js_objects[oi].type_id = tid;
            js_objects[oi].marked = 0;
            js_objects[oi].data = data;
        } else {
            if (js_n_objects == js_cap_objects) {
                js_cap_objects = js_cap_objects ? js_cap_objects * 2 : 256;
                js_objects = realloc(js_objects, js_cap_objects * sizeof(JsObj));
            }
            oi = (uint32_t)js_n_objects++;
            js_objects[oi].type_id = tid;
            js_objects[oi].marked = 0;
            js_objects[oi].data = data;
        }
        return ((uint64_t)oi + 1) * 2;
    }
    case 4: { /* COREGC_FIELD_READ(base, layout, field) -> word */
        uint32_t lay = (uint32_t)args[1], fld = (uint32_t)args[2];
        uint64_t off; uint32_t n;
        if (lay == 0) { off = (uint64_t)fld * 4; n = 4; }
        else { const JsLayout *li = &JS_LAYOUTS[lay]; off = li->field_off[fld]; n = li->field_size[fld]; }
        return js_read(js_obj(args[0])->data, off, n);
    }
    case 5: { /* COREGC_FIELD_WRITE(base, layout, field, value) */
        uint32_t lay = (uint32_t)args[1], fld = (uint32_t)args[2];
        uint64_t off; uint32_t n;
        if (lay == 0) { off = (uint64_t)fld * 4; n = 4; }
        else { const JsLayout *li = &JS_LAYOUTS[lay]; off = li->field_off[fld]; n = li->field_size[fld]; }
        js_write(js_obj(args[0])->data, off, args[3], n);
        return 0;
    }
    case 6: { /* COREGC_INDEX_READ(base, layout, index) -> word */
        const JsLayout *li = &JS_LAYOUTS[(uint32_t)args[1]];
        uint64_t off = li->data_off + args[2] * li->elem_size;
        return js_read(js_obj(args[0])->data, off, li->elem_size);
    }
    case 7: { /* COREGC_INDEX_WRITE(base, layout, index, value) */
        const JsLayout *li = &JS_LAYOUTS[(uint32_t)args[1]];
        uint64_t off = li->data_off + args[2] * li->elem_size;
        js_write(js_obj(args[0])->data, off, args[3], li->elem_size);
        return 0;
    }
    case 8: { /* COREGC_ROOT_ENTER(slots) */
        uint64_t n = args[0];
        if (js_n_frames == js_cap_frames) {
            js_cap_frames = js_cap_frames ? js_cap_frames * 2 : 64;
            js_roots = realloc(js_roots, js_cap_frames * sizeof(uint64_t *));
            js_root_counts = realloc(js_root_counts, js_cap_frames * sizeof(uint64_t));
        }
        if (js_root_buf_len + n > js_root_buf_cap) {
            uint64_t nc = js_root_buf_cap ? js_root_buf_cap : 1024;
            while (nc < js_root_buf_len + n) nc *= 2;
            js_root_bufs = realloc(js_root_bufs, nc * 8);
            js_root_buf_cap = nc;
        }
        uint64_t *frame = js_root_bufs + js_root_buf_len;
        memset(frame, 0, n * 8);
        js_roots[js_n_frames] = frame;
        js_root_counts[js_n_frames] = n;
        js_n_frames++;
        js_root_buf_len += n;
        return 0;
    }
    case 9: { /* COREGC_ROOT_STORE(slot, value) */
        uint64_t *frame = js_roots[js_n_frames - 1];
        frame[args[0]] = args[1];
        return 0;
    }
    case 10: { /* COREGC_ROOT_POP */
        js_root_buf_len -= js_root_counts[js_n_frames - 1];
        js_n_frames--;
        return 0;
    }
    case 11: { /* COREGC_ARRAY_COPY(dst, di, src, si, len, elem_log2) */
        JsObj *d = js_obj(args[0]);
        const JsLayout *li = &JS_LAYOUTS[JS_TYPE_TO_LAYOUT[d->type_id]];
        uint64_t esz = li->elem_size;
        uint64_t d_off = li->data_off + args[1] * esz;
        uint64_t s_off = li->data_off + args[3] * esz;
        uint64_t nbytes = args[4] * esz;
        uint8_t *tmp = malloc(nbytes ? nbytes : 1);
        memcpy(tmp, js_obj(args[2])->data + s_off, nbytes);
        memcpy(d->data + d_off, tmp, nbytes);
        free(tmp);
        return 0;
    }
    case 12: /* GLOBAL_GET(index) -> word */
        return JS_GLOBALS[args[0]];
    case 13: /* GLOBAL_SET(index, value) */
        JS_GLOBALS[args[0]] = args[1];
        return 0;
    default: /* FFI_CALL and anything else: no host imports here — fail closed */
        fprintf(stderr, "jsaw C host: unhandled host op %u\n", idx);
        abort();
    }
}

/* ── ext-op driver (mirrors MiniInterp::run's exit handling) ────────────── */
static void js_run(uint8_t *prog, size_t n, uint32_t entry) {
    uint8_t *pc = prog + entry;
    for (;;) {
        MnResult r = mn_interpret(js_store, NULL, pc, &js_hi_table);
        pc = r.pc;
        if (pc >= prog + n) {
            if (mn_call_sp > 0) { pc = prog + mn_call_stack[--mn_call_sp]; continue; }
            return;
        }
        uint8_t tag = *pc;
        if (tag == 0x80) { /* Jump */
            uint32_t tgt; memcpy(&tgt, pc + 1, 4);
            pc = prog + tgt;
        } else if (tag == 0x81) { /* Cond (raw 0/1 condition words) */
            uint8_t creg = pc[1];
            uint32_t tgt; memcpy(&tgt, pc + 2, 4);
            pc = mn_g4(js_store, creg) ? prog + tgt : pc + 6;
        } else if (tag == 0x82) { /* Table (f64 discriminant) */
            uint8_t ireg = pc[1];
            uint8_t n_arms = pc[2];
            uint64_t ibits = mn_g4(js_store, ireg);
            double dv; memcpy(&dv, &ibits, 8);
            uint32_t iv = (uint32_t)dv;
            uint8_t *p = pc + 3;
            uint32_t tgt = 0; int matched = 0;
            for (uint32_t k = 0; k < n_arms; k++) {
                uint32_t key, off;
                memcpy(&key, p, 4); memcpy(&off, p + 4, 4); p += 8;
                if (!matched && iv == key) { tgt = off; matched = 1; }
            }
            if (!matched) memcpy(&tgt, p, 4);
            pc = prog + tgt;
        } else if (tag == 0x83) { /* TailCall */
            uint8_t freg = pc[1];
            uint64_t b = mn_g4(js_store, freg);
            double off; memcpy(&off, &b, 8);
            pc = prog + (uint32_t)off;
        } else if (tag == 0x84) { /* Call */
            uint8_t freg = pc[1];
            uint64_t b = mn_g4(js_store, freg);
            double off; memcpy(&off, &b, 8);
            mn_call_stack[mn_call_sp++] = (uint64_t)((pc + 2) - prog);
            pc = prog + (uint32_t)off;
        } else if (tag == 0x9B) { /* jsaw host op */
            uint8_t hidx = pc[1], has_out = pc[2], nargs = pc[3];
            uint64_t args[16];
            for (uint32_t i = 0; i < nargs; i++) args[i] = mn_g4(js_store, pc[4 + i]);
            uint64_t res = js_host(hidx, args);
            if (has_out) mn_s4(js_store, pc[4 + nargs], res);
            pc += 5 + nargs;
        } else if (tag == 0x85) { /* Throw — corpus programs never throw uncaught */
            fprintf(stderr, "jsaw C host: uncaught throw\n");
            exit(2);
        } else if (mn_call_sp > 0) {
            pc = prog + mn_call_stack[--mn_call_sp];
        } else {
            return;
        }
    }
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s artifact.bin [hex-arg ...]\n", argv[0]); return 2; }
    FILE *f = fopen(argv[1], "rb");
    if (!f) { perror("artifact"); return 2; }
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *prog = malloc(n);
    if (fread(prog, 1, n, f) != (size_t)n) { perror("read"); return 2; }
    fclose(f);
    /* args occupy window runs 0..argc-2 (R4 words at bases 0,4,8,...). */
    for (int i = 2; i < argc; i++) {
        uint64_t bits = strtoull(argv[i], NULL, 16);
        mn_s4(js_store, (uint32_t)(i - 2) * 4, bits);
    }
    if (JS_START_PC != 0xFFFFFFFFu) js_run(prog, (size_t)n, JS_START_PC);
    js_run(prog, (size_t)n, JS_ENTRY_PC);
    printf("%016llx\n", (unsigned long long)mn_g4(js_store, 0));
    return 0;
}
"#;

/// Per-case C metadata: layout tables (by layout index), the type_id →
/// layout index map, globals init, store size, and the entry/start pcs.
pub fn c_meta_source(m: &LeModule, artifact: &MinibcArtifact, export: &str) -> String {
    let mut out = String::new();
    out.push_str("/* generated per-case jsaw MiniBC metadata */\n");
    out.push_str("typedef struct {\n");
    out.push_str("    uint32_t size, elem_size, elem_is_ref, data_off;\n");
    out.push_str("    uint32_t n_fields; const uint32_t *field_off; const uint32_t *field_size;\n");
    out.push_str("    uint32_t n_refs; const uint32_t *ref_off;\n");
    out.push_str("} JsLayout;\n");

    // Compute the same layout math as the Rust host (natural alignment).
    let type_size = |t: super::LeType| -> u32 {
        match t {
            super::LeType::I8 => 1,
            super::LeType::I16 => 2,
            super::LeType::I32 | super::LeType::F32 => 4,
            super::LeType::I64 | super::LeType::F64 | super::LeType::Ref | super::LeType::Slot => 8,
        }
    };
    const HEADER: u32 = 16;
    let mut max_type_id = 0u32;
    for l in m.layouts.iter().skip(1) {
        max_type_id = max_type_id.max(l.type_id);
    }
    let mut type_to_layout = vec![-1i64; max_type_id as usize + 1];

    let mut layout_inits: Vec<String> = Vec::new();
    for (idx, l) in m.layouts.iter().enumerate() {
        if idx == 0 {
            layout_inits.push("{0,0,0,0,0,0,0,0,0}".into());
            continue;
        }
        if (l.type_id as usize) < type_to_layout.len() {
            type_to_layout[l.type_id as usize] = idx as i64;
        }
        match &l.array_element {
            Some(elem) => {
                let esz = type_size(*elem);
                let off = HEADER.div_ceil(esz) * esz;
                layout_inits.push(format!(
                    "{{0,{esz},{is_ref},{off},0,0,0,0,0}}",
                    is_ref = if *elem == super::LeType::Ref { 1 } else { 0 },
                ));
            }
            None => {
                let mut off = HEADER;
                let mut foffs = Vec::new();
                let mut fsizes = Vec::new();
                let mut roffs = Vec::new();
                for f in &l.fields {
                    let sz = type_size(*f);
                    off = off.div_ceil(sz) * sz;
                    if *f == super::LeType::Ref {
                        roffs.push(off);
                    }
                    foffs.push(off);
                    fsizes.push(sz);
                    off += sz;
                }
                let size = off.max(HEADER);
                let fo_name = format!("js_fo_{idx}");
                let fs_name = format!("js_fs_{idx}");
                let ro_name = format!("js_ro_{idx}");
                let _ = writeln!(
                    out,
                    "static const uint32_t {fo_name}[] = {{{}}};",
                    foffs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
                );
                let _ = writeln!(
                    out,
                    "static const uint32_t {fs_name}[] = {{{}}};",
                    fsizes.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
                );
                let _ = writeln!(
                    out,
                    "static const uint32_t {ro_name}[] = {{{}}};",
                    roffs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
                );
                layout_inits.push(format!(
                    "{{{size},0,0,0,{nf},{fo_name},{fs_name},{nr},{ro_name}}}",
                    nf = foffs.len(),
                    nr = roffs.len(),
                ));
            }
        }
    }
    let _ = writeln!(
        out,
        "static const JsLayout JS_LAYOUTS[] = {{{}}};",
        layout_inits.join(",")
    );
    let _ = writeln!(
        out,
        "static const int32_t JS_TYPE_TO_LAYOUT[] = {{{}}};",
        type_to_layout.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
    );
    let _ = writeln!(out, "#define JS_N_TYPES {}", type_to_layout.len());

    // Globals (u64 init words).
    let _ = writeln!(
        out,
        "static uint64_t JS_GLOBALS[] = {{{}}};",
        m.globals
            .iter()
            .map(|(_, init)| format!("UINT64_C(0x{:016x})", init.unwrap_or(0)))
            .collect::<Vec<_>>()
            .join(",")
    );

    let entry = artifact.exports[export];
    let start = artifact.start.unwrap_or(u32::MAX);
    let _ = writeln!(out, "#define JS_ENTRY_PC {entry}u");
    let _ = writeln!(out, "#define JS_START_PC {start}u");
    let _ = writeln!(out, "#define JS_STORE_REGS {}", artifact.store_regs.max(64));
    // The interpreter state the harness's driver loop names.
    let _ = writeln!(
        out,
        "static uint16_t js_store[JS_STORE_REGS];\nstatic MnHiTable js_hi_table = {{ (void *)0, 0, 0 }};"
    );
    out
}
