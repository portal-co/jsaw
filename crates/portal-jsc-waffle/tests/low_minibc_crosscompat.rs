//! Bytecode-level cross-compatibility (DreamComp #39 acceptance #4): a
//! jsaw-emitted MiniBC artifact and a DreamComp-emitted MiniBC program both
//! execute in **one** `MiniInterp` instance without format errors.
//!
//! The DreamComp program is built through the same `dreamcompiler-minibc`
//! encoder DreamComp's own compiler emits with (stock R4 ops,
//! `StdExtOp::Jump`/`Call` segment exits, f64-valued slots), and is
//! additionally round-tripped through DreamComp's artifact `Container`
//! format. Semantic (NaN-box) interop between the two value models stays
//! out of scope (issue #16); this test proves format-level coexistence.

use std::collections::BTreeMap;

use dreamcompiler_minibc::ext_op::EncodeExtOp;
use dreamcompiler_minibc::interp::{MiniInterp, StdExtOp};
use dreamcompiler_minibc::{Encode, MiniOp, Reg, RegForm};

use portal_jsc_waffle::low_emit;

fn jsaw_artifact() -> (low_emit::LeModule, low_emit::minibc_render::MinibcArtifact) {
    let mut sources = BTreeMap::new();
    sources.insert(
        "main.js".to_string(),
        "export function run(x) { return x + 40; }".to_string(),
    );
    let set = portal_jsc_waffle::module_set_from_sources(
        sources.iter().map(|(k, v)| (k.as_str(), v.as_str())),
    )
    .expect("ingestion");
    let mut module = portal_pc_waffle::Module::empty();
    portal_jsc_waffle::convert_modules(
        "main.js",
        &set,
        &mut module,
        &portal_jsc_waffle::ConvertOptions::default(),
    )
    .expect("lowering");
    let plan = portal_jsc_waffle::BackendPlan::from_module(&module).expect("plan");
    low_emit::emit_minibc(&plan, &low_emit::NanoRepresentation).expect("emit minibc")
}

/// A DreamComp-flavored program: `run(entry)` computes `reg0 + 2.0` where
/// reg0 arrives in the R4 call window, using a direct call to a leaf
/// function that does the f64 add — exactly the op/ext-op surface
/// DreamComp's own MiniBC backend emits.
fn dreamcomp_program(base: u32) -> Vec<u8> {
    let mut b = Vec::new();
    // Layout: entry (load leaf pc; Call) / cont (Jump to terminator) / leaf
    // (reg0 += 2.0; Jump to terminator) / terminator 0xFF. All absolute pcs
    // are `base`-relative (the container payload starts at `base`).
    MiniOp::LoadConst64(Reg(4), [0; 8]).encode(0, &mut |byte| b.push(byte));
    let leaf_patch_at = b.len() - 8;
    b.push(0xFF);
    StdExtOp::encode_ext_op(&StdExtOp::Call { func_reg: 4 }, 0, &mut |byte| b.push(byte));
    // cont: the Call's return address lands here.
    b.push(0xFF);
    StdExtOp::encode_ext_op(&StdExtOp::Jump { target: 0 }, 0, &mut |byte| b.push(byte));
    let cont_patch_at = b.len() - 4;
    // leaf
    let leaf = base + b.len() as u32;
    MiniOp::LoadConst64(Reg(4), 2.0f64.to_bits().to_le_bytes()).encode(0, &mut |byte| b.push(byte));
    MiniOp::FAdd(Reg(0), Reg(4), Reg(0)).encode(0, &mut |byte| b.push(byte));
    b.push(0xFF);
    StdExtOp::encode_ext_op(&StdExtOp::Jump { target: 0 }, 0, &mut |byte| b.push(byte));
    let leaf_jump_patch_at = b.len() - 4;
    // terminator
    let term = base + b.len() as u32;
    b.push(0xFF);
    // patch: cont Jump -> term, leaf Jump -> term, entry const -> leaf (f64)
    b[cont_patch_at..cont_patch_at + 4].copy_from_slice(&term.to_le_bytes());
    b[leaf_jump_patch_at..leaf_jump_patch_at + 4].copy_from_slice(&term.to_le_bytes());
    let leaf_bits = (leaf as f64).to_bits().to_le_bytes();
    b[leaf_patch_at..leaf_patch_at + 8].copy_from_slice(&leaf_bits);
    b
}

#[test]
fn minibc_crosscompat_one_interpreter() {
    let (module, artifact) = jsaw_artifact();

    // Round-trip the DreamComp program through DreamComp's artifact
    // container format (the encoder receives the payload's absolute base pc
    // and must encode deterministically for it).
    let container = dreamcompiler_minibc::container::Container::build(&[(1, 0x42)], 0, {
        move |_handle, base| dreamcomp_program(base)
    });
    let parsed =
        dreamcompiler_minibc::container::Container::parse(container.into_bytes()).expect("parse");
    let dc_flat = parsed.flatten().to_vec();
    let dc_off = parsed.entry_offset(1).expect("entry");

    // One interpreter instance for both artifacts.
    let mut interp = MiniInterp::new(artifact.store_regs.max(64));
    let host = low_emit::minibc_host::JsawHost::new(&artifact, &module);
    host.install(&mut interp);

    // 1. jsaw artifact: start + export run(2.0) == 42.0.
    if let Some(sp) = artifact.start {
        interp.run(&artifact.bytes, sp as usize).expect("jsaw start");
    }
    let entry = artifact.exports["run"];
    interp.set_r4(0, 2.0f64.to_bits());
    interp.run(&artifact.bytes, entry as usize).expect("jsaw run");
    assert_eq!(f64::from_bits(interp.get_r4(0)), 42.0, "jsaw artifact");

    // 2. DreamComp program in the same interpreter: reg0 = 40.0 → 42.0.
    interp.set_r4(0, 40.0f64.to_bits());
    interp
        .run(&dc_flat, dc_off as usize)
        .expect("dreamcomp program");
    assert_eq!(f64::from_bits(interp.get_r4(0)), 42.0, "dreamcomp program");
}
