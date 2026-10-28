//! Compile-and-execute helpers for the low-level backends (DreamComp #41).
//!
//! Shared by the crate-local runners (`tests/low_c_runner.rs`,
//! `tests/low_rust_runner.rs`) and the extended corpus parity runner in
//! `jsaw-wasi-bin`. Each case compiles in its own artifact (per-case
//! isolation replaces batch bisection: see the issue comment for why
//! single-TU batching is infeasible at corpus artifact sizes).

use std::path::Path;

use crate::backend_plan::BackendPlan;
use crate::low_emit;

/// A low-level target lane.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LowLane {
    CNano,
    CCoregc,
    RustNano,
    RustCoregc,
    MinibcNano,
    MinibcCoregc,
}

impl LowLane {
    pub const ALL: [LowLane; 6] = [
        LowLane::CNano,
        LowLane::CCoregc,
        LowLane::RustNano,
        LowLane::RustCoregc,
        LowLane::MinibcNano,
        LowLane::MinibcCoregc,
    ];

    pub fn name(self) -> &'static str {
        match self {
            LowLane::CNano => "c_nano",
            LowLane::CCoregc => "c_coregc",
            LowLane::RustNano => "rust_nano",
            LowLane::RustCoregc => "rust_coregc",
            LowLane::MinibcNano => "minibc_nano",
            LowLane::MinibcCoregc => "minibc_coregc",
        }
    }

    /// The toolchain binary this lane needs (fail-closed when absent). The
    /// MiniBC lanes run in-process under the stock interpreter — no
    /// external toolchain.
    pub fn toolchain(self) -> &'static str {
        match self {
            LowLane::CNano | LowLane::CCoregc => "cc",
            LowLane::RustNano | LowLane::RustCoregc => "rustc",
            LowLane::MinibcNano | LowLane::MinibcCoregc => "minibc-interp",
        }
    }

    /// Whether the toolchain is invocable.
    pub fn toolchain_available(self) -> bool {
        match self {
            LowLane::MinibcNano | LowLane::MinibcCoregc => true,
            _ => std::process::Command::new(self.toolchain())
                .arg("--version")
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false),
        }
    }

    /// Whether this lane runs MiniBC bytecode in-process.
    pub fn is_minibc(self) -> bool {
        matches!(self, LowLane::MinibcNano | LowLane::MinibcCoregc)
    }
}

/// Outcome of one case-lane execution attempt.
#[derive(Clone, Debug)]
pub struct LowOutcome {
    /// "pass" | "fail" | "unsupported" | "unavailable".
    pub status: &'static str,
    /// The f64 result bits when the artifact ran.
    pub value_bits: Option<u64>,
    /// Emitted artifact size in bytes (0 when emission failed).
    pub artifact_bytes: usize,
    pub detail: String,
}

/// Emit the artifact source for one lane (byte-identical across calls).
pub fn emit_low_artifact(plan: &BackendPlan, lane: LowLane) -> Result<String, String> {
    let result = match lane {
        LowLane::CNano => low_emit::emit_c(plan, &low_emit::NanoRepresentation),
        LowLane::CCoregc => low_emit::emit_c(plan, &low_emit::CoreGcRepresentation),
        LowLane::RustNano => low_emit::emit_rust(plan, &low_emit::NanoRepresentation),
        LowLane::RustCoregc => low_emit::emit_rust(plan, &low_emit::CoreGcRepresentation),
        LowLane::MinibcNano | LowLane::MinibcCoregc => {
            unreachable!("minibc lanes emit bytecode, not source")
        }
    };
    result.map_err(|ds| {
        ds.iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    })
}

/// Compile and run one case through a low-level lane. `args` are f64 bit
/// patterns; the export must be in the numeric f64 ABI (single f64 result).
///
/// Toolchain-missing is reported as `unavailable` (the caller decides how to
/// surface it); emission failures, compile failures, traps, and wrong
/// results are `fail` with the diagnostic in `detail`.
pub fn run_low_case(
    plan: &BackendPlan,
    lane: LowLane,
    export: &str,
    args: &[f64],
    expected: f64,
    dir: &Path,
) -> LowOutcome {
    if lane.is_minibc() {
        return run_minibc_case(plan, lane, export, args, expected, dir);
    }
    if !lane.toolchain_available() {
        return LowOutcome {
            status: "unavailable",
            value_bits: None,
            artifact_bytes: 0,
            detail: format!("toolchain {:?} not invocable", lane.toolchain()),
        };
    }
    let src = match emit_low_artifact(plan, lane) {
        Ok(s) => s,
        Err(detail) => {
            return LowOutcome {
                status: "fail",
                value_bits: None,
                artifact_bytes: 0,
                detail: format!("emit: {detail}"),
            };
        }
    };
    let artifact_bytes = src.len();
    let (source_file, main_file) = match lane {
        LowLane::CNano | LowLane::CCoregc => ("m.c", "main.c"),
        LowLane::RustNano | LowLane::RustCoregc => ("artifact.rs", "main.rs"),
        LowLane::MinibcNano | LowLane::MinibcCoregc => unreachable!(),
    };
    std::fs::write(dir.join(source_file), &src).expect("write artifact");
    std::fs::write(dir.join(main_file), main_file_content(lane, export, args)).expect("write main");
    let compile = match lane {
        LowLane::MinibcNano | LowLane::MinibcCoregc => unreachable!(),
        LowLane::CNano | LowLane::CCoregc => std::process::Command::new(lane.toolchain())
            .args(["-std=c99", "-O0", "-w", "-o"])
            .arg(dir.join("a.out"))
            .arg(dir.join(source_file))
            .arg(dir.join(main_file))
            .arg("-lm")
            .output()
            .expect("cc spawn"),
        LowLane::RustNano | LowLane::RustCoregc => std::process::Command::new(lane.toolchain())
            .args(["--edition", "2021", "-C", "opt-level=0", "-o"])
            .arg(dir.join("a.out"))
            .arg(dir.join(main_file))
            .output()
            .expect("rustc spawn"),
    };
    if !compile.status.success() {
        return LowOutcome {
            status: "fail",
            value_bits: None,
            artifact_bytes,
            detail: format!(
                "compile: {}",
                String::from_utf8_lossy(&compile.stderr)
                    .lines()
                    .filter(|l| l.contains("error"))
                    .take(3)
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
        };
    }
    let run = std::process::Command::new(dir.join("a.out")).output().expect("run");
    if !run.status.success() {
        return LowOutcome {
            status: "fail",
            value_bits: None,
            artifact_bytes,
            detail: format!("artifact exited with {} (trap or crash)", run.status),
        };
    }
    let text = String::from_utf8_lossy(&run.stdout).trim().to_string();
    let Ok(bits) = u64::from_str_radix(&text, 16) else {
        return LowOutcome {
            status: "fail",
            value_bits: None,
            artifact_bytes,
            detail: format!("unparseable output {text:?}"),
        };
    };
    let actual = f64::from_bits(bits);
    if actual == expected || (actual.is_nan() && expected.is_nan()) {
        LowOutcome {
            status: "pass",
            value_bits: Some(bits),
            artifact_bytes,
            detail: String::new(),
        }
    } else {
        LowOutcome {
            status: "fail",
            value_bits: Some(bits),
            artifact_bytes,
            detail: format!("expected {expected}, got {actual}"),
        }
    }
}

/// Run one case through a MiniBC lane: emit the bytecode artifact, then
/// execute `start` and the numeric export in one interpreter + host state
/// under the stock `MiniInterp` (DreamComp #39).
fn run_minibc_case(
    plan: &BackendPlan,
    lane: LowLane,
    export: &str,
    args: &[f64],
    expected: f64,
    dir: &Path,
) -> LowOutcome {
    use dreamcompiler_minibc::interp::MiniInterp;
    let emit = match lane {
        LowLane::MinibcNano => low_emit::emit_minibc(plan, &low_emit::NanoRepresentation),
        LowLane::MinibcCoregc => low_emit::emit_minibc(plan, &low_emit::CoreGcRepresentation),
        _ => unreachable!(),
    };
    let (module, artifact) = match emit {
        Ok(x) => x,
        Err(ds) => {
            return LowOutcome {
                status: "fail",
                value_bits: None,
                artifact_bytes: 0,
                detail: format!(
                    "emit: {}",
                    ds.iter()
                        .map(|d| d.to_string())
                        .collect::<Vec<_>>()
                        .join("; ")
                ),
            };
        }
    };
    let artifact_bytes = artifact.bytes.len();
    std::fs::write(dir.join("artifact.minibc"), &artifact.bytes).expect("write artifact");
    let Some(&entry) = artifact.exports.get(export) else {
        return LowOutcome {
            status: "fail",
            value_bits: None,
            artifact_bytes,
            detail: format!("no export {export:?}"),
        };
    };
    let mut interp = MiniInterp::new(artifact.store_regs.max(64));
    let host = low_emit::minibc_host::JsawHost::new(&artifact, &module);
    host.install(&mut interp);
    if let Some(sp) = artifact.start {
        if let Err(e) = interp.run(&artifact.bytes, sp as usize) {
            return LowOutcome {
                status: "fail",
                value_bits: None,
                artifact_bytes,
                detail: format!("start: {e:?}"),
            };
        }
    }
    for (i, a) in args.iter().enumerate() {
        interp.set_r4(i * 4, a.to_bits());
    }
    match interp.run(&artifact.bytes, entry as usize) {
        Ok(()) => {
            let bits = interp.get_r4(0);
            let actual = f64::from_bits(bits);
            if actual == expected || (actual.is_nan() && expected.is_nan()) {
                LowOutcome {
                    status: "pass",
                    value_bits: Some(bits),
                    artifact_bytes,
                    detail: String::new(),
                }
            } else {
                LowOutcome {
                    status: "fail",
                    value_bits: Some(bits),
                    artifact_bytes,
                    detail: format!("expected {expected}, got {actual}"),
                }
            }
        }
        Err(e) => LowOutcome {
            status: "fail",
            value_bits: None,
            artifact_bytes,
            detail: format!("run: {e:?}"),
        },
    }
}

fn main_file_content(lane: LowLane, export: &str, args: &[f64]) -> String {
    match lane {
        LowLane::CNano | LowLane::CCoregc => {
            let sym = format!("js_export_{}", low_emit::c_render::c_ident(export));
            let exprs: Vec<String> = args
                .iter()
                .map(|a| format!("db(UINT64_C(0x{:016x}))", a.to_bits()))
                .collect();
            format!(
                "#include <stdint.h>\n#include <stdio.h>\n#include <string.h>\n\
                 extern void js_start(void);\nextern double {sym}({params});\n\
                 static double db(uint64_t b){{double d;memcpy(&d,&b,8);return d;}}\n\
                 int main(void){{ js_start(); double r = {sym}({args}); \
                 uint64_t b; memcpy(&b,&r,8); printf(\"%016llx\\n\",(unsigned long long)b); return 0; }}\n",
                params = vec!["double"; args.len()].join(", "),
                args = exprs.join(", "),
            )
        }
        LowLane::MinibcNano | LowLane::MinibcCoregc => unreachable!(),
        LowLane::RustNano | LowLane::RustCoregc => {
            let sym = format!("js_export_{}", low_emit::rust_render::rust_ident(export));
            let exprs: Vec<String> = args
                .iter()
                .map(|a| format!("f64::from_bits(0x{:016x}u64)", a.to_bits()))
                .collect();
            format!(
                "mod artifact;\nfn main() {{\n  artifact::js_start();\n  let r = artifact::{sym}({args});\n  println!(\"{{:016x}}\", r.to_bits());\n}}\n",
                args = exprs.join(", "),
            )
        }
    }
}
