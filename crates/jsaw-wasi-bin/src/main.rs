//! jsaw compiler as a wasm32-wasip1 binary.
//!
//! The host (Gradle plugin, test harness, CLI) invokes this with:
//!   jsaw-compiler.wasm --src /src --out /out
//! where `/src` and `/out` are WASI preopens (or real dirs natively).
//! The compilation manifest arrives as JSON on stdin; the result is a
//! single JSON line on stdout. All diagnostics go to stderr.

use std::io::Read;
use std::path::PathBuf;
use std::process::ExitCode;

use jsaw_wasi_bin::manifest;
use jsaw_wasi_bin::{compile_to_outputs, read_sources, write_outputs};

fn main() -> ExitCode {
    // Route panics to stderr so a compiler bug fails the task with a
    // message instead of an opaque trap.
    std::panic::set_hook(Box::new(|info| {
        eprintln!("jsaw-compiler panicked: {info}");
    }));

    let mut src: Option<PathBuf> = None;
    let mut out: Option<PathBuf> = None;
    let mut args = std::env::args_os().skip(1);
    while let Some(arg) = args.next() {
        match arg.to_str() {
            Some("--src") => src = args.next().map(PathBuf::from),
            Some("--out") => out = args.next().map(PathBuf::from),
            other => {
                eprintln!("jsaw-compiler: unexpected argument {other:?}");
                return ExitCode::from(2);
            }
        }
    }
    let (Some(src), Some(out)) = (src, out) else {
        eprintln!("usage: jsaw-compiler --src <dir> --out <dir>  (manifest JSON on stdin)");
        return ExitCode::from(2);
    };

    let result = run(&src, &out);
    // The result is a single JSON line on stdout, always.
    match serde_json::to_string(&result) {
        Ok(line) => println!("{line}"),
        Err(error) => {
            println!("{{\"status\":\"error\",\"error\":\"result serialization failed: {error}\"}}")
        }
    }
    match &result {
        manifest::Result::Ok { .. } => ExitCode::SUCCESS,
        manifest::Result::Error { .. } => ExitCode::FAILURE,
    }
}

fn run(src: &std::path::Path, out: &std::path::Path) -> manifest::Result {
    match try_run(src, out) {
        Ok((exports, outputs)) => manifest::Result::ok(exports, outputs),
        Err(error) => manifest::Result::error(format!("{error:#}")),
    }
}

fn try_run(
    src: &std::path::Path,
    out: &std::path::Path,
) -> anyhow::Result<(Vec<String>, Vec<String>)> {
    let mut stdin = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin)
        .map_err(|e| anyhow::anyhow!("could not read the manifest from stdin: {e}"))?;
    let manifest: manifest::Manifest = serde_json::from_str(&stdin)
        .map_err(|e| anyhow::anyhow!("manifest is not valid JSON: {e}"))?;

    let sources = read_sources(src, &manifest)?;
    let (exports, outputs) = compile_to_outputs(&manifest, &sources)?;
    write_outputs(out, &outputs)?;
    let written: Vec<String> = outputs.into_keys().collect();
    Ok((exports, written))
}
