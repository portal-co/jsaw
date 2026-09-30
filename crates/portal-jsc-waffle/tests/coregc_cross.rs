//! Cross-testing harness (docs/plan-coregc-atomic-collector-and-lowering.md
//! §14): the same `conv.rs`-compiled program runs under the native WasmGC
//! backend (wasmtime with GC enabled) and under `emit_coregc` (default core
//! features, at both a forced collection threshold and the normal one),
//! comparing observable results and trap behavior.
//!
//! wasmtime-only by design: the e2e suite's Node/JVM/Swift runs are the
//! native backend's own parity gate and cost ~135 s per fixture here; the
//! coregc artifact is pure core Wasm, so wasmtime hosts both paths.

use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::module::SModule;
use portal_jsc_swc_tac::module::TModule;
use portal_pc_waffle::Module;
use swc_common::{FileName, GLOBALS, Globals, SourceMap, sync::Lrc};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_module};
use wasmtime::{Config, Engine, Instance, Module as WasmtimeModule, Store, Val};

/// Sanity check: every value used by a source function body must be
/// defined in it (block param or operator result), and every operator's
/// args must be defined at-or-before the using instruction's position.
#[allow(dead_code)]
fn check_ssa_sanity(module: &Module<'_>) {
    use portal_pc_waffle::FuncDecl;
    for (f, decl) in module.funcs.entries() {
        let FuncDecl::Body(_, name, body) = decl else {
            continue;
        };
        // Mirror coregc_lower::translation_order: DFS postorder over succs
        // from entry, reversed, unreachable appended. A value used by an
        // instruction must already be defined (same block earlier, or any
        // dominating block) by the time its block is translated.
        let mut visited = std::collections::HashSet::new();
        let mut postorder = Vec::new();
        let mut stack = vec![(body.entry, 0usize)];
        visited.insert(body.entry);
        while let Some(&mut (block, ref mut index)) = stack.last_mut() {
            let succs = &body.blocks[block].succs;
            if *index < succs.len() {
                let next = succs[*index];
                *index += 1;
                if visited.insert(next) {
                    stack.push((next, 0));
                }
            } else {
                postorder.push(block);
                stack.pop();
            }
        }
        postorder.reverse();
        for (b, _) in body.blocks.entries() {
            if !visited.contains(&b) {
                postorder.push(b);
            }
        }
        let mut defined = std::collections::HashSet::new();
        for block in postorder {
            for &(_, p) in &body.blocks[block].params {
                defined.insert(p);
            }
            for inst in &body.blocks[block].insts {
                let v = inst.value;
                body.values[v].visit_uses(&body.arg_pool, |used| {
                    let used = body.resolve_alias(used);
                    if !defined.contains(&used) {
                        eprintln!(
                            "function {name:?} ({f:?}): {used:?} used by {v:?} in block {block:?} is not defined by translation time"
                        );
                    }
                });
                defined.insert(v);
            }
        }
    }
}

fn compile_module(source: &str) -> Module<'static> {
    GLOBALS.set(&Globals::default(), || {
        let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
        let file = cm.new_source_file(
            Lrc::new(FileName::Custom("fixture.mjs".into())),
            source.to_owned(),
        );
        let mut errors = vec![];
        let source = parse_file_as_module(
            &file,
            Syntax::Es(EsSyntax::default()),
            EsVersion::Es2022,
            None,
            &mut errors,
        )
        .expect("module fixture should parse");
        assert!(errors.is_empty(), "parser diagnostics: {errors:?}");
        let cfg = CfgModule::try_from(source).expect("CFG lowering should succeed");
        let tac = TModule::try_from(cfg).expect("TAC lowering should succeed");
        let ssa = SModule::try_from(tac).expect("SSA lowering should succeed");
        let mut wasm = Module::empty();
        portal_jsc_waffle::convert_module(
            &ssa,
            &mut wasm,
            &portal_jsc_waffle::ConvertOptions::default(),
        )
        .expect("WasmGC module lowering should succeed");
        wasm
    })
}

/// Outcome of one execution path: either a numeric result or a trap.
/// `PartialEq` is hand-written (not derived) because `f64`'s `==` makes
/// `NaN != NaN`, which would make this harness spuriously fail any fixture
/// whose *correct* result is `NaN` on both sides (e.g. JS `undefined + 1`);
/// bitwise identity is the right notion of "the same f64 result" here.
#[derive(Clone, Debug)]
enum Outcome {
    Value(f64),
    Trap,
}

impl PartialEq for Outcome {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Outcome::Value(a), Outcome::Value(b)) => a.to_bits() == b.to_bits(),
            (Outcome::Trap, Outcome::Trap) => true,
            _ => false,
        }
    }
}

fn execute(bytes: &[u8], name: &str, args: &[f64], gc: bool) -> Outcome {
    let mut config = Config::new();
    if gc {
        config.wasm_gc(true);
        config.wasm_function_references(true);
    }
    let engine = Engine::new(&config).expect("engine");
    let module = WasmtimeModule::new(&engine, bytes).expect("module compiles");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("module instantiates");
    let function = instance
        .get_func(&mut store, name)
        .unwrap_or_else(|| panic!("missing export {name:?}"));
    let inputs: Vec<Val> = args
        .iter()
        .copied()
        .map(|value| Val::F64(value.to_bits()))
        .collect();
    let mut outputs = [Val::F64(0)];
    match function.call(&mut store, &inputs, &mut outputs) {
        Ok(()) => match outputs[0] {
            Val::F64(bits) => Outcome::Value(f64::from_bits(bits)),
            ref value => panic!("export returned {value:?}, expected f64"),
        },
        Err(error) => {
            if std::env::var("COREGC_CROSS_VERBOSE").is_ok() {
                eprintln!("trap in {name:?} (gc={gc}): {error:#}");
                if let Some(trap_code) = instance.get_global(&mut store, "__coregc_trap_code") {
                    if let Val::I32(code) = trap_code.get(&mut store) {
                        eprintln!("coregc trap_code = {code}");
                    }
                }
                let mut debug_pair = (0i32, 0i32);
                for (slot, global) in [(0, "__coregc_debug_addr"), (1, "__coregc_debug_type")] {
                    if let Some(g) = instance.get_global(&mut store, global) {
                        if let Val::I32(v) = g.get(&mut store) {
                            eprintln!("coregc {global} = {v:#x}");
                            if slot == 0 {
                                debug_pair.0 = v;
                            } else {
                                debug_pair.1 = v;
                            }
                        }
                    }
                }
                if let Some(g) = instance.get_global(&mut store, "__coregc_root_bump") {
                    if let Val::I32(v) = g.get(&mut store) {
                        eprintln!(
                            "coregc root_bump = {v:#x} (worklist starts at 0x100200, heap at 0x200000)"
                        );
                    }
                }
                // Walk the full block list and print every allocation's
                // header, so which block got freed and which reused it is
                // visible around the offending address.
                if let (Some(blh), Some(memory)) = (
                    instance.get_global(&mut store, "__coregc_block_list_head"),
                    instance.get_memory(&mut store, "memory"),
                ) {
                    if let Val::I32(mut cur) = blh.get(&mut store) {
                        let mut seen = 0;
                        while cur != 0 && seen < 2000 {
                            let mut header = [0u8; 24];
                            if memory
                                .read(&store, (cur - 24) as usize, &mut header)
                                .is_err()
                            {
                                break;
                            }
                            let flags = u32::from_le_bytes(header[0..4].try_into().unwrap());
                            let ty = u32::from_le_bytes(header[4..8].try_into().unwrap());
                            let bytes = u32::from_le_bytes(header[8..12].try_into().unwrap());
                            let next = u32::from_le_bytes(header[12..16].try_into().unwrap());
                            let mark = if flags & 0x4 != 0 { "FREE" } else { "live" };
                            if (debug_pair.0 as u32).abs_diff(cur as u32) < 0x1000 || seen < 8 {
                                eprintln!("block {cur:#x}: {mark} type={ty:#x} bytes={bytes}");
                            }
                            cur = next as i32;
                            seen += 1;
                        }
                        eprintln!("(walked {seen} blocks)");
                    }
                }
                // Dump the offending pair's allocation header (flags,
                // type_id, payload_bytes) so the actual-vs-expected
                // mismatch is visible.
                if let Some(memory) = instance.get_memory(&mut store, "memory") {
                    let addr = debug_pair.0;
                    if addr >= 24 {
                        let mut header = [0u8; 24];
                        if memory
                            .read(&store, (addr - 24) as usize, &mut header)
                            .is_ok()
                        {
                            let flags = u32::from_le_bytes(header[0..4].try_into().unwrap());
                            let ty = u32::from_le_bytes(header[4..8].try_into().unwrap());
                            let bytes = u32::from_le_bytes(header[8..12].try_into().unwrap());
                            eprintln!(
                                "offending header: flags={flags:#x} type_id={ty:#x} payload_bytes={bytes}"
                            );
                        }
                    }
                }
                // Dump the shadow-root stack: walk the frame chain and print
                // each frame's slots so the bad pair's neighbors are visible.
                if let (Some(root_head), Some(memory)) = (
                    instance.get_global(&mut store, "__coregc_root_head"),
                    instance.get_memory(&mut store, "memory"),
                ) {
                    if let Val::I32(mut frame) = root_head.get(&mut store) {
                        let mut depth = 0;
                        while frame != 0 && depth < 32 {
                            let mut header = [0u8; 8];
                            if memory.read(&store, frame as usize, &mut header).is_err() {
                                break;
                            }
                            let prev = u32::from_le_bytes(header[0..4].try_into().unwrap());
                            let slots = u32::from_le_bytes(header[4..8].try_into().unwrap());
                            eprintln!("frame {frame:#x}: prev={prev:#x} slots={slots}");
                            for i in 0..slots.min(24) {
                                let mut pair = [0u8; 8];
                                let at = frame as usize + 8 + (i as usize) * 8;
                                if memory.read(&store, at, &mut pair).is_err() {
                                    break;
                                }
                                let addr = u32::from_le_bytes(pair[0..4].try_into().unwrap());
                                let ty = u32::from_le_bytes(pair[4..8].try_into().unwrap());
                                eprintln!("  slot[{i}] = ({addr:#x}, {ty:#x})");
                            }
                            frame = prev as i32;
                            depth += 1;
                        }
                    }
                }
            }
            Outcome::Trap
        }
    }
}

/// Run `name(args)` against a coregc artifact built with `debug_trace:
/// true`, wiring the `coregc_debug.trace` import to an `eprintln!` of every
/// allocation (kind 1 = fresh bump, kind 2 = free-list reuse) and every
/// sweep reclamation (kind 3), each with `(address, type_id)`. Used only
/// for ad-hoc investigation, gated by `COREGC_CROSS_TRACE`.
fn execute_with_trace(bytes: &[u8], name: &str, args: &[f64], watch_addr: i32) -> Outcome {
    let engine = Engine::new(&Config::new()).expect("engine");
    let module = WasmtimeModule::new(&engine, bytes).expect("module compiles");
    let mut store = Store::new(&engine, ());
    let trace = wasmtime::Func::wrap(&mut store, move |kind: i32, addr: i32, type_id: i32| {
        let label = match kind {
            1 => "ALLOC(fresh)",
            2 => "ALLOC(reuse)",
            3 => "FREE(sweep)",
            4 => "ROOT_STORE",
            6 => "CHECKPOINT_COLLECT",
            _ => "??",
        };
        if kind == 6 || watch_addr == 0 || addr == watch_addr {
            eprintln!("trace: {label} addr={addr:#x} type_id={type_id:#x}");
        }
    });
    let instance =
        Instance::new(&mut store, &module, &[trace.into()]).expect("module instantiates");
    let function = instance
        .get_func(&mut store, name)
        .unwrap_or_else(|| panic!("missing export {name:?}"));
    let inputs: Vec<Val> = args
        .iter()
        .copied()
        .map(|value| Val::F64(value.to_bits()))
        .collect();
    let mut outputs = [Val::F64(0)];
    match function.call(&mut store, &inputs, &mut outputs) {
        Ok(()) => match outputs[0] {
            Val::F64(bits) => Outcome::Value(f64::from_bits(bits)),
            ref value => panic!("export returned {value:?}, expected f64"),
        },
        Err(error) => {
            eprintln!("trap in {name:?} (traced): {error:#}");
            Outcome::Trap
        }
    }
}

/// Print the first N type descriptors (offset table) of a module's
/// inventory, for comparing native vs coregc layout conventions.
#[allow(dead_code)]
fn dump_descriptor_layouts(module: &Module<'_>) {
    let inventory = portal_jsc_waffle::CoreGcInventory::build(module).expect("inventory");
    let descriptors =
        portal_jsc_waffle::CoreGcDescriptorTable::build(&inventory).expect("descriptors");
    for layout in &descriptors.layouts {
        eprintln!(
            "type id={} align={} fixed={:?} stride={:?} slots={:?}",
            layout.id.get(),
            layout.payload_alignment,
            layout.fixed_payload_bytes,
            layout.array_stride,
            layout
                .slots
                .iter()
                .map(|slot| (slot.offset, format!("{:?}", slot.storage)))
                .collect::<Vec<_>>()
        );
    }
}

/// Compile `source`, then run each named export under (a) native WasmGC and
/// (b/c) coregc at both collection thresholds, requiring all observable
/// outcomes to match. Keeping several exports in one artifact catches
/// cross-component effects without recompiling the module per call.
fn cross_test_cases(source: &str, cases: &[(&str, &[f64])]) {
    let module = compile_module(source);
    let native_bytes = portal_pc_waffle::to_wasm_bytes(&module).expect("native encodes");
    if std::env::var("COREGC_CROSS_LAYOUTS").is_ok() {
        dump_descriptor_layouts(&module);
    }
    if std::env::var("COREGC_CROSS_SSA").is_ok() {
        check_ssa_sanity(&module);
    }
    let artifact = portal_jsc_waffle::emit_coregc(
        &module,
        &portal_jsc_waffle::CoreGcOptions {
            collect_threshold_bytes: 0,
            export_runtime_debug: true,
            ..portal_jsc_waffle::CoreGcOptions::default()
        },
    )
    .expect("fixture must lower under coregc");
    let core_bytes_forced =
        portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("coregc encodes");
    let artifact_normal =
        portal_jsc_waffle::emit_coregc(&module, &portal_jsc_waffle::CoreGcOptions::default())
            .expect("normal-threshold lowering");
    let core_bytes_normal =
        portal_pc_waffle::to_wasm_bytes(&artifact_normal.module).expect("coregc encodes");

    if let Ok(watch) = std::env::var("COREGC_CROSS_TRACE") {
        let watch_hex = watch.trim_start_matches("0x");
        let watch_addr = if watch_hex.is_empty() {
            0
        } else {
            i64::from_str_radix(watch_hex, 16)
                .expect("COREGC_CROSS_TRACE must be a hex address or empty") as i32
        };
        let traced_artifact = portal_jsc_waffle::emit_coregc(
            &module,
            &portal_jsc_waffle::CoreGcOptions {
                collect_threshold_bytes: 0,
                export_runtime_debug: true,
                debug_trace: true,
                ..portal_jsc_waffle::CoreGcOptions::default()
            },
        )
        .expect("traced lowering");
        let traced_bytes =
            portal_pc_waffle::to_wasm_bytes(&traced_artifact.module).expect("coregc encodes");
        for &(name, args) in cases {
            execute_with_trace(&traced_bytes, name, args, watch_addr);
        }
    }
    for &(name, args) in cases {
        let native = execute(&native_bytes, name, args, true);
        let forced = execute(&core_bytes_forced, name, args, false);
        let normal = execute(&core_bytes_normal, name, args, false);
        assert_eq!(
            native, forced,
            "forced-threshold coregc diverges from native for {name:?}"
        );
        assert_eq!(
            native, normal,
            "normal-threshold coregc diverges from native for {name:?}"
        );
    }
}

/// Convenience wrapper for the common one-export fixture shape.
fn cross_test(source: &str, name: &str, args: &[f64]) {
    cross_test_cases(source, &[(name, args)]);
}

// ---------------------------------------------------------------------
// Fixtures, in increasing Repr-complexity order (§14.3).
// ---------------------------------------------------------------------

#[test]
fn numeric_arithmetic() {
    cross_test(
        "export function run(x) { return x * 2 + 1; }",
        "run",
        &[21.0],
    );
}

#[test]
fn comparisons_and_branches() {
    cross_test(
        "export function run(x) { return x > 3 ? 10 : 20; }",
        "run",
        &[4.0],
    );
    cross_test(
        "export function run(x) { return x > 3 ? 10 : 20; }",
        "run",
        &[2.0],
    );
}

#[test]
fn direct_function_calls() {
    cross_test(
        "function add(a, b) { return a + b; } export function run(x) { return add(x, 2); }",
        "run",
        &[40.0],
    );
}

#[test]
fn closures_with_captured_state() {
    cross_test(
        "let mul = (x) => (y) => x * y; export function run() { return mul(3)(14); }",
        "run",
        &[],
    );
}

#[test]
fn object_literals_and_property_reads() {
    cross_test(
        "export function run() { let o = { value: 2, add(x) { return this.value + x; } }; return o.add(40); }",
        "run",
        &[],
    );
}

#[test]
fn array_literals_and_indexing() {
    cross_test(
        "export function run() { let a = [10, 20, 12]; return a[0] + a[1] + a[2]; }",
        "run",
        &[],
    );
}

#[test]
fn bigint_arithmetic_and_comparison() {
    cross_test(
        "export function run() { return (5n + 9n) === 14n ? 1 : 0; }",
        "run",
        &[],
    );
}

#[test]
fn strings_and_length() {
    cross_test(
        "export function run() { let s = 'ab' + 'cd'; return s.length; }",
        "run",
        &[],
    );
}

/// Source-level `throw` becomes a core-Wasm trap in jsaw, not a Wasm
/// exception terminator; its non-throwing path is therefore already part of
/// the supported core surface and must retain parity.
#[test]
fn non_throwing_path_of_a_source_throw() {
    cross_test(
        "export function run(n) { if (n > 0) throw new Error('boom'); return 42; }",
        "run",
        &[0.0],
    );
}

/// Representative object paths from the native e2e corpus. They exercise
/// dynamic property names, shape transitions, ref.test/ref.cast, and the
/// shape-trie tail-call fallback in one module.
#[test]
fn object_mutation_shape_changes_and_dynamic_fields() {
    cross_test_cases(
        "
            export function mutate() {
                let object = {};
                object.count = 1;
                object.count = object.count + 2;
                object.extra = 4;
                return object.count + object.extra;
            }
            export function reshape() {
                let object = { slot: 1 };
                object.slot = {};
                object.slot.count = 4;
                object.slot.extra = 6;
                return object.slot.count + object.slot.extra;
            }
            export function shaped_dynamic() {
                let object = { left: 2, right: 3 };
                let key = 'left';
                object[key] = 5;
                object.extra = 7;
                return object.left * 100 + object.right * 10 + object.extra;
            }
        ",
        &[("mutate", &[]), ("reshape", &[]), ("shaped_dynamic", &[])],
    );
}

/// Array literals, the arguments object, and rest destructuring use the same
/// dynamic-array representation in jsaw's real compiler output.
#[test]
fn arrays_arguments_and_object_rest() {
    cross_test_cases(
        "
            export function array_literal() {
                let values = [3, 4, 5];
                return values[0] * 100 + values[1] * 10 + values[2] + values.length;
            }
            export function arguments_visible(first, second) {
                return arguments[0] * 100 + arguments[1] * 10 + arguments.length;
            }
            export function object_rest() {
                let source = { hidden: 4, kept: 2, other: 3 };
                let { hidden, ...rest } = source;
                rest.kept = 7;
                return source.kept * 100 + rest.kept * 10 + rest.other;
            }
        ",
        &[
            ("array_literal", &[]),
            ("arguments_visible", &[2.0, 3.0]),
            ("object_rest", &[]),
        ],
    );
}

/// Accessor dispatch crosses the function-reference bridge; accessor_tail
/// reaches ReturnCallRef and must keep its arguments rooted at its checkpoint.
#[test]
fn object_accessors_and_tail_dispatch() {
    cross_test_cases(
        "
            export function paired() {
                let object = {
                    value: 2,
                    get doubled() { return this.value * 2; },
                    set doubled(next) { this.value = next / 2; },
                };
                object.doubled = 10;
                let result = object.doubled;
                object.extra = 3;
                return result * 100 + object.value + object.extra;
            }
            export function accessor_call() {
                let object = {
                    factor: 3,
                    get method() {
                        return function(value) { return this.factor * value; };
                    },
                };
                return object.method(4);
            }
            export function accessor_tail(value) {
                let object = {
                    get invoke() {
                        return function(input) { return input + 1; };
                    },
                };
                return object.invoke(value);
            }
        ",
        &[
            ("paired", &[]),
            ("accessor_call", &[]),
            ("accessor_tail", &[4.0]),
        ],
    );
}

/// Computed writes and length changes exercise grow/copy paths and dynamic
/// array-to-object casts.
#[test]
fn computed_array_writes_growth_and_length_resize() {
    cross_test_cases(
        "
            export function grow() {
                let values = [3];
                let index = 3;
                values[index] = 7;
                return values.length * 100 + values[3];
            }
            export function resize() {
                let values = [3, 4, 5];
                values.length = 1;
                return values.length * 10 + values[0];
            }
            export function static_index() {
                let values = [3];
                values[2] = 6;
                return values.length * 10 + values[2];
            }
        ",
        &[("grow", &[]), ("resize", &[]), ("static_index", &[])],
    );
}

/// Compact direct probes of the mechanical idioms blitz-js emits for the
/// self-hosted compiler: an operand stack, rest/spread dispatch, function
/// metadata, typed-array bulk writes, and DataView access.
#[test]
fn blitz_js_stack_machine_idioms() {
    cross_test_cases(
        "
            export function stack() {
                let stack = [], tmp;
                stack.length++;
                stack[1] = 40;
                stack.length++;
                stack[2] = 2;
                tmp = stack[2];
                stack.length--;
                return stack[1] + tmp;
            }
            export function rest_spread() {
                let f = function(...locals) {
                    let t = function(...inner) { return inner.length; };
                    return locals.length * 100 + t(...locals);
                };
                return f(40, 2);
            }
            export function function_metadata() {
                let f = function(x) { return x + 1; };
                f.__sig = { params: 1, rets: 1 };
                return f.__sig.params + f(40);
            }
            export function typed_array_set() {
                let mem = new Uint8Array(16);
                mem.set([1, 2, 3], 4);
                return mem[4] + mem[5] + mem[6] + mem.length;
            }
            export function dataview() {
                let mem = new Uint8Array(16);
                let dv = new DataView(mem.buffer);
                dv.setUint32(4, 40, true);
                dv.setUint8(8, 2);
                return dv.getUint32(4, true) + dv.getUint8(8);
            }
        ",
        &[
            ("stack", &[]),
            ("rest_spread", &[]),
            ("function_metadata", &[]),
            ("typed_array_set", &[]),
            ("dataview", &[]),
        ],
    );
}
