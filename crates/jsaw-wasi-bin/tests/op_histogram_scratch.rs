//! Scratch: operator histogram over BackendPlans of the corpus.

use std::collections::BTreeMap;

#[test]
#[ignore = "scratch"]
fn op_histogram() {
    let cases: &[(&str, &str)] = &[
        ("add", "export function add(a, b) { return a + b; }"),
        ("ternary", "export function t(cond) { return cond ? 1 : 2; }"),
        ("loop", "export function sum(n) { let t = 0; for (let i = 0; i < n; i++) { t += i; } return t; }"),
        ("call", "function addOne(x) { return x + 1; } export function combine(a, b) { return addOne(a) + addOne(b); }"),
        ("tail", "export function count(n) { let f = function(k, acc) { if (k <= 0) { return acc; } return f(k - 1, acc + k); }; return f(n, 0); }"),
        ("throw", "export function f(x) { if (x < 0) { throw 1; } return x; }"),
        ("object", "export function makeAndSum(x, y) { const pt = {x: x, y: y}; return pt.x + pt.y; }"),
    ];
    let mut hist: BTreeMap<String, usize> = BTreeMap::new();
    for (name, source) in cases {
        let set = portal_jsc_waffle::module_set_from_sources([("main.js", *source)]).unwrap();
        let mut module = portal_pc_waffle::Module::empty();
        portal_jsc_waffle::convert_modules(
            "main.js",
            &set,
            &mut module,
            &portal_jsc_waffle::ConvertOptions::default(),
        )
        .unwrap();
        let plan = portal_jsc_waffle::backend_plan::BackendPlan::from_module(&module)
            .unwrap_or_else(|d| panic!("{name} plan diagnostics: {d:?}"));
        for func in &plan.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    *hist.entry(inst.operator.clone()).or_default() += 1;
                }
                let _ = &block.terminator;
            }
        }
    }
    for (op, count) in &hist {
        println!("{count:>6}  {op}");
    }
    println!("total operators: {}", hist.len());
}
