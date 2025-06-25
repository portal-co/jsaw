use itertools::Itertools;

fn main() {
    let mut args = std::env::args();
    args.next();
    let root = args.next().expect("the source root");
    emit_base_intrinsics(&root);
}
fn emit_base_intrinsics(a: &str) {
    std::fs::write(
        format!("{a}/jsaw-intrinsics-base/index.ts"),
        format!(
            "
{}
    ",
            [
                ("a + b", "fast_add"),
                ("a & b", "fast_and"),
                ("a | b", "fast_or"),
                ("a === b", "fast_eq"),
                ("a - b", "fast_sub"),
                ("a << b", "fast_shl"),
                ("a * b", "fast_mul"),
                ("Math.imul(a,b)", "fast_imul"),
            ]
            .into_iter()
            .map(|(a, b)| format!("export const {b} = (globalThis as any)['~Natives_{b}'] ?? ((a: any, b: any) => {a});"))
            .chain(["assert", "comptime"].into_iter().flat_map(|a| [
                format!("export const {a}_string =  (globalThis as any)['~Natives_{a}_string'] ?? ((a: string) => a)"),
                format!("export const {a}_number = (globalThis as any)['~Natives_{a}_number'] ??( (a: number) => a)"),
                format!("export const {a}_static_fn = (globalThis as any)['~Natives_{a}_static_fn'] ?? ((a: Function) => a)"),
            ]))
            .join("\n")
        ),
    )
    .expect("to write the intrinsics")
}
