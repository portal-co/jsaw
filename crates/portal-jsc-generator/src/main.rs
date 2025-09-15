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
                ("a + b", "fast_add","number"),
                ("a & b", "fast_and","number"),
                ("a | b", "fast_or","number"),
                ("a === b", "fast_eq","boolean"),
                ("a - b", "fast_sub","number"),
                ("a << b", "fast_shl","number"),
                ("a * b", "fast_mul","number"),
                ("Math.imul(a,b)", "fast_imul","number"),
            ]
            .into_iter()
            .map(|(a, b,c)| format!("export const {b}: (a: number,b: number) => {c} = (globalThis as any)['~Natives_{b}'] ?? ((a: any, b: any) => {a});"))
            .chain(["assert", "comptime"].into_iter().flat_map(|a| [
                format!("export const {a}_string: (a: string) => string =  (globalThis as any)['~Natives_{a}_string'] ?? ((a: string) => a)"),
                format!("export const {a}_number: (a: number) => number = (globalThis as any)['~Natives_{a}_number'] ??( (a: number) => a)"),
                format!("export const {a}_static_fn: (a: Function) => Function = (globalThis as any)['~Natives_{a}_static_fn'] ?? ((a: Function) => a)"),
            ])).chain([
                format!("export const inlineme: () => void = (globalThis as any)['~Natives_inlineme'] ?? (()=>{{}})"),
                format!("export const inlineme_n: (n: number) => void = (globalThis as any)['~Natives_inlineme_n'] ?? ((n: number)=>{{}})")
            ])
            .join("\n")
        ),
    )
    .expect("to write the intrinsics")
}
