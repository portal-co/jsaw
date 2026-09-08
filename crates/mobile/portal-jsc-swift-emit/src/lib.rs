//! Emit a `portal-pc-waffle` IR module as Swift source.
//!
//! The module must satisfy the mobile feature closure (it is audited
//! first). Struct signatures become `final class`es, Wasm arrays become
//! `final class` wrappers (Swift arrays are value types; Wasm arrays are
//! mutable references), typed funcrefs become boxed closure classes, and
//! each function becomes a static method of the `Mod` namespace.
//!
//! Milestone 7 skeleton: classes, funcref boxes, method signatures, and the
//! runtime are emitted; method bodies are stubs filled in by Milestone 10.

pub mod render;

use std::collections::BTreeMap;

use anyhow::bail;
use portal_jsc_mob_emit::{audit, lower, names};
use portal_pc_waffle::{
    EntityRef, ExportKind, FuncDecl, HeapType, Module, SignatureData, StorageType, Type,
};

/// Generated Swift sources: relative file path -> contents.
#[derive(Clone, Debug, Default)]
pub struct SwiftSources {
    pub files: BTreeMap<String, String>,
}

impl SwiftSources {
    /// Render to a single string (for tests and debugging).
    pub fn concatenated(&self) -> String {
        let mut out = String::new();
        for (path, content) in &self.files {
            out.push_str(&format!("// ---- {path} ----\n{content}\n"));
        }
        out
    }

    /// Write the sources into a directory.
    pub fn write_to(&self, dir: impl AsRef<std::path::Path>) -> std::io::Result<()> {
        std::fs::create_dir_all(dir.as_ref())?;
        for (path, content) in &self.files {
            std::fs::write(dir.as_ref().join(path), content)?;
        }
        Ok(())
    }
}

/// Emit a module as Swift sources.
pub fn emit_swift(module: &Module<'_>) -> anyhow::Result<SwiftSources> {
    audit::audit_module(module)?;
    let mut emitter = Emitter {
        module,
        files: BTreeMap::new(),
    };
    emitter.emit_runtime();
    emitter.emit_structs();
    emitter.emit_arrays();
    emitter.emit_funcref_boxes();
    emitter.emit_mod()?;
    Ok(SwiftSources {
        files: emitter.files,
    })
}

/// Map a Wasm type to a Swift type name.
pub fn swift_ty(module: &Module<'_>, ty: Type) -> String {
    match ty {
        Type::I32 => "Int32".to_string(),
        Type::I64 => "Int64".to_string(),
        Type::F32 => "Float".to_string(),
        Type::F64 => "Double".to_string(),
        Type::Heap(h) => match h.value {
            HeapType::Sig { sig_index } => match &module.signatures[sig_index] {
                SignatureData::Struct { .. } => format!("{}?", names::struct_name(sig_index.index())),
                SignatureData::Array { .. } => format!("{}?", arr_name(sig_index.index())),
                SignatureData::Func { .. } => format!("{}?", fnbox_name(sig_index.index())),
                _ => "Any?".to_string(),
            },
            HeapType::FuncRef | HeapType::NoFunc => "IFun?".to_string(),
            _ => "Any?".to_string(),
        },
        _ => "Any?".to_string(),
    }
}

/// Map a Wasm array element storage type to the Swift element type inside
/// the wrapper class.
pub(crate) fn elem_ty(module: &Module<'_>, elem: StorageType) -> String {
    match elem {
        StorageType::I8 => "UInt8".to_string(),
        StorageType::I16 => "UInt16".to_string(),
        StorageType::Val(ty) => swift_ty(module, ty),
        _ => "Any?".to_string(),
    }
}

/// The array-wrapper typealias name for an array signature.
pub(crate) fn arr_name(sig: usize) -> String {
    format!("A{sig}")
}

/// The funcref-box class name for a func signature.
pub(crate) fn fnbox_name(sig: usize) -> String {
    format!("Fn{sig}")
}

struct Emitter<'m> {
    module: &'m Module<'m>,
    files: BTreeMap<String, String>,
}

impl<'m> Emitter<'m> {
    fn file(&mut self, name: &str, content: impl Into<String>) {
        self.files.insert(format!("{name}.swift"), content.into());
    }

    fn emit_runtime(&mut self) {
        self.file(
            "Runtime",
            "/// Runtime support for the emitted module.\n\
             public enum W {\n\
             \x20   /// A Wasm trap (`unreachable`, failed `ref.cast`, trapping arithmetic).\n\
             \x20   public struct WasmTrap: Error {\n\
             \x20       public let message: String\n\
             \x20       public init(_ message: String) { self.message = message }\n\
             \x20   }\n\n\
             \x20   /// One trampoline step: either a final value or a tail-call thunk.\n\
             \x20   public class Step {\n\
             \x20       public static func value(_ v: Any?) -> Step { Value(v) }\n\
             \x20       public static func tail(_ thunk: @escaping () -> Step) -> Step { Tail(thunk) }\n\
             \x20   }\n\
             \x20   public final class Value: Step {\n\
             \x20       public let value: Any?\n\
             \x20       public init(_ v: Any?) { value = v }\n\
             \x20   }\n\
             \x20   public final class Tail: Step {\n\
             \x20       public let thunk: () -> Step\n\
             \x20       public init(_ t: @escaping () -> Step) { thunk = t }\n\
             \x20       public func invoke() -> Step { thunk() }\n\
             \x20   }\n\n\
             \x20   // ---- rotations (Wasm rotl/rotr) ----\n\
             \x20   public static func rotl32(_ a: Int32, _ n: Int32) -> Int32 {\n\
             \x20       let k = UInt32(bitPattern: n) & 31\n\
             \x20       return Int32(bitPattern: (UInt32(bitPattern: a) &<< k) | (UInt32(bitPattern: a) &>> (32 - k)))\n\
             \x20   }\n\
             \x20   public static func rotr32(_ a: Int32, _ n: Int32) -> Int32 {\n\
             \x20       let k = UInt32(bitPattern: n) & 31\n\
             \x20       return Int32(bitPattern: (UInt32(bitPattern: a) &>> k) | (UInt32(bitPattern: a) &<< (32 - k)))\n\
             \x20   }\n\
             \x20   public static func rotl64(_ a: Int64, _ n: Int64) -> Int64 {\n\
             \x20       let k = UInt64(bitPattern: n) & 63\n\
             \x20       return Int64(bitPattern: (UInt64(bitPattern: a) &<< k) | (UInt64(bitPattern: a) &>> (64 - k)))\n\
             \x20   }\n\
             \x20   public static func rotr64(_ a: Int64, _ n: Int64) -> Int64 {\n\
             \x20       let k = UInt64(bitPattern: n) & 63\n\
             \x20       return Int64(bitPattern: (UInt64(bitPattern: a) &>> k) | (UInt64(bitPattern: a) &<< (64 - k)))\n\
             \x20   }\n\n\
             \x20   // ---- NaN-propagating, signed-zero-correct min/max (Wasm f.min/f.max) ----\n\
             \x20   public static func f64min(_ a: Double, _ b: Double) -> Double {\n\
             \x20       if a.isNaN || b.isNaN { return .nan }\n\
             \x20       if a < b { return a }\n\
             \x20       if b < a { return b }\n\
             \x20       return (a.sign == .minus || b.sign == .minus) ? -0.0 : 0.0\n\
             \x20   }\n\
             \x20   public static func f64max(_ a: Double, _ b: Double) -> Double {\n\
             \x20       if a.isNaN || b.isNaN { return .nan }\n\
             \x20       if a > b { return a }\n\
             \x20       if b > a { return b }\n\
             \x20       return (a.sign == .plus || b.sign == .plus) ? 0.0 : -0.0\n\
             \x20   }\n\
             \x20   public static func f32min(_ a: Float, _ b: Float) -> Float {\n\
             \x20       if a.isNaN || b.isNaN { return .nan }\n\
             \x20       if a < b { return a }\n\
             \x20       if b < a { return b }\n\
             \x20       return (a.sign == .minus || b.sign == .minus) ? -0.0 : 0.0\n\
             \x20   }\n\
             \x20   public static func f32max(_ a: Float, _ b: Float) -> Float {\n\
             \x20       if a.isNaN || b.isNaN { return .nan }\n\
             \x20       if a > b { return a }\n\
             \x20       if b > a { return b }\n\
             \x20       return (a.sign == .plus || b.sign == .plus) ? 0.0 : -0.0\n\
             \x20   }\n\n\
             \x20   // ---- saturating conversions (Wasm trunc_sat) ----\n\
             \x20   public static func truncSatS32(_ a: Double) -> Int32 {\n\
             \x20       if a.isNaN { return 0 }\n\
             \x20       if a >= 2147483647.0 { return Int32.max }\n\
             \x20       if a <= -2147483648.0 { return Int32.min }\n\
             \x20       return Int32(a)\n\
             \x20   }\n\
             \x20   public static func truncSatS32(_ a: Float) -> Int32 { truncSatS32(Double(a)) }\n\
             \x20   public static func truncSatU32(_ a: Double) -> Int32 {\n\
             \x20       if a.isNaN || a <= 0 { return 0 }\n\
             \x20       if a >= 4294967295.0 { return -1 }\n\
             \x20       return Int32(bitPattern: UInt32(a))\n\
             \x20   }\n\
             \x20   public static func truncSatU32(_ a: Float) -> Int32 { truncSatU32(Double(a)) }\n\
             \x20   public static func truncSatS64(_ a: Double) -> Int64 {\n\
             \x20       if a.isNaN { return 0 }\n\
             \x20       if a >= 9223372036854775807.0 { return Int64.max }\n\
             \x20       if a <= -9223372036854775808.0 { return Int64.min }\n\
             \x20       return Int64(a)\n\
             \x20   }\n\
             \x20   public static func truncSatS64(_ a: Float) -> Int64 { truncSatS64(Double(a)) }\n\
             \x20   public static func truncSatU64(_ a: Double) -> Int64 {\n\
             \x20       if a.isNaN || a <= 0 { return 0 }\n\
             \x20       if a >= 18446744073709551615.0 { return -1 }\n\
             \x20       return Int64(bitPattern: UInt64(a))\n\
             \x20   }\n\
             \x20   public static func truncSatU64(_ a: Float) -> Int64 { truncSatU64(Double(a)) }\n\n\
             \x20   // ---- array.copy (memmove semantics) ----\n\
             \x20   public static func arrayCopy<E>(_ dst: AArr<E>, _ d: Int, _ src: AArr<E>, _ s: Int, _ n: Int) {\n\
             \x20       dst.items.replaceSubrange(d..<(d + n), with: src.items[s..<(s + n)])\n\
             \x20   }\n\
             \x20   /// `array.len` (null-traps like Wasm).\n\
             \x20   public static func arrLen<E>(_ a: AArr<E>?) -> Int32 { Int32(a!.items.count) }\n\
             }\n\n\
             /// WasmGC array wrapper (reference semantics; Swift arrays are value types).\n\
             public final class AArr<E> {\n\
             \x20   public var items: [E]\n\
             \x20   public init(_ items: [E]) { self.items = items }\n\
             }\n\n\
             /// The JS `null` value (the WasmGC i31 sentinel).\n\
             public final class JsNull {\n\
             \x20   public static let shared = JsNull()\n\
             \x20   private init() {}\n\
             }\n\n\
             /// Marker protocol for all typed funcref boxes.\n\
             public protocol IFun: AnyObject {}\n\n\
             /// Marker protocol for all generated struct classes.\n\
             public protocol IStruct: AnyObject {}\n",
        );
    }

    fn emit_structs(&mut self) {
        for (sig, data) in self.module.signatures.entries() {
            let SignatureData::Struct { fields, .. } = data else {
                continue;
            };
            let name = names::struct_name(sig.index());
            let mut out = format!("public final class {name}: IStruct {{\n");
            for (i, field) in fields.iter().enumerate() {
                let StorageType::Val(ty) = field.value else {
                    continue;
                };
                let sty = swift_ty(self.module, ty);
                out.push_str(&format!("    public var f{i}: {sty}\n"));
            }
            // Defaults on every parameter give `S()` (StructNewDefault)
            // and the all-args initializer (StructNew) in one init.
            let params = fields
                .iter()
                .enumerate()
                .map(|(i, field)| {
                    let (sty, default) = match field.value {
                        StorageType::Val(ty) => (swift_ty(self.module, ty), match ty {
                            Type::I32 | Type::I64 | Type::F32 | Type::F64 => "0",
                            _ => "nil",
                        }),
                        _ => ("Int32".to_string(), "0"),
                    };
                    format!("f{i}: {sty} = {default}")
                })
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("    public init({params}) {{\n"));
            for i in 0..fields.len() {
                out.push_str(&format!("        self.f{i} = f{i}\n"));
            }
            out.push_str("    }\n}\n");
            self.file(&name, out);
        }
    }

    fn emit_arrays(&mut self) {
        for (sig, data) in self.module.signatures.entries() {
            let SignatureData::Array { ty, .. } = data else {
                continue;
            };
            let name = arr_name(sig.index());
            let elem = elem_ty(self.module, ty.value);
            let out = format!(
                "/// WasmGC array type (alias of the generic reference-semantics wrapper).\n\
                 public typealias {name} = AArr<{elem}>\n"
            );
            self.file(&name, out);
        }
    }

    fn emit_funcref_boxes(&mut self) {
        for (sig, data) in self.module.signatures.entries() {
            let SignatureData::Func { params, returns, .. } = data else {
                continue;
            };
            let name = fnbox_name(sig.index());
            let ret = match returns.first() {
                Some(&ty) => swift_ty(self.module, ty),
                None => "Void".to_string(),
            };
            let params_ty = params
                .iter()
                .map(|&ty| swift_ty(self.module, ty))
                .collect::<Vec<_>>()
                .join(", ");
            let out = format!(
                "/// Typed funcref box: `body` is the plain call, `step` the\n\
                 /// trampoline-protocol entry (one step).\n\
                 public final class {name}: IFun {{\n\
                 \x20   public let body: ({params_ty}) -> {ret}\n\
                 \x20   public let step: ({params_ty}) -> W.Step\n\
                 \x20   public init(body: @escaping ({params_ty}) -> {ret}, step: @escaping ({params_ty}) -> W.Step) {{\n\
                 \x20       self.body = body\n\
                 \x20       self.step = step\n\
                 \x20   }}\n\
                 }}\n"
            );
            self.file(&name, out);
        }
    }

    fn emit_mod(&mut self) -> anyhow::Result<()> {
        let mut out = String::from("public enum Mod {\n");
        let tail_set = portal_jsc_mob_emit::tail::tail_callable_set(self.module);
        let mut renderer = render::Renderer::new(self.module, &tail_set);
        for (func, decl) in self.module.funcs.entries() {
            let (sig, name) = match decl {
                FuncDecl::Body(sig, name, _) => (*sig, name.as_str()),
                _ => bail!("mobile closure violation: non-body function {func:?}"),
            };
            let SignatureData::Func { params, returns, .. } = &self.module.signatures[sig] else {
                bail!("function {name} has a non-function signature");
            };
            let ret = match returns.first() {
                Some(&ty) => swift_ty(self.module, ty),
                None => "Void".to_string(),
            };
            let n_params = params.len();
            let params = params
                .iter()
                .enumerate()
                .map(|(i, &ty)| format!("_ l{i}: {}", swift_ty(self.module, ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let args = (0..n_params)
                .map(|i| format!("l{i}"))
                .collect::<Vec<_>>()
                .join(", ");
            let fname = names::func_name(func.index());
            let FuncDecl::Body(_, _, body) = &self.module.funcs[func] else {
                unreachable!()
            };
            let sfunc = lower::lower_body(body)?;
            let rendered = renderer.render_func(func, &sfunc)?;
            let body = &rendered.body_src;
            // Self tail calls compile to `continue selfTail` inside a
            // wrapping loop; everything else is the plain body.
            let wrap_self = |inner: &str| {
                if rendered.has_self_tail {
                    format!("    selfTail: while true {{\n{inner}    }}\n")
                } else {
                    inner.to_string()
                }
            };
            if rendered.needs_step {
                let step = render::step_name(func.index());
                out.push_str(&format!(
                    "\n    /// {name} (trampoline body)\n\
                     \x20   public static func {step}({params}) -> W.Step {{\n{}\
                     \x20   }}\n",
                    wrap_self(body)
                ));
                let finish = match ret.as_str() {
                    "Void" => "return".to_string(),
                    "Int32" => "return (s as! W.Value).value as! Int32".to_string(),
                    "Int64" => "return (s as! W.Value).value as! Int64".to_string(),
                    "Float" => "return (s as! W.Value).value as! Float".to_string(),
                    "Double" => "return (s as! W.Value).value as! Double".to_string(),
                    "Any?" => "return (s as! W.Value).value".to_string(),
                    other => {
                        let base = other.strip_suffix('?').unwrap_or(other);
                        format!("return (s as! W.Value).value.map {{ $0 as! {base} }}")
                    }
                };
                out.push_str(&format!(
                    "\n    /// {name}\n\
                     \x20   public static func {fname}({params}) -> {ret} {{\n\
                     \x20       var s = {step}({args})\n\
                     \x20       while let t = s as? W.Tail {{ s = t.invoke() }}\n\
                     \x20       {finish}\n\
                     \x20   }}\n"
                ));
            } else {
                out.push_str(&format!(
                    "\n    /// {name}\n\
                     \x20   public static func {fname}({params}) -> {ret} {{\n{}\
                     \x20   }}\n",
                    wrap_self(body)
                ));
            }
        }
        for export in &self.module.exports {
            let ExportKind::Func(func) = export.kind else {
                continue;
            };
            let FuncDecl::Body(sig, _, _) = &self.module.funcs[func] else {
                continue;
            };
            let SignatureData::Func { params, returns, .. } = &self.module.signatures[*sig] else {
                continue;
            };
            let ret = match returns.first() {
                Some(&ty) => swift_ty(self.module, ty),
                None => "Void".to_string(),
            };
            let ename = names::swift(&export.name);
            let n_params = params.len();
            let params = params
                .iter()
                .enumerate()
                .map(|(i, &ty)| format!("_ a{i}: {}", swift_ty(self.module, ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let args = (0..n_params)
                .map(|i| format!("a{i}"))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!(
                "\n    /// Export `{}`.\n\
                 \x20   public static func {ename}({params}) -> {ret} {{\n\
                 \x20       return {}({args})\n\
                 \x20   }}\n",
                export.name,
                names::func_name(func.index()),
            ));
        }
        out.push_str("}\n");
        self.file("Mod", out);
        Ok(())
    }
}
