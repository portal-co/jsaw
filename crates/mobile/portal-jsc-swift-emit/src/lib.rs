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

use std::collections::BTreeMap;

use anyhow::bail;
use portal_jsc_mob_emit::{audit, names};
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
fn elem_ty(module: &Module<'_>, elem: StorageType) -> String {
    match elem {
        StorageType::I8 => "UInt8".to_string(),
        StorageType::I16 => "UInt16".to_string(),
        StorageType::Val(ty) => swift_ty(module, ty),
        _ => "Any?".to_string(),
    }
}

/// The array-wrapper class name for an array signature.
fn arr_name(sig: usize) -> String {
    format!("A{sig}")
}

/// The funcref-box class name for a func signature.
fn fnbox_name(sig: usize) -> String {
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
             \x20   }\n\
             }\n\n\
             /// The JS `null` value (the WasmGC i31 sentinel).\n\
             public final class JsNull {\n\
             \x20   public static let shared = JsNull()\n\
             \x20   private init() {}\n\
             }\n\n\
             /// Marker protocol for all typed funcref boxes.\n\
             public protocol IFun: AnyObject {}\n",
        );
    }

    fn emit_structs(&mut self) {
        for (sig, data) in self.module.signatures.entries() {
            let SignatureData::Struct { fields, .. } = data else {
                continue;
            };
            let name = names::struct_name(sig.index());
            let mut out = format!("public final class {name} {{\n");
            for (i, field) in fields.iter().enumerate() {
                let StorageType::Val(ty) = field.value else {
                    continue;
                };
                let sty = swift_ty(self.module, ty);
                out.push_str(&format!("    public var f{i}: {sty}\n"));
            }
            let params = fields
                .iter()
                .enumerate()
                .map(|(i, field)| match field.value {
                    StorageType::Val(ty) => format!("f{i}: {}", swift_ty(self.module, ty)),
                    _ => format!("f{i}: Int32"),
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
                "/// WasmGC array wrapper (reference semantics).\n\
                 public final class {name} {{\n\
                 \x20   public var items: [{elem}]\n\
                 \x20   public init(_ items: [{elem}]) {{ self.items = items }}\n\
                 }}\n"
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
                "/// Typed funcref box.\n\
                 public final class {name}: IFun {{\n\
                 \x20   public let body: ({params_ty}) -> {ret}\n\
                 \x20   public init(_ body: @escaping ({params_ty}) -> {ret}) {{ self.body = body }}\n\
                 }}\n"
            );
            self.file(&name, out);
        }
    }

    fn emit_mod(&mut self) -> anyhow::Result<()> {
        let mut out = String::from("public enum Mod {\n");
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
            let params = params
                .iter()
                .enumerate()
                .map(|(i, &ty)| format!("_ a{i}: {}", swift_ty(self.module, ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let fname = names::func_name(func.index());
            out.push_str(&format!(
                "\n    /// {name}\n\
                 \x20   public static func {fname}({params}) -> {ret} {{\n\
                 \x20       fatalError(\"{fname} not yet emitted\")\n\
                 \x20   }}\n"
            ));
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
