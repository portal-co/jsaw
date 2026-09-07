//! Emit a `portal-pc-waffle` IR module as Java source.
//!
//! The module must satisfy the mobile feature closure (it is audited
//! first). Struct signatures become `final class`es with public fields,
//! Wasm arrays become plain Java arrays (reference semantics already),
//! typed funcrefs become functional interfaces, and each function becomes a
//! static method of `Mod`.
//!
//! Milestone 7 skeleton: classes, interfaces, method signatures, and the
//! runtime are emitted; method bodies are stubs filled in by Milestone 8.

use std::collections::BTreeMap;

use anyhow::bail;
use portal_jsc_mob_emit::{audit, names};
use portal_pc_waffle::{
    EntityRef, ExportKind, FuncDecl, HeapType, Module, SignatureData, StorageType, Type,
};

/// Generated Java sources: relative file path -> contents.
#[derive(Clone, Debug, Default)]
pub struct JavaSources {
    pub files: BTreeMap<String, String>,
}

impl JavaSources {
    /// Render to a single string (for tests and debugging).
    pub fn concatenated(&self) -> String {
        let mut out = String::new();
        for (path, content) in &self.files {
            out.push_str(&format!("// ---- {path} ----\n{content}\n"));
        }
        out
    }
}

/// The Java package all generated files live in.
pub const PACKAGE: &str = "pc.portal.mob";

/// Emit a module as Java sources.
pub fn emit_java(module: &Module<'_>) -> anyhow::Result<JavaSources> {
    audit::audit_module(module)?;
    let mut emitter = Emitter {
        module,
        files: BTreeMap::new(),
    };
    emitter.emit_runtime();
    emitter.emit_structs();
    emitter.emit_funcref_interfaces();
    emitter.emit_mod()?;
    Ok(JavaSources {
        files: emitter.files,
    })
}

/// Map a Wasm type to a Java type name.
pub fn java_ty(module: &Module<'_>, ty: Type) -> String {
    match ty {
        Type::I32 => "int".to_string(),
        Type::I64 => "long".to_string(),
        Type::F32 => "float".to_string(),
        Type::F64 => "double".to_string(),
        Type::Heap(h) => match h.value {
            HeapType::Sig { sig_index } => match &module.signatures[sig_index] {
                SignatureData::Struct { .. } => names::struct_name(sig_index.index()),
                SignatureData::Array { ty, .. } => array_ty(module, ty.value),
                SignatureData::Func { .. } => iface_name(sig_index.index()),
                _ => "Object".to_string(),
            },
            HeapType::FuncRef | HeapType::NoFunc => "IFun".to_string(),
            _ => "Object".to_string(),
        },
        _ => "Object".to_string(),
    }
}

/// Map a Wasm array element storage type to a Java array type.
fn array_ty(module: &Module<'_>, elem: StorageType) -> String {
    match elem {
        StorageType::I8 => "byte[]".to_string(),
        StorageType::I16 => "char[]".to_string(),
        StorageType::Val(ty) => format!("{}[]", java_ty(module, ty)),
        _ => "Object[]".to_string(),
    }
}

/// The functional-interface name for a func signature.
fn iface_name(sig: usize) -> String {
    format!("I{sig}")
}

struct Emitter<'m> {
    module: &'m Module<'m>,
    files: BTreeMap<String, String>,
}

impl<'m> Emitter<'m> {
    fn file(&mut self, name: &str, content: impl Into<String>) {
        self.files
            .insert(format!("{}/{name}.java", PACKAGE.replace('.', "/")), content.into());
    }

    fn emit_runtime(&mut self) {
        self.file(
            "W",
            format!(
                "package {PACKAGE};\n\n\
                 /** Runtime support for the emitted module. */\n\
                 public final class W {{\n\
                 \x20   private W() {{}}\n\n\
                 \x20   /** A Wasm trap (`unreachable`, failed `ref.cast`, trapping arithmetic). */\n\
                 \x20   public static final class WasmTrap extends RuntimeException {{\n\
                 \x20       public WasmTrap(String message) {{ super(message); }}\n\
                 \x20   }}\n\n\
                 \x20   /** The JS `null` value (the WasmGC i31 sentinel). */\n\
                 \x20   public static final class JsNull {{\n\
                 \x20       public static final JsNull INSTANCE = new JsNull();\n\
                 \x20       private JsNull() {{}}\n\
                 \x20   }}\n\n\
                 \x20   /** Marker super-interface for all typed funcref interfaces. */\n\
                 \x20   public interface IFun {{}}\n\n\
                 \x20   /** Non-constant `true`: keeps `javac` reachability analysis from\n\
                 \x20       rejecting Wasm-shaped infinite loops. */\n\
                 \x20   public static boolean T = true;\n\
                 }}\n"
            ),
        );
        // Top-level alias so mapped funcref types can just say `IFun`.
        self.file(
            "IFun",
            format!(
                "package {PACKAGE};\n\n\
                 /** Marker super-interface for all typed funcref interfaces. */\n\
                 public interface IFun extends W.IFun {{}}\n"
            ),
        );
    }

    fn emit_structs(&mut self) {
        for (sig, data) in self.module.signatures.entries() {
            let SignatureData::Struct { fields, .. } = data else {
                continue;
            };
            let name = names::struct_name(sig.index());
            let mut out = format!("package {PACKAGE};\n\npublic final class {name} {{\n");
            for (i, field) in fields.iter().enumerate() {
                let StorageType::Val(ty) = field.value else {
                    continue;
                };
                let jty = java_ty(self.module, ty);
                out.push_str(&format!("    public {jty} f{i};\n"));
            }
            // No-arg constructor for large StructNew sites (see below).
            out.push_str(&format!("    public {name}() {{}}\n"));
            // All-args constructor for StructNew emission. Java limits
            // methods (constructors included) to 255 parameter slots, so
            // larger structs — e.g. the generated global-context struct —
            // are populated field-by-field at the construction site.
            if !fields.is_empty() && fields.len() <= 255 {
                let params = fields
                    .iter()
                    .enumerate()
                    .map(|(i, field)| match field.value {
                        StorageType::Val(ty) => format!("{} f{i}", java_ty(self.module, ty)),
                        _ => "int f{i}".to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!("    public {name}({params}) {{\n"));
                for i in 0..fields.len() {
                    out.push_str(&format!("        this.f{i} = f{i};\n"));
                }
                out.push_str("    }\n");
            }
            out.push_str("}\n");
            self.file(&name, out);
        }
    }

    fn emit_funcref_interfaces(&mut self) {
        for (sig, data) in self.module.signatures.entries() {
            let SignatureData::Func { params, returns, .. } = data else {
                continue;
            };
            let name = iface_name(sig.index());
            let ret = match returns.first() {
                Some(&ty) => java_ty(self.module, ty),
                None => "void".to_string(),
            };
            let params = params
                .iter()
                .enumerate()
                .map(|(i, &ty)| format!("{} a{i}", java_ty(self.module, ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let out = format!(
                "package {PACKAGE};\n\n\
                 public interface {name} extends IFun {{\n\
                 \x20   {ret} apply({params});\n\
                 }}\n"
            );
            self.file(&name, out);
        }
    }

    fn emit_mod(&mut self) -> anyhow::Result<()> {
        let mut out = format!("package {PACKAGE};\n\npublic final class Mod {{\n    private Mod() {{}}\n");
        for (func, decl) in self.module.funcs.entries() {
            let (sig, name) = match decl {
                FuncDecl::Body(sig, name, _) => (*sig, name.as_str()),
                _ => bail!("mobile closure violation: non-body function {func:?}"),
            };
            let SignatureData::Func { params, returns, .. } = &self.module.signatures[sig] else {
                bail!("function {name} has a non-function signature");
            };
            let ret = match returns.first() {
                Some(&ty) => java_ty(self.module, ty),
                None => "void".to_string(),
            };
            let params = params
                .iter()
                .enumerate()
                .map(|(i, &ty)| format!("{} a{i}", java_ty(self.module, ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let fname = names::func_name(func.index());
            out.push_str(&format!(
                "\n    /** {name} */\n\
                 \x20   public static {ret} {fname}({params}) {{\n\
                 \x20       throw new W.WasmTrap(\"{fname} not yet emitted\");\n\
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
                Some(&ty) => java_ty(self.module, ty),
                None => "void".to_string(),
            };
            let ename = names::java(&export.name);
            let n_params = params.len();
            let params = params
                .iter()
                .enumerate()
                .map(|(i, &ty)| format!("{} a{i}", java_ty(self.module, ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let args = (0..n_params)
                .map(|i| format!("a{i}"))
                .collect::<Vec<_>>()
                .join(", ");
            let ret_kw = if ret == "void" { "" } else { "return " };
            out.push_str(&format!(
                "\n    /** Export `{}`. */\n\
                 \x20   public static {ret} {ename}({params}) {{\n\
                 \x20       {ret_kw}{}({args});\n\
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
