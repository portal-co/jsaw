use std::{collections::BTreeMap, fmt::Display};

pub use portal_jsc_common::ImportMap;
use swc_atoms::Atom;
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceFile, SourceMap, Span, Spanned,
};
use swc_ecma_ast::{Expr, ExprStmt, Id, Ident, Lit, Module, ModuleDecl, ModuleItem, Stmt};
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax};

pub mod brighten;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Natives {
    pub all: BTreeMap<Atom, Id>,
}

#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct MakeSpanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned for MakeSpanned<T> {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ImportOr<T> {
    NotImport(T),
    Import {
        value: T,
        module: Atom,
        name: ImportMap<Atom>,
    },
}
impl<T: Default> Default for ImportOr<T> {
    fn default() -> Self {
        Self::NotImport(T::default())
    }
}

impl<T> AsRef<T> for ImportOr<T> {
    fn as_ref(&self) -> &T {
        match self {
            ImportOr::NotImport(value) => value,
            ImportOr::Import {
                value,
                module,
                name,
            } => value,
        }
    }
}
impl<T> AsMut<T> for ImportOr<T> {
    fn as_mut(&mut self) -> &mut T {
        match self {
            ImportOr::NotImport(value) => value,
            ImportOr::Import {
                value,
                module,
                name,
            } => value,
        }
    }
}
impl<T: Spanned> Spanned for ImportOr<T> {
    fn span(&self) -> Span {
        match self {
            ImportOr::NotImport(a) => a.span(),
            ImportOr::Import {
                value,
                module,
                name,
            } => value.span(),
        }
    }
}
pub trait Extract<T>: AsRef<T> + AsMut<T> {
    fn extract_own(self) -> T;
}
impl<T> Extract<T> for ImportOr<T> {
    fn extract_own(self) -> T {
        match self {
            ImportOr::NotImport(value) => value,
            ImportOr::Import {
                value,
                module,
                name,
            } => value,
        }
    }
}
impl<T: AsRef<T> + AsMut<T>> Extract<T> for T {
    fn extract_own(self) -> T {
        self
    }
}
// impl<T, U: Into<T> + AsRef<T> + AsMut<T>> Extract<T> for U {}

pub fn mangle((a, b): &Id) -> Atom {
    Atom::new(format!("{}${a}", b.as_u32()))
}
pub trait ImportMapper {
    fn import_of(&self, cx: &Id) -> Option<(Atom, ImportMap<Atom>)>;
}
pub trait ModuleMapper {
    fn item_of(&self, id: &Id) -> Option<&ModuleItem>;
}
#[cfg(feature = "ty")]
pub mod r#type;
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum BreakKind {
    BreakAfter,
    DoNotBreakAfter,
}
#[cfg(feature = "cli")]
pub fn cli_load(cm: &Lrc<SourceMap>, fm: &Lrc<SourceFile>) -> Module {
    // let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");
    // let fm = cm.new_source_file(
    //     FileName::Custom("test.js".into()).into(),
    //     "function foo() {}".into(),
    // );
    let lexer = Lexer::new(
        // We want to parse ecmascript
        Syntax::Es(Default::default()),
        // EsVersion defaults to es5
        Default::default(),
        StringInput::from(&**fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_module()
        .map_err(|mut e| {
            // Unrecoverable fatal error occurred
            e.into_diagnostic(&handler).emit()
        })
        .expect("failed to parser module");
    return module;
}
