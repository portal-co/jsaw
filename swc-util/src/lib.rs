use std::fmt::Display;

use swc_atoms::Atom;
use swc_common::{Span, Spanned};
use swc_ecma_ast::{Expr, ExprStmt, Id, Ident, Lit, ModuleDecl, ModuleItem, Stmt};

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
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ImportMap<T> {
    Default,
    Star,
    Named { name: T },
}
pub fn mangle((a, b): &Id) -> Atom {
    Atom::new(format!("{}${a}", b.as_u32()))
}
pub trait ImportMapper {
    fn import_of(&self, cx: &Id) -> Option<(Atom, ImportMap<Atom>)>;
}
pub trait ModuleMapper{
    fn item_of(&self, id: &Id) -> Option<&ModuleItem>;
}