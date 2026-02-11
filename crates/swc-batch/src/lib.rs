use std::{collections::BTreeMap, sync::Arc};
use portal_jsc_swc_util::{ImportMap, ImportMapper, ModuleMapper};
use swc_atoms::{Atom, Wtf8Atom};
use swc_common::{sync::Lrc, Mark, Spanned, SyntaxContext};
use swc_ecma_ast::{
    BlockStmt, CallExpr, Decl, Expr, ExprOrSpread, FnDecl, FnExpr, Function, Id, Ident, IdentName,
    Import, ImportDecl, Lit, MemberExpr, MethodProp, Module, ModuleDecl, ModuleItem, ObjectLit,
    PropOrSpread, ReturnStmt, Stmt, Str, VarDecl, VarDeclarator,
};
use swc_ecma_visit::{Visit, VisitMut, VisitMutWith};
pub fn setup(mut module: Module) -> (ImportMapping, ModuleMapping) {
    let mut i = ImportMapping::default();
    module.visit_mut_with(&mut i);
    (i, ModuleMapping::new(module))
}
#[derive(Default)]
pub struct ImportMapping {
    pub mapping: BTreeMap<(Atom, SyntaxContext), (Wtf8Atom, ImportMap<Atom>)>,
}
impl VisitMut for ImportMapping {
    fn visit_mut_import_decl(&mut self, a: &mut ImportDecl) {
        let src = a.src.as_ref().value.clone();
        for s in a.specifiers.iter() {
            match s {
                swc_ecma_ast::ImportSpecifier::Named(import_named_specifier) => {
                    if let Some(module) = &import_named_specifier.imported {
                        self.mapping.insert(
                            import_named_specifier.local.to_id(),
                            (
                                src.clone(),
                                ImportMap::Named {
                                    name: (&*module.atom()).clone(),
                                },
                            ),
                        );
                    } else {
                        self.mapping.insert(
                            import_named_specifier.local.to_id(),
                            (
                                src.clone(),
                                ImportMap::Named {
                                    name: import_named_specifier.local.to_id().0,
                                },
                            ),
                        );
                    }
                }
                swc_ecma_ast::ImportSpecifier::Default(import_default_specifier) => {
                    // if let Some(module) = &import_named_specifier.imported{
                    //     self.mapping.insert(import_named_specifier.local.to_id(), (Mark::new(),src.clone(),Some(module.atom().clone())));
                    //     }else{
                    self.mapping.insert(
                        import_default_specifier.local.to_id(),
                        (src.clone(), ImportMap::Default),
                    );
                    // }
                }
                swc_ecma_ast::ImportSpecifier::Namespace(import_star_as_specifier) => {
                    self.mapping.insert(
                        import_star_as_specifier.local.to_id(),
                        (src.clone(), ImportMap::Star),
                    );
                }
            }
        }
        a.visit_mut_children_with(self);
    }
    fn visit_mut_ident(&mut self, ident: &mut Ident) {
        ident.visit_mut_children_with(self);
    }
}
impl ImportMapper for ImportMapping {
    fn import_of(&self, cx: &Id) -> Option<(Wtf8Atom, ImportMap<Atom>)> {
        if let Some(a) = self.mapping.get(cx) {
            return Some(a.clone());
        }
        return None;
    }
}
#[derive(Default)]
pub struct ModuleMapping {
    pub mapping: BTreeMap<Id, Arc<ModuleItem>>,
    pub remainder: Vec<ModuleItem>,
}
impl ModuleMapper for ModuleMapping {
    fn item_of(&self, id: &Id) -> Option<&ModuleItem> {
        self.mapping.get(id).map(|a| &**a)
    }
}
impl ModuleMapping {
    pub fn new(m: Module) -> Self {
        let mut r = vec![];
        Self {
            mapping: m
                .body
                .into_iter()
                .flat_map(|d| {
                    // match d{ModuleItem::ModuleDecl(d) => {
                    let d = Arc::new(d);
                    match &*d {
                        ModuleItem::ModuleDecl(module_decl) => match &module_decl {
                            ModuleDecl::Import(import_decl) => import_decl
                                .specifiers
                                .iter()
                                .flat_map(|s| match s {
                                    swc_ecma_ast::ImportSpecifier::Named(
                                        import_named_specifier,
                                    ) => vec![(import_named_specifier.local.to_id(), d.clone())],
                                    swc_ecma_ast::ImportSpecifier::Default(
                                        import_default_specifier,
                                    ) => vec![(import_default_specifier.local.to_id(), d.clone())],
                                    swc_ecma_ast::ImportSpecifier::Namespace(
                                        import_star_as_specifier,
                                    ) => vec![(import_star_as_specifier.local.to_id(), d.clone())],
                                })
                                .collect::<Vec<_>>(),
                            _ => {
                                r.push(d);
                                vec![]
                            }
                        },
                        ModuleItem::Stmt(stmt) => match stmt {
                            Stmt::Decl(d2) => match d2 {
                                Decl::Var(v) => v
                                    .decls
                                    .iter()
                                    .filter_map(|d2| d2.name.as_ident())
                                    .map(|i| (i.id.to_id(), d.clone()))
                                    .collect(),
                                Decl::Fn(f) => vec![(f.ident.to_id(), d.clone())],
                                _ => {
                                    r.push(d);
                                    vec![]
                                }
                            },
                            _ => {
                                r.push(d);
                                vec![]
                            }
                        },
                    }
                })
                .collect(),
            remainder: r.into_iter().map(|a| Arc::unwrap_or_clone(a)).collect(),
        }
    }
}
pub struct SourceMapper {
    pub sm: Lrc<swc_common::SourceMap>,
}
impl VisitMut for SourceMapper {
    fn visit_mut_decl(&mut self, node: &mut Decl) {
        if let Decl::Fn(f) = node.clone() {
            *node = Decl::Var(Box::new(VarDecl {
                span: f.span(),
                ctxt: f.ident.ctxt.clone(),
                kind: swc_ecma_ast::VarDeclKind::Const,
                declare: true,
                decls: vec![VarDeclarator {
                    span: f.span(),
                    name: f.ident.clone().into(),
                    init: Some(Box::new(Expr::Fn(FnExpr {
                        ident: Some(f.ident),
                        function: f.function,
                    }))),
                    definite: false,
                }],
            }))
        }
        node.visit_mut_children_with(self);
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);
        if let Expr::Fn(f) = node.clone() {
            let a = Expr::Object(ObjectLit {
                span: f.span(),
                props: vec![PropOrSpread::Prop(Box::new(swc_ecma_ast::Prop::Method(
                    MethodProp {
                        key: swc_ecma_ast::PropName::Ident(IdentName {
                            span: f.span(),
                            sym: Atom::new("toString"),
                        }),
                        function: Box::new(Function {
                            params: vec![],
                            decorators: vec![],
                            span: f.span(),
                            ctxt: f.function.ctxt.clone(),
                            body: Some(BlockStmt {
                                span: f.span(),
                                ctxt: f.function.ctxt.clone(),
                                stmts: vec![Stmt::Return(ReturnStmt {
                                    span: f.span(),
                                    arg: Some(Box::new(Expr::Lit(Lit::Str(Str {
                                        span: f.span(),
                                        raw: None,
                                        value: Wtf8Atom::new(self.sm.span_to_string(f.span())),
                                    })))),
                                })],
                            }),
                            is_async: false,
                            is_generator: false,
                            type_params: None,
                            return_type: None,
                        }),
                    },
                )))],
            });
            *node = Expr::Call(CallExpr {
                span: f.span(),
                ctxt: SyntaxContext::empty(),
                callee: swc_ecma_ast::Callee::Expr(Box::new(Expr::Member(MemberExpr {
                    span: f.span(),
                    obj: Box::new(
                        Ident::new(Atom::new("Object"), f.span(), f.function.ctxt.clone()).into(),
                    ),
                    prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                        span: f.span(),
                        sym: Atom::new("assign"),
                    }),
                }))),
                args: vec![
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(Expr::Fn(f)),
                    },
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(a),
                    },
                ],
                type_args: None,
            })
        }
    }
}
