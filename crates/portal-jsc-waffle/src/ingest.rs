//! Source ingestion: parse ECMAScript module sources into a [`ModuleSet`]
//! of SSA modules ready for [`convert_modules`](crate::convert_modules).
//!
//! This is the multi-module front half of the compiler pulled out of the
//! e2e harness so both the tests and the `jsaw-wasi-bin` WebAssembly CLI
//! share exactly one parse → CFG → TAC → SSA → `ModuleSet` pipeline. It
//! owns the `GLOBALS` scoped TLS that swc parsing and lowering require.

use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::module::{SModule, SModuleBuilder};
use portal_jsc_swc_tac::module::TModule;
use swc_common::{FileName, GLOBALS, Globals, SourceMap, input::SourceFileInput, sync::Lrc};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{Context, EsSyntax, Lexer, Parser, Syntax, parse_file_as_module};

use crate::{ConvertError, ModuleSet};

/// Parse one ECMAScript module source into its SSA module.
///
/// The caller supplies `path` purely for parse diagnostics (the file name
/// in the source map); the SSA module does not retain it. Must be called
/// inside [`with_globals`] (or [`module_set_from_sources`], which wraps
/// it) because swc lowering reads scoped TLS.
pub fn parse_module_source(path: &str, source: &str) -> Result<SModule, ConvertError> {
    let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
    let file = cm.new_source_file(
        Lrc::new(FileName::Custom(path.to_owned())),
        source.to_owned(),
    );
    let mut errors = vec![];
    let module = parse_file_as_module(
        &file,
        Syntax::Es(EsSyntax::default()),
        EsVersion::Es2022,
        None,
        &mut errors,
    )
    .map_err(|error| ConvertError::invalid(format!("parse of {path:?} failed: {error:?}")))?;
    if !errors.is_empty() {
        return Err(ConvertError::invalid(format!(
            "parse of {path:?} reported diagnostics: {errors:?}"
        )));
    }
    let cfg = CfgModule::try_from(module).map_err(|error| {
        ConvertError::invalid(format!("CFG lowering of {path:?} failed: {error:?}"))
    })?;
    let tac = TModule::try_from(cfg).map_err(|error| {
        ConvertError::invalid(format!("TAC lowering of {path:?} failed: {error:?}"))
    })?;
    SModule::try_from(tac).map_err(|error| {
        ConvertError::invalid(format!("SSA lowering of {path:?} failed: {error:?}"))
    })
}

/// Parse and lower one module without retaining a complete SWC AST plus CFG,
/// TAC, and SSA copies of every hoisted function at once.
///
/// This is intentionally a separate entry point: the existing
/// [`parse_module_source`] stays the simple whole-module API for small inputs
/// and callers that need a full parse diagnostic. Large generated modules use
/// this streaming path, which hands each parser item to jsaw-core's
/// [`SModuleBuilder`] immediately and therefore bounds retained function IR
/// to the item currently being translated plus the final SSA module.
pub fn parse_module_source_lazy(path: &str, source: &str) -> Result<SModule, ConvertError> {
    let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
    let file = cm.new_source_file(
        Lrc::new(FileName::Custom(path.to_owned())),
        source.to_owned(),
    );
    let lexer = Lexer::new(
        Syntax::Es(EsSyntax::default()),
        EsVersion::Es2022,
        SourceFileInput::from(&*file),
        None,
    );
    let mut parser = Parser::new_from(lexer);
    parser.set_ctx(
        parser.ctx() | Context::Module | Context::CanBeModule | Context::TopLevel | Context::Strict,
    );
    parser
        .parse_shebang()
        .map_err(|error| ConvertError::invalid(format!("parse of {path:?} failed: {error:?}")))?;
    let mut builder = SModuleBuilder::new();
    // `Token` is intentionally not part of swc_ecma_parser's public API.
    // Its EOF token has a zero-width span at `file.end_pos`, so compare the
    // public current-token span instead without depending on parser internals.
    while parser.input().get_cur().span.lo < file.end_pos {
        let item = parser.parse_module_item().map_err(|error| {
            ConvertError::invalid(format!("parse of {path:?} failed: {error:?}"))
        })?;
        builder.append(item).map_err(|error| {
            ConvertError::invalid(format!(
                "incremental lowering of {path:?} failed: {error:?}"
            ))
        })?;
    }
    let errors = parser.take_errors();
    if !errors.is_empty() {
        return Err(ConvertError::invalid(format!(
            "parse of {path:?} reported diagnostics: {errors:?}"
        )));
    }
    builder.finish().map_err(|error| {
        ConvertError::invalid(format!(
            "incremental SSA lowering of {path:?} failed: {error:?}"
        ))
    })
}

/// Run `f` with a fresh swc `GLOBALS` scoped TLS installed.
pub fn with_globals<R>(f: impl FnOnce() -> R) -> R {
    GLOBALS.set(&Globals::default(), f)
}

/// Parse every `(path, source)` pair into a [`ModuleSet`].
///
/// Paths must be the relative module keys the linker resolves against
/// (e.g. `"index.js"`, `"lib/a.js"`); each becomes the set key verbatim.
/// The whole parse runs under one fresh `GLOBALS` scope.
pub fn module_set_from_sources<'s, I>(modules: I) -> Result<ModuleSet<'s>, ConvertError>
where
    I: IntoIterator<Item = (&'s str, &'s str)>,
{
    module_set_from_sources_with(modules, parse_module_source)
}

/// Parse every source module through the bounded-memory incremental pipeline.
///
/// Use this for generated or otherwise large modules. The returned
/// [`ModuleSet`] has the same interface and semantics as
/// [`module_set_from_sources`], so conversion callers only choose an
/// ingestion policy rather than learning a second linking API.
pub fn module_set_from_sources_lazy<'s, I>(modules: I) -> Result<ModuleSet<'s>, ConvertError>
where
    I: IntoIterator<Item = (&'s str, &'s str)>,
{
    module_set_from_sources_with(modules, parse_module_source_lazy)
}

fn module_set_from_sources_with<'s, I>(
    modules: I,
    parse: impl Fn(&str, &str) -> Result<SModule, ConvertError>,
) -> Result<ModuleSet<'s>, ConvertError>
where
    I: IntoIterator<Item = (&'s str, &'s str)>,
{
    with_globals(|| {
        let mut set = ModuleSet::new();
        for (path, source) in modules {
            let ssa = parse(path, source)?;
            // The set borrows the SSA modules; leak them so the returned set
            // is free of the iterator's stack frame. The set (and resulting
            // Module) are used once per compilation and then dropped.
            set.insert(path, Box::leak(Box::new(ssa)))?;
        }
        Ok(set)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lazy_module_ingestion_matches_whole_module_metadata() {
        let source = r#"
            import { twice } from './dep.js';
            function private_helper(x) { return x + 10; }
            export function run(x) { return twice(private_helper(x)) + 1; }
            export default function fallback() { return 0; }
            const retained_top_level_value = 3;
            export { retained_top_level_value };
        "#;
        with_globals(|| {
            let eager = parse_module_source("main.js", source).expect("eager parse");
            let lazy = parse_module_source_lazy("main.js", source).expect("lazy parse");
            assert_eq!(lazy.imports.len(), eager.imports.len());
            assert_eq!(lazy.exports.len(), eager.exports.len());
            assert_eq!(lazy.funcs.len(), eager.funcs.len());
            for name in eager.funcs.keys() {
                assert!(
                    lazy.funcs.contains_key(name),
                    "missing hoisted function {name}"
                );
            }
            assert!(
                lazy.funcs
                    .contains_key(&swc_atoms::Atom::new("private_helper")),
                "ordinary module-scope function declarations must be lazily lowered too"
            );
            assert_eq!(lazy.body.cfg.blocks.len(), eager.body.cfg.blocks.len());
        });
    }
}
