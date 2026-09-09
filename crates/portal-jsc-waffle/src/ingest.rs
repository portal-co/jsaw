//! Source ingestion: parse ECMAScript module sources into a [`ModuleSet`]
//! of SSA modules ready for [`convert_modules`](crate::convert_modules).
//!
//! This is the multi-module front half of the compiler pulled out of the
//! e2e harness so both the tests and the `jsaw-wasi-bin` WebAssembly CLI
//! share exactly one parse → CFG → TAC → SSA → `ModuleSet` pipeline. It
//! owns the `GLOBALS` scoped TLS that swc parsing and lowering require.

use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::module::SModule;
use portal_jsc_swc_tac::module::TModule;
use swc_common::{FileName, GLOBALS, Globals, SourceMap, sync::Lrc};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_module};

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
    let cfg = CfgModule::try_from(module)
        .map_err(|error| ConvertError::invalid(format!("CFG lowering of {path:?} failed: {error:?}")))?;
    let tac = TModule::try_from(cfg)
        .map_err(|error| ConvertError::invalid(format!("TAC lowering of {path:?} failed: {error:?}")))?;
    SModule::try_from(tac)
        .map_err(|error| ConvertError::invalid(format!("SSA lowering of {path:?} failed: {error:?}")))
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
    with_globals(|| {
        let parsed: Vec<(&str, SModule)> = modules
            .into_iter()
            .map(|(path, source)| parse_module_source(path, source).map(|ssa| (path, ssa)))
            .collect::<Result<_, _>>()?;
        // The set borrows the SSA modules; leak them so the returned set is
        // free of the iterator's stack frame. The set (and the resulting
        // `Module`) is used once per compilation and then dropped.
        let mut set = ModuleSet::new();
        for (path, ssa) in parsed {
            set.insert(path, Box::leak(Box::new(ssa)))?;
        }
        Ok(set)
    })
}
