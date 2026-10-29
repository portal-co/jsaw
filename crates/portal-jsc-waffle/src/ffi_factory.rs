// DreamCompat-style FFI factories via the `~ffi` virtual module (DreamComp
// #37). include!d into conv.rs.
//
// `import { ffi, struct, opaque } from "~ffi"` gives source access to the
// hybrid-ABI factory surface (`callback`/`contains` fail closed). The
// factories are call-only compiler intrinsics:
//
// - `ffi(sym, sig)` parses dreamcomp's hybrid signature grammar with the
//   real `dreamcompiler-ffi-sig` crate (byte-identical sigs), declares raw
//   host imports `ffi.{sym}` and `ffi.{sym}_{method}`, and evaluates to a
//   thunk function object. Calling the thunk converts arguments per the
//   sig, calls the raw import, and boxes the result; a `P<rid>` result is
//   wrapped in an object carrying the sig's method table, whose methods
//   receive the wrapper's hidden `\u{1}ffi_handle` as their first host
//   argument.
// - `struct('extern ...')` evaluates to a constructor function object that
//   builds plain objects with the declared fields in order. Only `extern`
//   (host-pinned layout) structs are supported; jsaw never computes host
//   layouts.
// - `opaque(name)` evaluates to a non-callable marker object carrying the
//   name for diagnostics.
//
// Boundary mapping: `slot` and `P<rid>` cross as `ref` (a full JS value /
// host-opaque handle), `i32` as `i32`, `ptr` as `i64`, `void` as no result.
// All factory arguments must be compile-time string literals; template
// splices fail closed this round.

use dreamcompiler_ffi_sig::{FfiAbiTy, FfiCallableSig, FfiTy};

/// The wasm import module every ffi-factory host symbol lives under.
const FFI_IMPORT_MODULE: &str = "ffi";
/// Hidden own-property carrying the host handle on method wrappers.
const FFI_HANDLE_SLOT: &str = "\u{1}ffi_handle";
/// Hidden own-property carrying the canonical struct declaration.
const FFI_STRUCT_SLOT: &str = "\u{1}ffi_struct";
/// Hidden own-property carrying the opaque type name.
const FFI_OPAQUE_SLOT: &str = "\u{1}ffi_opaque";
/// The canonical splice token for a factory result — used both for
/// compile-time template resolution and as the runtime `toString` of the
/// factory value, so the two always agree (#42). Tokens never contain ','
/// (the struct field separator).
fn ffi_splice_token(kind: &str, text: &str) -> String {
    let text = text.replace(',', ";");
    match kind {
        "struct" => format!("struct<{text}>"),
        "callback" => format!("callback<{text}>"),
        "opaque" => format!("opaque:{text}"),
        "contains" => "contains".to_string(),
        _ => unreachable!("ffi splice token kind"),
    }
}

/// Hidden own-property carrying the callback declaration ordinal.
const FFI_CALLBACK_SLOT: &str = "\u{1}ffi_callback";
/// Hidden own-property carrying the bound callback handler function.
const FFI_HANDLER_SLOT: &str = "\u{1}ffi_handler";
/// Hidden own-property carrying the contained value (contains() markers).
const FFI_CONTAINS_SLOT: &str = "\u{1}ffi_contains";

/// A parsed `ffi(sym, sig)` declaration discovered by the prescan.
pub(crate) struct FfiThunkPlan {
    /// The host symbol (`ffi.{sym}` import field).
    sym: String,
    /// The parsed hybrid signature.
    sig: FfiCallableSig,
}

/// Everything the `~ffi` prescan collects before lowering (DreamComp #42).
pub(crate) struct FfiPrescan {
    /// `ffi(sym, sig)` thunk plans by host symbol.
    pub(crate) thunks: BTreeMap<String, FfiThunkPlan>,
    /// `callback(sig)` declarations by canonical signature text.
    pub(crate) callbacks: BTreeMap<String, FfiCallableSig>,
}

impl FfiPrescan {
    /// The PIT interface catalog: rid -> (declaring sym, methods), from every
    /// thunk whose top-level result is `P<rid>`.
    pub(crate) fn pit_ifaces(&self) -> BTreeMap<[u8; 32], (String, Vec<FfiMethodSigRef>)> {
        let mut out = BTreeMap::new();
        for plan in self.thunks.values() {
            if let Some(rid) = plan.sig.return_pit_rid() {
                out.insert(rid, (plan.sym.clone(), plan.sig.methods.clone()));
            }
        }
        out
    }
}

/// Map one FFI type to its boundary type (None = void, result-position only).
fn ffi_boundary_type(ty: &FfiTy, what: &str) -> Result<Option<WasmBoundaryType>, ConvertError> {
    match ty {
        FfiTy::Abi(FfiAbiTy::Slot) => Ok(Some(WasmBoundaryType::Ref)),
        FfiTy::Abi(FfiAbiTy::Ptr) => Ok(Some(WasmBoundaryType::I64)),
        FfiTy::Abi(FfiAbiTy::I32) => Ok(Some(WasmBoundaryType::I32)),
        FfiTy::Abi(FfiAbiTy::Void) if what == "result" => Ok(None),
        FfiTy::Abi(FfiAbiTy::Void) => Err(ConvertError::invalid(
            "ffi signature: `void` is only valid in result position"
        )),
        FfiTy::Pit(_) => Ok(Some(WasmBoundaryType::Ref)),
    }
}

fn ffi_boundary_signature(
    sym: &str,
    args: &[FfiTy],
    ret: &FfiTy,
) -> Result<WasmBoundarySignature, ConvertError> {
    let mut params = Vec::with_capacity(args.len());
    for arg in args {
        params.push(
            ffi_boundary_type(arg, "parameter")?
                .expect("parameter position never yields void"),
        );
    }
    let result = ffi_boundary_type(ret, "result").map_err(|e| {
        ConvertError::invalid(format!("ffi signature for {sym:?}: {}", e.message))
    })?;
    Ok(WasmBoundarySignature { params, result })
}

/// Walk every function in the module set, find `ffi(sym, sig)` calls on
/// `~ffi` bindings, parse their signatures, and return the thunk plans.
/// Runs before any lowering so raw imports can be declared import-first.
pub(crate) fn prescan_ffi(
    set: &ModuleSet<'_>,
    import_tables: &BTreeMap<String, BTreeMap<Ident, ImportTarget<'_>>>,
) -> Result<FfiPrescan, ConvertError> {
    let mut plans: BTreeMap<String, FfiThunkPlan> = BTreeMap::new();
    let mut callbacks: BTreeMap<String, FfiCallableSig> = BTreeMap::new();
    for path in set.keys() {
        let module = set.get(path)?;
        let Some(table) = import_tables.get(path.as_str()) else {
            continue;
        };
        let mut funcs: Vec<&SFunc> = vec![&module.body];
        funcs.extend(module.funcs.values());
        for func in funcs {
            // Tail calls (`return ffi(...)`) are terminators, not items:
            // collect (callee, args) pairs from both shapes.
            let mut call_sites: Vec<(&TCallee<SValueId>, &[portal_jsc_swc_tac::SpreadOr<SValueId>])> =
                Vec::new();
            for (_, block) in func.cfg.blocks.iter() {
                if let TTerm::Tail { callee, args } = &block.postcedent.term {
                    call_sites.push((callee, args));
                }
            }
            for (_, value) in func.cfg.values.iter() {
                if let SValue::Item {
                    item: Item::Call { callee, args },
                    ..
                } = &value.value
                {
                    call_sites.push((callee, args));
                }
            }
            for (callee, args) in call_sites {
                let TCallee::Val(callee) = callee else {
                    continue;
                };
                let Some(SValue::LoadId(id)) =
                    func.cfg.values.get(*callee).map(|v| &v.value)
                else {
                    continue;
                };
                let Some(ImportTarget::Intrinsic { module: m, name }) = table.get(id) else {
                    continue;
                };
                if m != "~ffi" {
                    continue;
                }
                // Literal extraction (compile-time-only rule).
                let literal = |index: usize| -> Result<String, ConvertError> {
                    let Some(arg) = args.get(index) else {
                        return Err(ConvertError::invalid(format!(
                            "~ffi {name}() takes {name}-specific string arguments"
                        )));
                    };
                    match func.cfg.values.get(args[index].value).map(|v| &v.value) {
                        Some(SValue::Item {
                            item: Item::Lit { lit: Lit::Str(s) },
                            ..
                        }) => Ok(s.value.to_string_lossy().into_owned()),
                        _ => Err(ConvertError::invalid(format!(
                            "~ffi {name}() arguments must be compile-time string literals"
                        ))),
                    }
                };
                match name.as_str() {
                    "ffi" => {
                        let sym = literal(0)?;
                        let sig_src = literal(1)?;
                        let sig =
                            dreamcompiler_ffi_sig::parse_sig(&sig_src).map_err(|e| {
                                ConvertError::invalid(format!(
                                    "ffi({sym:?}, …) signature parse error: {e}"
                                ))
                            })?;
                        match plans.entry(sym.clone()) {
                            std::collections::btree_map::Entry::Vacant(entry) => {
                                entry.insert(FfiThunkPlan { sym, sig });
                            }
                            std::collections::btree_map::Entry::Occupied(entry) => {
                                if entry.get().sig != sig {
                                    return Err(ConvertError::invalid(format!(
                                        "ffi({sym:?}, …) is declared with two different \
                                         signatures"
                                    )));
                                }
                            }
                        }
                    }
                    // struct/opaque build runtime values; no imports needed.
                    // struct sigs may be template literals with factory-result
                    // splices (#42); the lowering does the full resolution.
                    "struct" | "opaque" => {
                        if literal(0).is_err() {
                            validate_tpl_splices(func, args[0].value)?;
                        }
                    }
                    // callback(sig): parse + collect (export adapters are
                    // declared at lowering; imports are unaffected).
                    "callback" => {
                        let sig_src = literal(0)?;
                        let sig =
                            dreamcompiler_ffi_sig::parse_sig(&sig_src).map_err(|e| {
                                ConvertError::invalid(format!(
                                    "callback({sig_src:?}) signature parse error: {e}"
                                ))
                            })?;
                        match callbacks.entry(sig_src.clone()) {
                            std::collections::btree_map::Entry::Vacant(entry) => {
                                entry.insert(sig);
                            }
                            std::collections::btree_map::Entry::Occupied(entry) => {
                                if entry.get() != &sig {
                                    return Err(ConvertError::invalid(
                                        "callback() declared twice with equal text but different                                          parses"
                                            .to_string(),
                                    ));
                                }
                            }
                        }
                    }
                    // contains(value): a runtime value, not a literal.
                    "contains" => {
                        if args.len() != 1 {
                            return Err(ConvertError::invalid(
                                "~ffi contains() takes exactly one value argument".to_string(),
                            ));
                        }
                    }
                    other => {
                        return Err(ConvertError::invalid(format!(
                            "unknown ~ffi intrinsic {other:?}"
                        )));
                    }
                }
            }
        }
    }
    Ok(FfiPrescan {
        thunks: plans,
        callbacks,
    })
}

/// Validate that a template-literal signature's splices each resolve to a
/// `~ffi` factory call in the same function (DreamComp #42 structural
/// identity rule, same-function-body per the public spec).
fn validate_tpl_splices(
    func: &SFunc,
    tpl_arg: SValueId,
) -> Result<(), ConvertError> {
    let Some(SValue::Item {
        item: Item::Tpl { exprs, .. },
        ..
    }) = func.cfg.values.get(tpl_arg).map(|v| &v.value)
    else {
        return Err(ConvertError::invalid(
            "~ffi factory signature must be a compile-time string or template literal"
                .to_string(),
        ));
    };
    for expr in exprs {
        // Chase alias chains (`const T = contains(v); … ${T} …`).
        let mut cur = *expr;
        let callee = loop {
            match func.cfg.values.get(cur).map(|v| &v.value) {
                Some(SValue::Item {
                    item: Item::Call { callee, .. },
                    ..
                }) => break callee,
                Some(SValue::Item {
                    item: Item::Just { id },
                    ..
                }) => cur = *id,
                other => {
                    if std::env::var("MBTRACE_FFI").is_ok() {
                        eprintln!("prescan splice item: {other:?}");
                    }
                    return Err(ConvertError::invalid(
                        "~ffi template splices must be factory results (prescan)".to_string(),
                    ));
                }
            }
        };
        let TCallee::Val(callee) = callee else {
            return Err(ConvertError::invalid(
                "~ffi template splices must call a ~ffi factory directly".to_string(),
            ));
        };
        match func.cfg.values.get(*callee).map(|v| &v.value) {
            Some(SValue::LoadId(_)) => {}
            _ => {
                return Err(ConvertError::invalid(
                    "~ffi template splices must call a ~ffi factory directly".to_string(),
                ));
            }
        }
    }
    Ok(())
}

/// Declare the raw host imports for every prescanned ffi thunk, import-first
/// (core Wasm ordering). Returns the (module, field) → func map.
pub(crate) fn declare_ffi_host_imports(
    module: &mut Module<'_>,
    plans: &BTreeMap<String, FfiThunkPlan>,
) -> Result<BTreeMap<(String, String), Func>, ConvertError> {
    let mut out = BTreeMap::new();
    for plan in plans.values() {
        let thunk_sig = ffi_boundary_signature(&plan.sym, &plan.sig.args, &plan.sig.ret)?;
        declare_ffi_import(module, &mut out, &plan.sym, &thunk_sig)?;
        for method in &plan.sig.methods {
            let field = format!("{}_{}", plan.sym, method.name);
            // The host receives the wrapper's handle as the first argument,
            // then the declared parameters.
            let mut params = vec![WasmBoundaryType::Ref];
            for arg in &method.args {
                params.push(
                    ffi_boundary_type(arg, "parameter")?
                        .expect("parameter position never yields void"),
                );
            }
            let result = ffi_boundary_type(&method.ret, "result")?;
            declare_ffi_import(
                module,
                &mut out,
                &field,
                &WasmBoundarySignature { params, result },
            )?;
        }
    }
    Ok(out)
}

fn declare_ffi_import(
    module: &mut Module<'_>,
    out: &mut BTreeMap<(String, String), Func>,
    field: &str,
    signature: &WasmBoundarySignature,
) -> Result<(), ConvertError> {
    let key = (FFI_IMPORT_MODULE.to_string(), field.to_string());
    if out.contains_key(&key) {
        return Ok(());
    }
    let signature_idx = module.signatures.push(SignatureData::Func {
        params: signature
            .params
            .iter()
            .copied()
            .map(wasm_boundary_type)
            .collect(),
        returns: signature
            .result
            .map(wasm_boundary_type)
            .into_iter()
            .collect(),
        shared: false,
    });
    let func = module.funcs.push(FuncDecl::Import(signature_idx, field.to_string()));
    module.imports.push(Import {
        module: FFI_IMPORT_MODULE.to_string(),
        name: field.to_string(),
        kind: ImportKind::Func(func),
    });
    out.insert(key, func);
    Ok(())
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {

    /// The `~ffi` call interception, reached from `lower_item`'s intrinsic
    /// branch before ordinary call dispatch.
    fn ffi_intrinsic_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        this: LowerValue,
        values: &ContinuationValues,
        name: &str,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        match name {
            "ffi" => self.ffi_factory_call(body, block, context, values, args),
            "struct" => self.struct_factory_call(body, block, context, values, args),
            "opaque" => self.opaque_factory_call(body, block, context, values, args),
            "callback" => self.callback_factory_call(body, block, context, values, args),
            "contains" => self.contains_factory_call(body, block, context, values, args),
            other => Err(ConvertError::invalid(format!(
                "unknown ~ffi intrinsic {other:?}"
            ))),
        }
        .map(|v| {
            let _ = this;
            v
        })
    }

    fn literal_arg(
        &self,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
        index: usize,
        what: &str,
    ) -> Result<String, ConvertError> {
        let Some(arg) = args.get(index) else {
            return Err(ConvertError::invalid(format!(
                "~ffi {what}: missing argument {index}"
            )));
        };
        match values.get(&arg.value) {
            Some(LowerValue::String { key, .. }) => Ok(key.clone()),
            _ => Err(ConvertError::invalid(format!(
                "~ffi {what}: argument {index} must be a compile-time string literal"
            ))),
        }
    }

    // -- ffi(sym, sig) ---------------------------------------------------------

    fn ffi_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let sym = self.literal_arg(values, args, 0, "ffi sym")?;
        let _sig_src = self.literal_arg(values, args, 1, "ffi sig")?;
        if !self.ffi_thunks.contains_key(&sym) {
            return Err(ConvertError::invalid(format!(
                "ffi({sym:?}, …) was not found by the ffi prescan (the call must be visible in \
                 the module set before lowering)"
            )));
        }
        let native = self.ffi_thunk_native(&sym)?;
        let thunk = self.function_object_for_native(body, block, context, native)?;
        Ok(vec![(block, thunk)])
    }

    /// The thunk native for one `ffi(sym, …)` declaration (adapter ABI:
    /// `(context, this, arguments, formals…) -> value`).
    fn ffi_thunk_native(&mut self, sym: &str) -> Result<Func, ConvertError> {
        if let Some(f) = self.ffi_adapters.get(sym) {
            return Ok(*f);
        }
        let (sig_args, sig_ret, methods) = {
            let plan = self.ffi_thunks.get(sym).expect("thunk plan exists");
            (
                plan.sig.args.clone(),
                plan.sig.ret,
                plan.sig.methods.clone(),
            )
        };
        let boundary = ffi_boundary_signature(sym, &sig_args, &sig_ret)?;
        let raw = self.ffi_host_imports[&(FFI_IMPORT_MODULE.to_string(), sym.to_string())];

        let native_sig = self.module.signatures.push(SignatureData::Func {
            params: std::iter::once(self.repr.object_ty())
                .chain(std::iter::once(self.repr.value))
                .chain(std::iter::once(self.repr.arguments_ty()))
                .chain(std::iter::repeat_n(self.repr.value, boundary.params.len()))
                .collect(),
            returns: vec![self.repr.value],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, native_sig);
        let block = body.entry;
        let formals = body.blocks[block].params[3..]
            .iter()
            .map(|(_, value)| *value)
            .collect::<Vec<_>>();
        let mut raw_args = Vec::with_capacity(boundary.params.len());
        for (value, ty) in formals.into_iter().zip(boundary.params.iter().copied()) {
            let lv = LowerValue::Wasm {
                value,
                kind: ValueKind::Reference,
            };
            raw_args.push(self.ffi_convert_in(&mut body, block, &lv, ty)?);
        }
        let result_types = boundary
            .result
            .map(wasm_boundary_type)
            .into_iter()
            .collect::<Vec<_>>();
        let result = body.add_op(
            block,
            Operator::Call {
                function_index: raw,
            },
            &raw_args,
            &result_types,
        );
        // Box the result; only a P<…> result becomes a method-table wrapper.
        let wrap = if sig_ret.is_pit() {
            Some((sym, methods.as_slice()))
        } else {
            None
        };
        let (block, out) =
            self.ffi_convert_out(&mut body, block, result, boundary.result, wrap)?;
        let (out_v, _) = out.wasm()?;
        body.set_terminator(block, Terminator::Return { values: vec![out_v] });
        let native = self.module.funcs.push(FuncDecl::Body(
            native_sig,
            format!("js_ffi_thunk_{sym}"),
            body,
        ));
        let adapter = self.make_adapter(native, boundary.params.len())?;
        self.declare_function_reference(adapter);
        self.ffi_adapters.insert(sym.to_string(), adapter);
        Ok(adapter)
    }

    /// The method native for one `{sym}_{method}` (adapter ABI): prepends
    /// the receiver wrapper's hidden handle to the host call.
    fn ffi_method_native(
        &mut self,
        sym: &str,
        method: &FfiMethodSigRef,
    ) -> Result<Func, ConvertError> {
        let key = (sym.to_string(), method.name.clone());
        if let Some(f) = self.ffi_method_adapters.get(&key) {
            return Ok(*f);
        }
        let mut param_tys = Vec::with_capacity(method.args.len());
        for arg in &method.args {
            param_tys.push(
                ffi_boundary_type(arg, "parameter")?.expect("parameter position never yields void"),
            );
        }
        let result_ty = ffi_boundary_type(&method.ret, "result")?;
        let raw = self.ffi_host_imports[&(
            FFI_IMPORT_MODULE.to_string(),
            format!("{sym}_{}", method.name),
        )];

        let native_sig = self.module.signatures.push(SignatureData::Func {
            params: std::iter::once(self.repr.object_ty())
                .chain(std::iter::once(self.repr.value))
                .chain(std::iter::once(self.repr.arguments_ty()))
                .chain(std::iter::repeat_n(self.repr.value, param_tys.len()))
                .collect(),
            returns: vec![self.repr.value],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, native_sig);
        let block = body.entry;
        let this = body.blocks[block].params[1].1;
        let formals = body.blocks[block].params[3..]
            .iter()
            .map(|(_, value)| *value)
            .collect::<Vec<_>>();
        // Receiver handle: the wrapper's hidden slot (passthrough if absent).
        let this_lv = LowerValue::Wasm {
            value: this,
            kind: ValueKind::Reference,
        };
        let (mut block, handle) = {
            let (b, slot) = self.get_own_property_value(&mut body, block, &this_lv, FFI_HANDLE_SLOT)?;
            (b, slot)
        };
        let mut raw_args = vec![handle.wasm()?.0];
        for (value, ty) in formals.into_iter().zip(param_tys.iter().copied()) {
            let lv = LowerValue::Wasm {
                value,
                kind: ValueKind::Reference,
            };
            raw_args.push(self.ffi_convert_in(&mut body, block, &lv, ty)?);
        }
        let result_types = result_ty.map(wasm_boundary_type).into_iter().collect::<Vec<_>>();
        let result = body.add_op(
            block,
            Operator::Call {
                function_index: raw,
            },
            &raw_args,
            &result_types,
        );
        // A method returning an interface type wraps the result with the
        // DECLARING thunk's method table (the PIT catalog, #42).
        let wrap = if let Some(rid) = method.ret.pit_rid() {
            let Some((sym2, methods2)) = self.pit_ifaces.get(&rid).cloned() else {
                return Err(ConvertError::invalid(format!(
                    "ffi method {}::{} returns P<{}>, which no ffi(…) declaration in this                      module set returns at top level (no method table is registered for it)",
                    sym,
                    method.name,
                    dreamcompiler_ffi_sig::rid_hex(&rid)
                )));
            };
            Some((sym2, methods2))
        } else {
            None
        };
        let (block, out) = self.ffi_convert_out(
            &mut body,
            block,
            result,
            result_ty,
            wrap.as_ref().map(|(s, m)| (s.as_str(), m.as_slice())),
        )?;
        let (out_v, _) = out.wasm()?;
        body.set_terminator(block, Terminator::Return { values: vec![out_v] });
        let native = self.module.funcs.push(FuncDecl::Body(
            native_sig,
            format!("js_ffi_method_{sym}_{}", method.name),
            body,
        ));
        let adapter = self.make_adapter(native, param_tys.len())?;
        self.declare_function_reference(adapter);
        self.ffi_method_adapters.insert(key, adapter);
        Ok(adapter)
    }

    /// JS → host conversion for one argument.
    fn ffi_convert_in(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        value: &LowerValue,
        ty: WasmBoundaryType,
    ) -> Result<Value, ConvertError> {
        Ok(match ty {
            WasmBoundaryType::I32 => self.as_i32(body, block, value)?,
            WasmBoundaryType::I64 => self.as_i64(body, block, value)?,
            WasmBoundaryType::F32 => {
                let number = self.as_f64(body, block, value)?;
                body.add_op(block, Operator::F32DemoteF64, &[number], &[Type::F32])
            }
            WasmBoundaryType::F64 => self.as_f64(body, block, value)?,
            WasmBoundaryType::Ref => value.wasm()?.0,
        })
    }

    /// Host → JS conversion for one result, with `P<…>` wrapping when the
    /// result carries a method table.
    fn ffi_convert_out(
        &mut self,
        body: &mut FunctionBody,
        mut block: Block,
        result: Value,
        ty: Option<WasmBoundaryType>,
        methods: Option<(&str, &[FfiMethodSigRef])>,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let value = match ty {
            None => self.undef(body, block),
            Some(WasmBoundaryType::I32) => LowerValue::Wasm {
                value: self.box_value(
                    body,
                    block,
                    &LowerValue::Wasm {
                        value: result,
                        kind: ValueKind::Integer,
                    },
                )?,
                kind: ValueKind::Reference,
            },
            Some(WasmBoundaryType::I64) => LowerValue::Wasm {
                value: self.box_value(
                    body,
                    block,
                    &LowerValue::Wasm {
                        value: result,
                        kind: ValueKind::BigInt,
                    },
                )?,
                kind: ValueKind::Reference,
            },
            Some(WasmBoundaryType::F32) => {
                let promoted = body.add_op(block, Operator::F64PromoteF32, &[result], &[Type::F64]);
                LowerValue::Wasm {
                    value: self.box_value(
                        body,
                        block,
                        &LowerValue::Wasm {
                            value: promoted,
                            kind: ValueKind::Number,
                        },
                    )?,
                    kind: ValueKind::Number,
                }
            }
            Some(WasmBoundaryType::F64) => LowerValue::Wasm {
                value: result,
                kind: ValueKind::Number,
            },
            Some(WasmBoundaryType::Ref) => LowerValue::Wasm {
                value: result,
                kind: ValueKind::Reference,
            },
        };
        let Some((sym, methods)) = methods else {
            return Ok((block, value));
        };
        // Wrap: fresh object with the hidden handle and one function object
        // per declared method.
        let wrapper = self.new_object(body, block)?;
        block = self.set_static_property_value_raw(body, block, &wrapper, FFI_HANDLE_SLOT, &value)?;
        for method in methods {
            let native = self.ffi_method_native(sym, method)?;
            let ctx = self.new_object(body, block)?.wasm()?.0;
            let func_obj = self.function_object_for_native(body, block, ctx, native)?;
            block = self.set_static_property_value_raw(
                body,
                block,
                &wrapper,
                &method.name,
                &func_obj,
            )?;
        }
        Ok((block, wrapper))
    }

    // -- callback(sig) -----------------------------------------------------------

    fn callback_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let sig_src = self.literal_arg(values, args, 0, "callback sig")?;
        if !self.ffi_callbacks.contains_key(&sig_src) {
            return Err(ConvertError::invalid(format!(
                "callback({sig_src:?}) was not found by the ffi prescan (the call must be                  visible in the module set before lowering)"
            )));
        }
        let native = self.callback_ctor_native(&sig_src)?;
        // The reverse-boundary export adapter is declared alongside so the
        // host can call bound wrappers (`js_ffi_cb_<ordinal>`).
        self.callback_export_native(&sig_src)?;
        let ctor = self.function_object_for_native(body, block, context, native)?;
        let token = ffi_splice_token("callback", &sig_src);
        let block = self.ffi_attach_token_tostring(body, block, context, &ctor, &token)?;
        Ok(vec![(block, ctor)])
    }

    /// A zero-arg native returning a constant string, cached per text; used
    /// to give factory results an own `toString` so template-literal sigs
    /// evaluate to the canonical splice token at runtime too (#42).
    fn ffi_const_string_native(&mut self, text: &str) -> Result<Func, ConvertError> {
        let native_sig = self.module.signatures.push(SignatureData::Func {
            params: vec![
                self.repr.object_ty(),
                self.repr.value,
                self.repr.arguments_ty(),
            ],
            returns: vec![self.repr.value],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, native_sig);
        let block = body.entry;
        let text_owned = text.to_string();
        let sv = self.new_string(&mut body, block, text_owned.as_bytes())?;
        let out = LowerValue::String {
            value: sv,
            key: text_owned.clone(),
            bytes: text_owned.into_bytes(),
        };
        let (v, _) = out.wasm()?;
        body.set_terminator(block, Terminator::Return { values: vec![v] });
        let native = self.module.funcs.push(FuncDecl::Body(
            native_sig,
            format!("js_ffi_tostring_{}", self.module.funcs.len()),
            body,
        ));
        let adapter = self.make_adapter(native, 0)?;
        self.declare_function_reference(adapter);
        Ok(adapter)
    }

    /// Attach an own `toString` returning `token` to a factory result.
    fn ffi_attach_token_tostring(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        value: &LowerValue,
        token: &str,
    ) -> Result<Block, ConvertError> {
        let native = self.ffi_const_string_native(token)?;
        let ts = self.function_object_for_native(body, block, context, native)?;
        self.set_static_property_value_raw(body, block, value, "toString", &ts)
    }

    /// The callback declaration ordinal (deterministic: sorted by canonical
    /// signature text).
    fn callback_ordinal(&self, sig_src: &str) -> usize {
        self.ffi_callbacks
            .keys()
            .position(|k| k == sig_src)
            .expect("callback declaration")
    }

    /// The constructor native for one `callback(sig)`: takes the handler
    /// function and builds the wrapper object that crosses to the host.
    fn callback_ctor_native(&mut self, sig_src: &str) -> Result<Func, ConvertError> {
        if let Some(f) = self.ffi_callback_ctors.get(sig_src) {
            return Ok(*f);
        }
        let ordinal = self.callback_ordinal(sig_src);
        let native_sig = self.module.signatures.push(SignatureData::Func {
            params: std::iter::once(self.repr.object_ty())
                .chain(std::iter::once(self.repr.value))
                .chain(std::iter::once(self.repr.arguments_ty()))
                .chain(std::iter::once(self.repr.value))
                .collect(),
            returns: vec![self.repr.value],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, native_sig);
        let block = body.entry;
        let handler = body.blocks[block].params[3].1;
        let obj = self.new_object(&mut body, block)?;
        let ordinal_const = body.add_op(
            block,
            Operator::I32Const { value: ordinal as u32 },
            &[],
            &[Type::I32],
        );
        let ordinal_boxed = LowerValue::Wasm {
            value: self.box_value(
                &mut body,
                block,
                &LowerValue::Wasm {
                    value: ordinal_const,
                    kind: ValueKind::Integer,
                },
            )?,
            kind: ValueKind::Reference,
        };
        let mut block = self.set_static_property_value_raw(
            &mut body,
            block,
            &obj,
            FFI_CALLBACK_SLOT,
            &ordinal_boxed,
        )?;
        let handler_lv = LowerValue::Wasm {
            value: handler,
            kind: ValueKind::Reference,
        };
        block =
            self.set_static_property_value_raw(&mut body, block, &obj, FFI_HANDLER_SLOT, &handler_lv)?;
        let (obj_v, _) = obj.wasm()?;
        body.set_terminator(block, Terminator::Return { values: vec![obj_v] });
        let native = self.module.funcs.push(FuncDecl::Body(
            native_sig,
            format!("js_ffi_cb_ctor_{ordinal}"),
            body,
        ));
        let adapter = self.make_adapter(native, 1)?;
        self.declare_function_reference(adapter);
        self.ffi_callback_ctors.insert(sig_src.to_string(), adapter);
        Ok(adapter)
    }

    /// The exported reverse-boundary adapter for one `callback(sig)`
    /// (`js_ffi_cb_<ordinal>`): boundary signature `(ref wrapper, args…)
    /// -> result` — loads the bound handler from the wrapper, converts
    /// arguments into JS values, calls through the ordinary function-call
    /// path, and converts the result back. A non-`Return` exit from the
    /// handler escapes as a wasm exception (matching DreamComp's first cut:
    /// no cross-boundary propagation).
    fn callback_export_native(&mut self, sig_src: &str) -> Result<Func, ConvertError> {
        if let Some(f) = self.ffi_callback_exports.get(sig_src) {
            return Ok(*f);
        }
        let ordinal = self.callback_ordinal(sig_src);
        let sig = self
            .ffi_callbacks
            .get(sig_src)
            .expect("callback declaration")
            .clone();
        let boundary = ffi_boundary_signature("callback", &sig.args, &sig.ret)?;
        let native_sig = self.module.signatures.push(SignatureData::Func {
            params: std::iter::once(self.repr.object_ty())
                .chain(boundary.params.iter().copied().map(wasm_boundary_type))
                .collect(),
            returns: boundary
                .result
                .map(wasm_boundary_type)
                .into_iter()
                .collect(),
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, native_sig);
        let block = body.entry;
        let wrapper = body.blocks[block].params[0].1;
        let formals: Vec<Value> = body.blocks[block].params[1..]
            .iter()
            .map(|(_, value)| *value)
            .collect();
        // The bound handler from the wrapper's hidden slot.
        let wrapper_lv = LowerValue::Wasm {
            value: wrapper,
            kind: ValueKind::Reference,
        };
        let (mut block, handler) =
            self.get_own_property_value(&mut body, block, &wrapper_lv, FFI_HANDLER_SLOT)?;
        // Boundary args -> JS values.
        let mut js_args = Vec::with_capacity(formals.len());
        for (value, ty) in formals.into_iter().zip(boundary.params.iter().copied()) {
            let (b, lv) = self.ffi_convert_out(&mut body, block, value, Some(ty), None)?;
            block = b;
            js_args.push(lv.wasm()?.0);
        }
        let array = body.add_op(
            block,
            Operator::ArrayNewFixed {
                sig: self.repr.arguments,
                num: js_args.len(),
            },
            &js_args,
            &[self.repr.arguments_ty()],
        );
        let receiver = self.undef(&mut body, block);
        let (context, this, code, _arrow) =
            self.callable_parts(&mut body, block, &handler, receiver)?;
        let result = body.add_op(
            block,
            Operator::CallRef {
                sig_index: self.repr.adapter,
            },
            &[context, this, array, code],
            &[self.repr.value],
        );
        // JS result -> boundary value.
        let result_lv = LowerValue::Wasm {
            value: result,
            kind: ValueKind::Reference,
        };
        let mut returns = Vec::new();
        if let Some(ty) = boundary.result {
            returns.push(self.ffi_convert_in(&mut body, block, &result_lv, ty)?);
        }
        body.set_terminator(block, Terminator::Return { values: returns });
        let native = self.module.funcs.push(FuncDecl::Body(
            native_sig,
            format!("js_ffi_cb_{ordinal}"),
            body,
        ));
        self.module.exports.push(Export {
            name: format!("js_ffi_cb_{ordinal}"),
            kind: ExportKind::Func(native),
        });
        self.ffi_callback_exports
            .insert(sig_src.to_string(), native);
        Ok(native)
    }

    // -- contains(value) ---------------------------------------------------------

    fn contains_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let Some(arg) = args.first() else {
            return Err(ConvertError::invalid(
                "~ffi contains() takes exactly one value argument"
            ));
        };
        // The value is a real runtime argument (already evaluated); the
        // marker object carries it so it stays alive and observable. In
        // jsaw's value model every value is exactly one word, so a
        // `contains` field is an ordinary struct field (see
        // docs/plan-ffi-42.md).
        let value = values.get(&arg.value).cloned().ok_or_else(|| {
            ConvertError::invalid("~ffi contains() argument could not be evaluated")
        })?;
        let obj = self.new_object(body, block)?;
        let block =
            self.set_static_property_value_raw(body, block, &obj, FFI_CONTAINS_SLOT, &value)?;
        let token = ffi_splice_token("contains", "");
        let block = self.ffi_attach_token_tostring(body, block, context, &obj, &token)?;
        Ok(vec![(block, obj)])
    }

    // -- struct(sig) -------------------------------------------------------------

    fn struct_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let sig_src = match self.literal_arg(values, args, 0, "struct sig") {
            Ok(s) => s,
            Err(e) => {
                // Template-literal sig with factory-result splices (#42).
                let Some(arg) = args.first() else { return Err(e) };
                self.resolve_tpl_sig(arg.value)?
            }
        };
        let decl = parse_struct_sig(&sig_src)?;
        let native = self.struct_ctor_native(&decl)?;
        let ctor = self.function_object_for_native(body, block, context, native)?;
        let token = ffi_splice_token("struct", &decl.canonical);
        let block = self.ffi_attach_token_tostring(body, block, context, &ctor, &token)?;
        Ok(vec![(block, ctor)])
    }

    /// The constructor native for one struct declaration: builds a plain
    /// object with the declared fields from positional arguments.
    fn struct_ctor_native(&mut self, decl: &StructDecl) -> Result<Func, ConvertError> {
        if let Some(f) = self.ffi_struct_ctors.get(&decl.canonical) {
            return Ok(*f);
        }
        let native_sig = self.module.signatures.push(SignatureData::Func {
            params: std::iter::once(self.repr.object_ty())
                .chain(std::iter::once(self.repr.value))
                .chain(std::iter::once(self.repr.arguments_ty()))
                .chain(std::iter::repeat_n(self.repr.value, decl.fields.len()))
                .collect(),
            returns: vec![self.repr.value],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, native_sig);
        let block = body.entry;
        let formals = body.blocks[block].params[3..]
            .iter()
            .map(|(_, value)| *value)
            .collect::<Vec<_>>();
        let obj = self.new_object(&mut body, block)?;
        let mut block = block;
        for (i, (field, _ty)) in decl.fields.iter().enumerate() {
            let lv = LowerValue::Wasm {
                value: formals[i],
                kind: ValueKind::Reference,
            };
            block = self.set_static_property_value_raw(&mut body, block, &obj, field, &lv)?;
        }
        // Identity marker: the canonical declaration text.
        let marker_text = decl.canonical.clone();
        let marker = LowerValue::String {
            value: self.new_string(&mut body, block, marker_text.as_bytes())?,
            key: marker_text.clone(),
            bytes: marker_text.into_bytes(),
        };
        block = self.set_static_property_value_raw(&mut body, block, &obj, FFI_STRUCT_SLOT, &marker)?;
        let (obj_v, _) = obj.wasm()?;
        body.set_terminator(block, Terminator::Return { values: vec![obj_v] });
        let native = self.module.funcs.push(FuncDecl::Body(
            native_sig,
            format!("js_ffi_struct_{}", self.module.funcs.len()),
            body,
        ));
        let adapter = self.make_adapter(native, decl.fields.len())?;
        self.declare_function_reference(adapter);
        self.ffi_struct_ctors.insert(decl.canonical.clone(), adapter);
        Ok(adapter)
    }

    /// Resolve a template-literal signature at lowering time (DreamComp
    /// #42): each splice must be a `~ffi` factory call whose declaration we
    /// substitute as a canonical token. The runtime string the template
    /// builds is discarded; splice expressions still evaluate normally.
    fn resolve_tpl_sig(&mut self, sig_value: SValueId) -> Result<String, ConvertError> {
        let source = self.lowering_function.expect("factory call in a function");
        let Some(SValue::Item {
            item: Item::Tpl { quasis, exprs },
            ..
        }) = source.cfg.values.get(sig_value).map(|v| &v.value)
        else {
            return Err(ConvertError::invalid(
                "~ffi struct() signature must be a compile-time string literal or a template                  literal with factory-result splices"
                    .to_string(),
            ));
        };
        let mut out = quasis[0].to_string();
        for (i, expr) in exprs.iter().enumerate() {
            let token = self.resolve_splice_token(source, *expr)?;
            out.push_str(&token);
            out.push_str(&quasis[i + 1].to_string());
        }
        Ok(out)
    }

    /// The canonical token for one spliced factory result (structural
    /// identity, per the public spec).
    fn resolve_splice_token(
        &mut self,
        source: &SFunc,
        expr: SValueId,
    ) -> Result<String, ConvertError> {
        let fail = || {
            if std::env::var("MBTRACE_FFI").is_ok() {
                eprintln!("splice fail; item = {:?}", source.cfg.values.get(expr).map(|v| &v.value));
            }
            ConvertError::invalid(
                "~ffi template splices must be struct/callback/opaque/contains factory results declared earlier in the same function".to_string(),
            )
        };
        // Chase alias chains (`const Bytes = struct(...); … ${Bytes} …`):
        // the splice may reference the declaration through locals.
        let mut cur = expr;
        let (callee, args) = loop {
            match source.cfg.values.get(cur).map(|v| &v.value) {
                Some(SValue::Item {
                    item: Item::Call { callee, args },
                    ..
                }) => break (callee, args),
                Some(SValue::Item {
                    item: Item::Just { id },
                    ..
                }) => cur = *id,
                _ => return Err(fail()),
            }
        };
        let TCallee::Val(callee) = callee else { return Err(fail()) };
        let Some(SValue::LoadId(id)) = source.cfg.values.get(*callee).map(|v| &v.value) else {
            return Err(fail());
        };
        let Some(ImportTarget::Intrinsic { module, name }) = self
            .import_tables
            .get(&self.current_module)
            .and_then(|table| table.get(id))
            .cloned()
        else {
            return Err(fail());
        };
        if module != "~ffi" {
            return Err(fail());
        }
        let name = name.as_str();
        let mut str_arg = |index: usize| -> Result<String, ConvertError> {
            let Some(arg) = args.get(index) else { return Err(fail()) };
            match source.cfg.values.get(arg.value).map(|v| &v.value) {
                Some(SValue::Item {
                    item: Item::Lit { lit: Lit::Str(s) },
                    ..
                }) => Ok(s.value.to_string_lossy().into_owned()),
                Some(SValue::Item {
                    item: Item::Tpl { .. },
                    ..
                }) if name == "struct" => self.resolve_tpl_sig(arg.value),
                _ => Err(fail()),
            }
        };
        match name {
            "struct" => {
                let sig = str_arg(0)?;
                let decl = parse_struct_sig(&sig)?;
                Ok(ffi_splice_token("struct", &decl.canonical))
            }
            "opaque" => Ok(ffi_splice_token("opaque", &str_arg(0)?)),
            "contains" => Ok(ffi_splice_token("contains", "")),
            "callback" => Ok(ffi_splice_token("callback", &str_arg(0)?)),
            _ => Err(fail()),
        }
    }

    // -- opaque(name) -------------------------------------------------------------

    fn opaque_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let name = self.literal_arg(values, args, 0, "opaque name")?;
        let obj = self.new_object(body, block)?;
        let marker_text = name.clone();
        let marker = LowerValue::String {
            value: self.new_string(body, block, marker_text.as_bytes())?,
            key: marker_text.clone(),
            bytes: marker_text.into_bytes(),
        };
        let block =
            self.set_static_property_value_raw(body, block, &obj, FFI_OPAQUE_SLOT, &marker)?;
        let token = ffi_splice_token("opaque", &name);
        let block = self.ffi_attach_token_tostring(body, block, context, &obj, &token)?;
        Ok(vec![(block, obj)])
    }
}

/// A parsed `struct(sig)` declaration.
pub(crate) struct StructDecl {
    /// Canonical text (dedup key): `extern` followed by `name: type` fields.
    canonical: String,
    fields: Vec<(String, String)>,
}

/// The method signature reference used by the factory (the real type from
/// the sig crate).
pub(crate) type FfiMethodSigRef = dreamcompiler_ffi_sig::FfiMethodSig;

/// Parse the jsaw struct field-list grammar:
/// `extern Name: field: ty, field: ty` or `extern field: ty, …`.
/// Only `extern` (host-pinned layout) structs are supported; field types are
/// recorded for diagnostics and future ABI work — the constructor stores
/// ordinary JS values.
fn parse_struct_sig(src: &str) -> Result<StructDecl, ConvertError> {
    let rest0 = src.trim();
    // `extern` marks a host-pinned layout; without it the struct is
    // jsaw-owned. Both build the same plain-object constructor (jsaw never
    // shares raw memory with the host — see docs/plan-ffi-42.md).
    let (canonical_prefix, rest1) = match rest0.strip_prefix("extern") {
        Some(after) => ("extern", after.trim()),
        None => ("", rest0),
    };
    let rest = rest1;
    // Optional struct name before ':' (used for diagnostics only).
    let mut fields = Vec::new();
    let mut canonical = String::from(canonical_prefix);
    if rest.is_empty() {
        return Err(ConvertError::invalid(
            "struct() signature has no fields (expected `extern x: i32, …` or `x: i32, …`)"
        ));
    }
    for (i, part) in rest.split(',').enumerate() {
        let part = part.trim();
        let Some((name, ty)) = part.split_once(':') else {
            return Err(ConvertError::invalid(format!(
                "struct() field {i} {part:?} is not `name: type`"
            )));
        };
        let name = name.trim();
        let ty = ty.trim();
        if name.is_empty() || ty.is_empty() {
            return Err(ConvertError::invalid(format!(
                "struct() field {i} {part:?} has an empty name or type"
            )));
        }
        // A leading "Name:" segment on the first field is the struct name
        // (PIT-style); it is part of the canonical text but not a field.
        if i == 0 && !fields.is_empty() {
            unreachable!()
        }
        canonical.push_str(&format!(" {name}: {ty},"));
        fields.push((name.to_string(), ty.to_string()));
    }
    Ok(StructDecl { canonical, fields })
}
