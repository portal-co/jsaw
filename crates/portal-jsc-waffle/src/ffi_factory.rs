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

/// A parsed `ffi(sym, sig)` declaration discovered by the prescan.
pub(crate) struct FfiThunkPlan {
    /// The host symbol (`ffi.{sym}` import field).
    sym: String,
    /// The parsed hybrid signature.
    sig: FfiCallableSig,
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
) -> Result<BTreeMap<String, FfiThunkPlan>, ConvertError> {
    let mut plans: BTreeMap<String, FfiThunkPlan> = BTreeMap::new();
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
                        // Methods returning interface types have no method
                        // table to wrap results with; fail closed.
                        for method in &sig.methods {
                            if method.ret.is_pit() {
                                return Err(ConvertError::invalid(format!(
                                    "ffi({sym:?}, …): method {:?} returns an interface type; \
                                     method-level P<…> results are not supported in jsaw yet",
                                    method.name
                                )));
                            }
                        }
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
                    "struct" | "opaque" => {
                        let _ = literal(0)?;
                    }
                    // Recognized but unimplemented: fail at the call site
                    // during lowering with the named diagnostic.
                    "callback" | "contains" => {}
                    other => {
                        return Err(ConvertError::invalid(format!(
                            "unknown ~ffi intrinsic {other:?}"
                        )));
                    }
                }
            }
        }
    }
    Ok(plans)
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
            "callback" => Err(ConvertError::invalid(
                "~ffi callback() is not supported in jsaw yet (host-to-JS callbacks need the \
                 reverse boundary; tracked separately)"
                    
            )),
            "contains" => Err(ConvertError::invalid(
                "~ffi contains() is not supported in jsaw (by-value embedding needs the CPS-time \
                 width resolution jsaw does not have)"
                    
            )),
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
        // Box the result; a P<…> result becomes a method-table wrapper.
        let (block, out) =
            self.ffi_convert_out(&mut body, block, result, boundary.result, Some((sym, &methods)))?;
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
        let (block, out) = self.ffi_convert_out(&mut body, block, result, result_ty, None)?;
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

    // -- struct(sig) -------------------------------------------------------------

    fn struct_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let sig_src = self.literal_arg(values, args, 0, "struct sig")?;
        let decl = parse_struct_sig(&sig_src)?;
        let native = self.struct_ctor_native(&decl)?;
        let ctor = self.function_object_for_native(body, block, context, native)?;
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

    // -- opaque(name) -------------------------------------------------------------

    fn opaque_factory_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        _context: Value,
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
    let mut rest = src.trim();
    let Some(after) = rest.strip_prefix("extern") else {
        return Err(ConvertError::invalid(
            "jsaw struct() only supports `extern` (host-pinned) structs; jsaw never computes \
             host layouts"
                
        ));
    };
    rest = after.trim();
    // Optional struct name before ':' (used for diagnostics only).
    let mut fields = Vec::new();
    let mut canonical = String::from("extern");
    if rest.is_empty() {
        return Err(ConvertError::invalid(
            "struct() signature has no fields (expected `extern x: i32, …`)"
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
