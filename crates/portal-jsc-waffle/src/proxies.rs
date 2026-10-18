// Proxy support (DreamComp #34).
//
// A `Proxy` instance is a distinct WasmGC struct (`repr::proxy`: target,
// handler, revoked flag), so one `ref.test` separates proxied values from
// ordinary ones. The trap-dispatch helpers below are reached only behind
// that test, and only for receivers whose provenance does not rule a proxy
// out (`provably_non_proxy`): fresh literals, function objects with known
// origins, and primitives keep the direct shape/trie path with no added
// code at all, so provably-non-Proxy hot paths are unpessimized.
//
// Covered traps: `get` (property reads), `set` (writes), `has` (the `in`
// operator), `apply` (calls through a proxied function), and `construct`
// (`new` on a proxy), plus revocation (`Proxy.revocable`). A missing trap
// forwards to the target through the ordinary operation. `deleteProperty`,
// `getOwnPropertyDescriptor`, `ownKeys`, and the remaining traps are
// unreachable from jsaw's source operations (no `delete` operator, no
// enumeration primordials), so they cannot be silently skipped — the
// triggering operation itself fails closed upstream.
//
// Deliberate divergences from full JS semantics:
// - Forwarding to a target that is itself a proxy runs the ordinary
//   operation on it (the inner proxy's traps are bypassed) except for
//   `has`, which recurses through the generated `js_chain_has`.
// - Traps that throw while exception handling is enabled are not routed
//   through the exception envelope; the corpus's proxies do not throw.
// - A construct trap's result is used as-is (JS requires an object and
//   throws otherwise).
// - `Proxy(t, h)` called without `new` constructs instead of throwing.

/// The generated Proxy helpers (#34).
#[derive(Clone, Copy)]
struct ProxyHelpers {
    /// `js_native_proxy_new` — the `Proxy` constructor body (adapter ABI).
    new: Func,
    /// `js_native_proxy_revocable` — `Proxy.revocable` (adapter ABI).
    revocable: Func,
    /// `js_native_proxy_revoke` — the returned `revoke` function (adapter ABI).
    revoke: Func,
    /// `js_proxy_get(proxy, key) -> slot` — the `get` trap or a forwarded
    /// chain lookup on the target.
    get: Func,
    /// `js_proxy_set(proxy, key, slot)` — the `set` trap or a forwarded
    /// ordinary set on the target.
    set: Func,
    /// `js_proxy_has(proxy, key) -> i32` — the `has` trap or a forwarded
    /// chain presence walk on the target.
    has: Func,
    /// `js_chain_has(value, key) -> i32` — own-or-proto presence walk.
    chain_has: Func,
    /// `js_value_has(value, key) -> i32` — the `in`-operator entry point:
    /// the `has` trap for a proxy, the presence walk otherwise. Single-block
    /// at the call site; the dispatch lives inside the generated body.
    value_has: Func,
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// Values whose construction rules out the proxy representation: fresh
    /// literals, function objects with a known origin, and primitives. An
    /// opaque reference (a parameter, a call result, a context load) pays
    /// one `ref.test` at the interception points instead.
    fn provably_non_proxy(value: &LowerValue) -> bool {
        match value {
            LowerValue::NumberLiteral { .. }
            | LowerValue::String { .. }
            | LowerValue::FunctionRef { .. }
            | LowerValue::ObjectLiteral { .. } => true,
            LowerValue::Wasm { kind, .. } => *kind != ValueKind::Reference,
            _ => false,
        }
    }

    /// Read a bounds-checked positional argument in an adapter-ABI native
    /// body: `args[index]` when present, else the `undefined` value.
    fn proxy_native_arg(
        &self,
        body: &mut FunctionBody,
        block: Block,
        args: Value,
        index: u32,
    ) -> (Block, Value) {
        let len = body.add_op(block, Operator::ArrayLen, &[args], &[Type::I32]);
        let idx = body.add_op(block, Operator::I32Const { value: index }, &[], &[Type::I32]);
        let present = body.add_op(block, Operator::I32LtU, &[idx, len], &[Type::I32]);
        let get = body.add_block();
        let missing = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: present,
                if_true: BlockTarget {
                    block: get,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: missing,
                    args: vec![],
                },
            },
        );
        let value = body.add_op(
            get,
            Operator::ArrayGet {
                sig: self.repr.arguments,
            },
            &[args, idx],
            &[self.repr.value],
        );
        body.set_terminator(
            get,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![value],
                },
            },
        );
        let undef = body.add_op(
            missing,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        );
        body.set_terminator(
            missing,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![undef],
                },
            },
        );
        (join, result)
    }

    /// Allocate a proxy struct value around `target`/`handler` (both already
    /// boxed `anyref`).
    fn new_proxy_struct(
        &self,
        body: &mut FunctionBody,
        block: Block,
        target: Value,
        handler: Value,
    ) -> Value {
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let proxy = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.proxy,
            },
            &[target, handler, zero],
            &[self.repr.proxy_ty()],
        );
        body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.value,
            },
            &[proxy],
            &[self.repr.value],
        )
    }

    /// The generated Proxy helpers: the `Proxy` constructor and
    /// `Proxy.revocable` natives, the `revoke` thunk, and the per-trap
    /// dispatchers.
    fn ensure_proxy_helpers(&mut self) -> Result<ProxyHelpers, ConvertError> {
        if let Some(helpers) = self.proxy_helpers {
            return Ok(helpers);
        }
        let chain = self.ensure_chain_lookup()?;
        let truthy = self.ensure_truthy_helper()?;
        let property = self.ensure_property_helpers()?;

        // js_native_proxy_new: wrap args[0]/args[1] in a proxy struct. The
        // throwaway receiver `Item::New` created is discarded by the
        // constructor-result override.
        let adapter = self.repr.adapter;
        let new = {
            let mut body = FunctionBody::new(self.module, adapter);
            let entry = body.entry;
            let args = body.blocks[entry].params[2].1;
            let (block, target) = self.proxy_native_arg(&mut body, entry, args, 0);
            let (block, handler) = self.proxy_native_arg(&mut body, block, args, 1);
            let proxy = self.new_proxy_struct(&mut body, block, target, handler);
            body.set_terminator(block, Terminator::Return { values: vec![proxy] });
            self.module.funcs.push(FuncDecl::Body(
                adapter,
                format!("js_native_proxy_new_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_native_proxy_revoke: read the captured proxy from the context's
        // hidden slot and set its revoked flag.
        let revoke = {
            let mut body = FunctionBody::new(self.module, adapter);
            let entry = body.entry;
            let ctx = body.blocks[entry].params[0].1;
            let key = self.new_string(&mut body, entry, b"\x01proxy")?;
            let slot = body.add_op(
                entry,
                Operator::Call {
                    function_index: chain,
                },
                &[ctx, key],
                &[self.repr.slot_ty()],
            );
            let (block, proxy) = self.unwrap_slot(&mut body, entry, slot);
            let proxy = body.add_op(
                block,
                Operator::RefCast {
                    ty: self.repr.proxy_ty(),
                },
                &[proxy],
                &[self.repr.proxy_ty()],
            );
            let one = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            body.add_op(
                block,
                Operator::StructSet {
                    sig: self.repr.proxy,
                    idx: 2,
                },
                &[proxy, one],
                &[],
            );
            let undef = body.add_op(
                block,
                Operator::RefNull {
                    ty: self.repr.value,
                },
                &[],
                &[self.repr.value],
            );
            body.set_terminator(block, Terminator::Return { values: vec![undef] });
            self.module.funcs.push(FuncDecl::Body(
                adapter,
                format!("js_native_proxy_revoke_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_native_proxy_revocable: build the proxy, capture it in a fresh
        // context for the revoke thunk, and return `{ proxy, revoke }`.
        let revocable = {
            let mut body = FunctionBody::new(self.module, adapter);
            let entry = body.entry;
            let args = body.blocks[entry].params[2].1;
            let (mut block, target) = self.proxy_native_arg(&mut body, entry, args, 0);
            let (b, handler) = self.proxy_native_arg(&mut body, block, args, 1);
            block = b;
            let proxy = self.new_proxy_struct(&mut body, block, target, handler);
            let proxy_lv = LowerValue::Wasm {
                value: proxy,
                kind: ValueKind::Reference,
            };
            let revoke_ctx = self.new_object(&mut body, block)?;
            let revoke_ctx_v = revoke_ctx.wasm()?.0;
            block = self.set_static_property_value_raw(
                &mut body,
                block,
                &revoke_ctx,
                "\u{1}proxy",
                &proxy_lv,
            )?;
            let revoke_fn =
                self.function_object_for_native(&mut body, block, revoke_ctx_v, revoke)?;
            let result = self.new_object(&mut body, block)?;
            block =
                self.set_static_property_value_raw(&mut body, block, &result, "proxy", &proxy_lv)?;
            block = self.set_static_property_value_raw(
                &mut body,
                block,
                &result,
                "revoke",
                &revoke_fn,
            )?;
            let (result_v, _) = result.wasm()?;
            body.set_terminator(block, Terminator::Return { values: vec![result_v] });
            self.module.funcs.push(FuncDecl::Body(
                adapter,
                format!("js_native_proxy_revocable_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_proxy_get(proxy, key) -> slot. Revocation traps (the TypeError
        // analog); a missing trap forwards to a chain lookup on the target.
        let get = {
            let signature = self.module.signatures.push(SignatureData::Func {
                params: vec![self.repr.proxy_ty(), self.repr.string_ty()],
                returns: vec![self.repr.slot_ty()],
                shared: false,
            });
            let mut body = FunctionBody::new(self.module, signature);
            let entry = body.entry;
            let proxy = body.blocks[entry].params[0].1;
            let key = body.blocks[entry].params[1].1;
            let revoked = body.add_op(
                entry,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 2,
                },
                &[proxy],
                &[Type::I32],
            );
            let live = body.add_block();
            let revoked_arm = body.add_block();
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: revoked,
                    if_true: BlockTarget {
                        block: revoked_arm,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: live,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(revoked_arm, Terminator::Unreachable);
            let target = body.add_op(
                live,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 0,
                },
                &[proxy],
                &[self.repr.value],
            );
            let handler = body.add_op(
                live,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 1,
                },
                &[proxy],
                &[self.repr.value],
            );
            let get_name = self.new_string(&mut body, live, b"get")?;
            let trap_slot = body.add_op(
                live,
                Operator::Call {
                    function_index: chain,
                },
                &[handler, get_name],
                &[self.repr.slot_ty()],
            );
            let (block, trap) = self.unwrap_slot(&mut body, live, trap_slot);
            let has_trap = body.add_op(block, Operator::RefIsNull, &[trap], &[Type::I32]);
            let forward = body.add_block();
            let invoke = body.add_block();
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: has_trap,
                    if_true: BlockTarget {
                        block: forward,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: invoke,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(
                forward,
                Terminator::ReturnCall {
                    func: chain,
                    args: vec![target, key],
                },
            );
            let trap_fn = body.add_op(
                invoke,
                Operator::RefCast {
                    ty: self.repr.function_ty(),
                },
                &[trap],
                &[self.repr.function_ty()],
            );
            let trap_ctx = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 4,
                },
                &[trap_fn],
                &[self.repr.object_ty()],
            );
            let trap_code = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 3,
                },
                &[trap_fn],
                &[ref_sig(self.repr.adapter)],
            );
            let proxy_value = body.add_op(
                invoke,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[proxy],
                &[self.repr.value],
            );
            let trap_args = body.add_op(
                invoke,
                Operator::ArrayNewFixed {
                    sig: self.repr.arguments,
                    num: 3,
                },
                &[target, key, proxy_value],
                &[ref_sig(self.repr.arguments)],
            );
            let result = body.add_op(
                invoke,
                Operator::CallRef {
                    sig_index: self.repr.adapter,
                },
                &[trap_ctx, handler, trap_args, trap_code],
                &[self.repr.value],
            );
            let slot = self.new_slot_literal(&mut body, invoke, result, crate::repr::SLOT_FLAGS_DEFAULT);
            body.set_terminator(invoke, Terminator::Return { values: vec![slot] });
            self.module.funcs.push(FuncDecl::Body(
                signature,
                format!("js_proxy_get_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_proxy_set(proxy, key, slot). A missing trap forwards to an
        // ordinary own-property set on the target.
        let set = {
            let signature = self.module.signatures.push(SignatureData::Func {
                params: vec![
                    self.repr.proxy_ty(),
                    self.repr.string_ty(),
                    self.repr.slot_ty(),
                ],
                returns: vec![],
                shared: false,
            });
            let mut body = FunctionBody::new(self.module, signature);
            let entry = body.entry;
            let proxy = body.blocks[entry].params[0].1;
            let key = body.blocks[entry].params[1].1;
            let slot = body.blocks[entry].params[2].1;
            let revoked = body.add_op(
                entry,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 2,
                },
                &[proxy],
                &[Type::I32],
            );
            let live = body.add_block();
            let trap_block = body.add_block();
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: revoked,
                    if_true: BlockTarget {
                        block: trap_block,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: live,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(trap_block, Terminator::Unreachable);
            let target = body.add_op(
                live,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 0,
                },
                &[proxy],
                &[self.repr.value],
            );
            let handler = body.add_op(
                live,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 1,
                },
                &[proxy],
                &[self.repr.value],
            );
            let name = self.new_string(&mut body, live, b"set")?;
            let trap_slot = body.add_op(
                live,
                Operator::Call {
                    function_index: chain,
                },
                &[handler, name],
                &[self.repr.slot_ty()],
            );
            let (block, trap) = self.unwrap_slot(&mut body, live, trap_slot);
            let no_trap = body.add_op(block, Operator::RefIsNull, &[trap], &[Type::I32]);
            let forward = body.add_block();
            let invoke = body.add_block();
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: no_trap,
                    if_true: BlockTarget {
                        block: forward,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: invoke,
                        args: vec![],
                    },
                },
            );
            // Forward: extract the target's property root (function or
            // object header) and run the ordinary set. A primitive target
            // drops the write, matching a sloppy-mode store.
            let is_fn = body.add_op(
                forward,
                Operator::RefTest {
                    ty: self.repr.function_non_null_ty(),
                },
                &[target],
                &[Type::I32],
            );
            let fn_arm = body.add_block();
            let not_fn = body.add_block();
            body.set_terminator(
                forward,
                Terminator::CondBr {
                    cond: is_fn,
                    if_true: BlockTarget {
                        block: fn_arm,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: not_fn,
                        args: vec![],
                    },
                },
            );
            let fn_ref = body.add_op(
                fn_arm,
                Operator::RefCast {
                    ty: self.repr.function_ty(),
                },
                &[target],
                &[self.repr.function_ty()],
            );
            let fn_root = body.add_op(
                fn_arm,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 0,
                },
                &[fn_ref],
                &[self.repr.value],
            );
            let zero = body.add_op(fn_arm, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            body.add_op(
                fn_arm,
                Operator::Call {
                    function_index: property.set,
                },
                &[fn_root, key, slot, zero],
                &[],
            );
            body.set_terminator(fn_arm, Terminator::Return { values: vec![] });
            let is_obj = body.add_op(
                not_fn,
                Operator::RefTest {
                    ty: self.repr.object_non_null_ty(),
                },
                &[target],
                &[Type::I32],
            );
            let obj_arm = body.add_block();
            let prim_arm = body.add_block();
            body.set_terminator(
                not_fn,
                Terminator::CondBr {
                    cond: is_obj,
                    if_true: BlockTarget {
                        block: obj_arm,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: prim_arm,
                        args: vec![],
                    },
                },
            );
            let obj_ref = body.add_op(
                obj_arm,
                Operator::RefCast {
                    ty: self.repr.object_ty(),
                },
                &[target],
                &[self.repr.object_ty()],
            );
            let obj_root = body.add_op(
                obj_arm,
                Operator::StructGet {
                    sig: self.repr.object,
                    idx: 0,
                },
                &[obj_ref],
                &[self.repr.value],
            );
            let zero2 = body.add_op(obj_arm, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            body.add_op(
                obj_arm,
                Operator::Call {
                    function_index: property.set,
                },
                &[obj_root, key, slot, zero2],
                &[],
            );
            body.set_terminator(obj_arm, Terminator::Return { values: vec![] });
            body.set_terminator(prim_arm, Terminator::Return { values: vec![] });
            // Invoke the trap: handler.set(target, key, value, receiver).
            let trap_fn = body.add_op(
                invoke,
                Operator::RefCast {
                    ty: self.repr.function_ty(),
                },
                &[trap],
                &[self.repr.function_ty()],
            );
            let trap_ctx = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 4,
                },
                &[trap_fn],
                &[self.repr.object_ty()],
            );
            let trap_code = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 3,
                },
                &[trap_fn],
                &[ref_sig(self.repr.adapter)],
            );
            let stored = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.slot,
                    idx: 0,
                },
                &[slot],
                &[self.repr.value],
            );
            let proxy_value = body.add_op(
                invoke,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[proxy],
                &[self.repr.value],
            );
            let trap_args = body.add_op(
                invoke,
                Operator::ArrayNewFixed {
                    sig: self.repr.arguments,
                    num: 4,
                },
                &[target, key, stored, proxy_value],
                &[ref_sig(self.repr.arguments)],
            );
            body.add_op(
                invoke,
                Operator::CallRef {
                    sig_index: self.repr.adapter,
                },
                &[trap_ctx, handler, trap_args, trap_code],
                &[self.repr.value],
            );
            body.set_terminator(invoke, Terminator::Return { values: vec![] });
            self.module.funcs.push(FuncDecl::Body(
                signature,
                format!("js_proxy_set_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_chain_has(value, key) -> i32: own-or-proto presence walk.
        let chain_has = {
            let signature = self.module.signatures.push(SignatureData::Func {
                params: vec![self.repr.value, self.repr.string_ty()],
                returns: vec![Type::I32],
                shared: false,
            });
            let mut body = FunctionBody::new(self.module, signature);
            let entry = body.entry;
            let obj0 = body.blocks[entry].params[0].1;
            let key = body.blocks[entry].params[1].1;
            let loop_block = body.add_block();
            let cur = body.add_blockparam(loop_block, self.repr.value);
            body.set_terminator(
                entry,
                Terminator::Br {
                    target: BlockTarget {
                        block: loop_block,
                        args: vec![obj0],
                    },
                },
            );
            let is_null = body.add_op(loop_block, Operator::RefIsNull, &[cur], &[Type::I32]);
            let not_null = body.add_block();
            let absent = body.add_block();
            body.set_terminator(
                loop_block,
                Terminator::CondBr {
                    cond: is_null,
                    if_true: BlockTarget {
                        block: absent,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: not_null,
                        args: vec![],
                    },
                },
            );
            let zero = body.add_op(absent, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            body.set_terminator(absent, Terminator::Return { values: vec![zero] });
            let is_fn = body.add_op(
                not_null,
                Operator::RefTest {
                    ty: self.repr.function_non_null_ty(),
                },
                &[cur],
                &[Type::I32],
            );
            let fn_arm = body.add_block();
            let not_fn = body.add_block();
            body.set_terminator(
                not_null,
                Terminator::CondBr {
                    cond: is_fn,
                    if_true: BlockTarget {
                        block: fn_arm,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: not_fn,
                        args: vec![],
                    },
                },
            );
            let have_root = body.add_block();
            let root = body.add_blockparam(have_root, self.repr.value);
            let proto = body.add_blockparam(have_root, self.repr.value);
            let fn_ref = body.add_op(
                fn_arm,
                Operator::RefCast {
                    ty: self.repr.function_ty(),
                },
                &[cur],
                &[self.repr.function_ty()],
            );
            let fn_root = body.add_op(
                fn_arm,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 0,
                },
                &[fn_ref],
                &[self.repr.value],
            );
            let fn_proto = body.add_op(
                fn_arm,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 2,
                },
                &[fn_ref],
                &[self.repr.value],
            );
            body.set_terminator(
                fn_arm,
                Terminator::Br {
                    target: BlockTarget {
                        block: have_root,
                        args: vec![fn_root, fn_proto],
                    },
                },
            );
            let is_obj = body.add_op(
                not_fn,
                Operator::RefTest {
                    ty: self.repr.object_non_null_ty(),
                },
                &[cur],
                &[Type::I32],
            );
            let obj_arm = body.add_block();
            body.set_terminator(
                not_fn,
                Terminator::CondBr {
                    cond: is_obj,
                    if_true: BlockTarget {
                        block: obj_arm,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: absent,
                        args: vec![],
                    },
                },
            );
            let obj_ref = body.add_op(
                obj_arm,
                Operator::RefCast {
                    ty: self.repr.object_ty(),
                },
                &[cur],
                &[self.repr.object_ty()],
            );
            let obj_root = body.add_op(
                obj_arm,
                Operator::StructGet {
                    sig: self.repr.object,
                    idx: 0,
                },
                &[obj_ref],
                &[self.repr.value],
            );
            let obj_proto = body.add_op(
                obj_arm,
                Operator::StructGet {
                    sig: self.repr.object,
                    idx: 2,
                },
                &[obj_ref],
                &[self.repr.value],
            );
            body.set_terminator(
                obj_arm,
                Terminator::Br {
                    target: BlockTarget {
                        block: have_root,
                        args: vec![obj_root, obj_proto],
                    },
                },
            );
            let zero3 = body.add_op(have_root, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let slot = body.add_op(
                have_root,
                Operator::Call {
                    function_index: property.lookup,
                },
                &[root, key, zero3],
                &[self.repr.slot_ty()],
            );
            let missing = body.add_op(have_root, Operator::RefIsNull, &[slot], &[Type::I32]);
            let found = body.add_block();
            let keep_walking = body.add_block();
            body.set_terminator(
                have_root,
                Terminator::CondBr {
                    cond: missing,
                    if_true: BlockTarget {
                        block: keep_walking,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: found,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(
                keep_walking,
                Terminator::Br {
                    target: BlockTarget {
                        block: loop_block,
                        args: vec![proto],
                    },
                },
            );
            let one = body.add_op(found, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            body.set_terminator(found, Terminator::Return { values: vec![one] });
            self.module.funcs.push(FuncDecl::Body(
                signature,
                format!("js_chain_has_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_proxy_has(proxy, key) -> i32.
        let has = {
            let signature = self.module.signatures.push(SignatureData::Func {
                params: vec![self.repr.proxy_ty(), self.repr.string_ty()],
                returns: vec![Type::I32],
                shared: false,
            });
            let mut body = FunctionBody::new(self.module, signature);
            let entry = body.entry;
            let proxy = body.blocks[entry].params[0].1;
            let key = body.blocks[entry].params[1].1;
            let revoked = body.add_op(
                entry,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 2,
                },
                &[proxy],
                &[Type::I32],
            );
            let live = body.add_block();
            let trap_block = body.add_block();
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: revoked,
                    if_true: BlockTarget {
                        block: trap_block,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: live,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(trap_block, Terminator::Unreachable);
            let target = body.add_op(
                live,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 0,
                },
                &[proxy],
                &[self.repr.value],
            );
            let handler = body.add_op(
                live,
                Operator::StructGet {
                    sig: self.repr.proxy,
                    idx: 1,
                },
                &[proxy],
                &[self.repr.value],
            );
            let name = self.new_string(&mut body, live, b"has")?;
            let trap_slot = body.add_op(
                live,
                Operator::Call {
                    function_index: chain,
                },
                &[handler, name],
                &[self.repr.slot_ty()],
            );
            let (block, trap) = self.unwrap_slot(&mut body, live, trap_slot);
            let no_trap = body.add_op(block, Operator::RefIsNull, &[trap], &[Type::I32]);
            let forward = body.add_block();
            let invoke = body.add_block();
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: no_trap,
                    if_true: BlockTarget {
                        block: forward,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: invoke,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(
                forward,
                Terminator::ReturnCall {
                    func: chain_has,
                    args: vec![target, key],
                },
            );
            let trap_fn = body.add_op(
                invoke,
                Operator::RefCast {
                    ty: self.repr.function_ty(),
                },
                &[trap],
                &[self.repr.function_ty()],
            );
            let trap_ctx = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 4,
                },
                &[trap_fn],
                &[self.repr.object_ty()],
            );
            let trap_code = body.add_op(
                invoke,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 3,
                },
                &[trap_fn],
                &[ref_sig(self.repr.adapter)],
            );
            let trap_args = body.add_op(
                invoke,
                Operator::ArrayNewFixed {
                    sig: self.repr.arguments,
                    num: 2,
                },
                &[target, key],
                &[ref_sig(self.repr.arguments)],
            );
            let result = body.add_op(
                invoke,
                Operator::CallRef {
                    sig_index: self.repr.adapter,
                },
                &[trap_ctx, handler, trap_args, trap_code],
                &[self.repr.value],
            );
            let truth = body.add_op(
                invoke,
                Operator::Call {
                    function_index: truthy,
                },
                &[result],
                &[Type::I32],
            );
            body.set_terminator(invoke, Terminator::Return { values: vec![truth] });
            self.module.funcs.push(FuncDecl::Body(
                signature,
                format!("js_proxy_has_{}", self.module.funcs.len()),
                body,
            ))
        };

        // js_value_has(value, key) -> i32: the single-block `in` entry
        // point (the `binary` lowering contract is single-block, so the
        // proxy/ordinary split lives here).
        let value_has = {
            let signature = self.module.signatures.push(SignatureData::Func {
                params: vec![self.repr.value, self.repr.string_ty()],
                returns: vec![Type::I32],
                shared: false,
            });
            let mut body = FunctionBody::new(self.module, signature);
            let entry = body.entry;
            let value = body.blocks[entry].params[0].1;
            let key = body.blocks[entry].params[1].1;
            let is_proxy = body.add_op(
                entry,
                Operator::RefTest {
                    ty: self.repr.proxy_non_null_ty(),
                },
                &[value],
                &[Type::I32],
            );
            let proxy_arm = body.add_block();
            let ordinary_arm = body.add_block();
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: is_proxy,
                    if_true: BlockTarget {
                        block: proxy_arm,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: ordinary_arm,
                        args: vec![],
                    },
                },
            );
            let proxy = body.add_op(
                proxy_arm,
                Operator::RefCast {
                    ty: self.repr.proxy_ty(),
                },
                &[value],
                &[self.repr.proxy_ty()],
            );
            body.set_terminator(
                proxy_arm,
                Terminator::ReturnCall {
                    func: has,
                    args: vec![proxy, key],
                },
            );
            body.set_terminator(
                ordinary_arm,
                Terminator::ReturnCall {
                    func: chain_has,
                    args: vec![value, key],
                },
            );
            self.module.funcs.push(FuncDecl::Body(
                signature,
                format!("js_value_has_{}", self.module.funcs.len()),
                body,
            ))
        };

        // The natives are referenced by `ref.func` from their function
        // objects; the trap helpers are only ever `call`ed directly.
        self.declare_function_reference(new);
        self.declare_function_reference(revocable);
        self.declare_function_reference(revoke);
        let helpers = ProxyHelpers {
            new,
            revocable,
            revoke,
            get,
            set,
            has,
            chain_has,
            value_has,
        };
        self.proxy_helpers = Some(helpers);
        Ok(helpers)
    }

    /// Install the `Proxy` global: a constructor function object carrying a
    /// `prototype` and the `revocable` static.
    fn build_proxy_primordial(
        &mut self,
        body: &mut FunctionBody,
        mut block: Block,
        context: &LowerValue,
    ) -> Result<Block, ConvertError> {
        let helpers = self.ensure_proxy_helpers()?;
        let ctor_ctx = self.new_object(body, block)?;
        let ctor_ctx_v = ctor_ctx.wasm()?.0;
        let ctor = self.function_object_for_native(body, block, ctor_ctx_v, helpers.new)?;
        let proto = self.new_object(body, block)?;
        block = self.set_static_property_value_raw(body, block, &ctor, "prototype", &proto)?;
        let revocable_ctx = self.new_object(body, block)?;
        let revocable_ctx_v = revocable_ctx.wasm()?.0;
        let revocable =
            self.function_object_for_native(body, block, revocable_ctx_v, helpers.revocable)?;
        block = self.set_static_property_value_raw(body, block, &ctor, "revocable", &revocable)?;
        block = self.set_static_property_value_raw(body, block, context, "Proxy", &ctor)?;
        Ok(block)
    }

    /// A proxy-aware own/chain read for a static string key: `ref.test` the
    /// receiver, dispatch to the `get` trap helper for a proxy, and keep the
    /// ordinary read otherwise. The returned value is the raw slot contents;
    /// the caller runs `resolve_property_read` so a forwarded accessor on the
    /// target still invokes its getter.
    fn get_property_value_proxy_aware(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let (object_v, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.get_property_value_raw(body, block, object, key);
        }
        let helpers = self.ensure_proxy_helpers()?;
        let is_proxy = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.proxy_non_null_ty(),
            },
            &[object_v],
            &[Type::I32],
        );
        let proxy_arm = body.add_block();
        let ordinary_arm = body.add_block();
        let join = body.add_block();
        let joined = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_proxy,
                if_true: BlockTarget {
                    block: proxy_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ordinary_arm,
                    args: vec![],
                },
            },
        );
        let proxy = body.add_op(
            proxy_arm,
            Operator::RefCast {
                ty: self.repr.proxy_ty(),
            },
            &[object_v],
            &[self.repr.proxy_ty()],
        );
        let key_v = self.new_string(body, proxy_arm, key.as_bytes())?;
        let slot = body.add_op(
            proxy_arm,
            Operator::Call {
                function_index: helpers.get,
            },
            &[proxy, key_v],
            &[self.repr.slot_ty()],
        );
        let (proxy_arm, trapped) = self.unwrap_slot(body, proxy_arm, slot);
        body.set_terminator(
            proxy_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![trapped],
                },
            },
        );
        let (ordinary_arm, ordinary) = self.get_property_value_raw(body, ordinary_arm, object, key)?;
        let ordinary = self.box_value(body, ordinary_arm, &ordinary)?;
        body.set_terminator(
            ordinary_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![ordinary],
                },
            },
        );
        Ok((
            join,
            LowerValue::Wasm {
                value: joined,
                kind: ValueKind::Reference,
            },
        ))
    }

    /// The proxy-aware variant of a dynamic-string-key read.
    fn get_string_member_proxy_aware(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: Value,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let (object_v, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.get_string_member_raw(body, block, object, key);
        }
        let helpers = self.ensure_proxy_helpers()?;
        let is_proxy = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.proxy_non_null_ty(),
            },
            &[object_v],
            &[Type::I32],
        );
        let proxy_arm = body.add_block();
        let ordinary_arm = body.add_block();
        let join = body.add_block();
        let joined = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_proxy,
                if_true: BlockTarget {
                    block: proxy_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ordinary_arm,
                    args: vec![],
                },
            },
        );
        let proxy = body.add_op(
            proxy_arm,
            Operator::RefCast {
                ty: self.repr.proxy_ty(),
            },
            &[object_v],
            &[self.repr.proxy_ty()],
        );
        let slot = body.add_op(
            proxy_arm,
            Operator::Call {
                function_index: helpers.get,
            },
            &[proxy, key],
            &[self.repr.slot_ty()],
        );
        let (proxy_arm, trapped) = self.unwrap_slot(body, proxy_arm, slot);
        body.set_terminator(
            proxy_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![trapped],
                },
            },
        );
        let (ordinary_arm, ordinary) = self.get_string_member_raw(body, ordinary_arm, object, key)?;
        let ordinary = self.box_value(body, ordinary_arm, &ordinary)?;
        body.set_terminator(
            ordinary_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![ordinary],
                },
            },
        );
        Ok((
            join,
            LowerValue::Wasm {
                value: joined,
                kind: ValueKind::Reference,
            },
        ))
    }

    /// The proxy-aware write: the `set` trap on a proxied receiver, the
    /// ordinary descriptor-aware write otherwise. Returns the completed
    /// blocks, matching `set_property_value`'s contract.
    fn set_property_value_proxy_aware(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
        value: &LowerValue,
    ) -> Result<Vec<Block>, ConvertError> {
        let (object_v, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.set_property_value(body, block, object, key, value);
        }
        let helpers = self.ensure_proxy_helpers()?;
        let is_proxy = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.proxy_non_null_ty(),
            },
            &[object_v],
            &[Type::I32],
        );
        let proxy_arm = body.add_block();
        let ordinary_arm = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_proxy,
                if_true: BlockTarget {
                    block: proxy_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ordinary_arm,
                    args: vec![],
                },
            },
        );
        let proxy = body.add_op(
            proxy_arm,
            Operator::RefCast {
                ty: self.repr.proxy_ty(),
            },
            &[object_v],
            &[self.repr.proxy_ty()],
        );
        let key_v = self.new_string(body, proxy_arm, key.as_bytes())?;
        let boxed = self.box_value(body, proxy_arm, value)?;
        let slot = self.new_slot_literal(body, proxy_arm, boxed, crate::repr::SLOT_FLAGS_DEFAULT);
        body.add_op(
            proxy_arm,
            Operator::Call {
                function_index: helpers.set,
            },
            &[proxy, key_v, slot],
            &[],
        );
        let mut done = vec![proxy_arm];
        done.extend(self.set_property_value_ordinary(body, ordinary_arm, object, key, value)?);
        Ok(vec![self.join_write_continuations(body, done)])
    }

    /// The proxy-aware variant of a dynamic-string-key write.
    fn set_string_member_proxy_aware(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: Value,
        value: &LowerValue,
    ) -> Result<Vec<Block>, ConvertError> {
        let (object_v, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.set_string_member(body, block, object, key, value);
        }
        let helpers = self.ensure_proxy_helpers()?;
        let is_proxy = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.proxy_non_null_ty(),
            },
            &[object_v],
            &[Type::I32],
        );
        let proxy_arm = body.add_block();
        let ordinary_arm = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_proxy,
                if_true: BlockTarget {
                    block: proxy_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ordinary_arm,
                    args: vec![],
                },
            },
        );
        let proxy = body.add_op(
            proxy_arm,
            Operator::RefCast {
                ty: self.repr.proxy_ty(),
            },
            &[object_v],
            &[self.repr.proxy_ty()],
        );
        let boxed = self.box_value(body, proxy_arm, value)?;
        let slot = self.new_slot_literal(body, proxy_arm, boxed, crate::repr::SLOT_FLAGS_DEFAULT);
        body.add_op(
            proxy_arm,
            Operator::Call {
                function_index: helpers.set,
            },
            &[proxy, key, slot],
            &[],
        );
        let mut done = vec![proxy_arm];
        done.extend(self.set_string_member_ordinary(body, ordinary_arm, object, key, value)?);
        Ok(vec![self.join_write_continuations(body, done)])
    }

    /// `key in obj`: the `has` trap for a proxied object, an own-or-proto
    /// presence walk otherwise. A primitive right-hand side traps (the
    /// TypeError analog).
    fn has_in(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        left: &LowerValue,
        right: &LowerValue,
    ) -> Result<LowerValue, ConvertError> {
        let key = match left {
            LowerValue::String { value, .. } => *value,
            LowerValue::NumberLiteral { key, .. } => {
                self.new_string(body, block, key.as_bytes())?
            }
            _ => self.dynamic_string_key(body, block, left)?,
        };
        let (right_v, kind) = right.wasm()?;
        if kind != ValueKind::Reference {
            return Err(ConvertError::invalid("`in` on a primitive"));
        }
        let helpers = self.ensure_proxy_helpers()?;
        // The generated `js_value_has` keeps this site single-block (the
        // `binary` lowering contract); provenance-known receivers skip
        // straight to the presence walk.
        let helper = if Self::provably_non_proxy(right) {
            helpers.chain_has
        } else {
            helpers.value_has
        };
        let result = body.add_op(
            block,
            Operator::Call {
                function_index: helper,
            },
            &[right_v, key],
            &[Type::I32],
        );
        Ok(LowerValue::Wasm {
            value: result,
            kind: ValueKind::Boolean,
        })
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// The `(context, this, arguments, code)` tuple for a call whose callee
    /// could be a proxy: the `apply` trap (or a forward to the target) for a
    /// proxied function, the ordinary function-object extraction otherwise.
    /// Both arms produce the same tuple shape, so the caller's `CallRef`
    /// emission is unchanged and its exception handling keeps working.
    fn call_parts_proxy_aware(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        callee: &LowerValue,
        receiver: LowerValue,
        values: &ContinuationValues,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<(Block, Value, Value, Value, Value), ConvertError> {
        let (callee_v, kind) = callee.wasm()?;
        if Self::provably_non_proxy(callee) || kind != ValueKind::Reference {
            let (captured_context, effective_this, code, _) =
                self.callable_parts(body, block, callee, receiver)?;
            let array = self.make_arguments(body, block, values, args)?;
            return Ok((block, captured_context, effective_this, array, code));
        }
        let helpers = self.ensure_proxy_helpers()?;
        let chain = self.ensure_chain_lookup()?;
        let is_proxy = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.proxy_non_null_ty(),
            },
            &[callee_v],
            &[Type::I32],
        );
        let proxy_arm = body.add_block();
        let ordinary_arm = body.add_block();
        let join = body.add_block();
        let ctx_out = body.add_blockparam(join, ref_sig(self.repr.object));
        let this_out = body.add_blockparam(join, self.repr.value);
        let array_out = body.add_blockparam(join, ref_sig(self.repr.arguments));
        let code_out = body.add_blockparam(join, ref_sig(self.repr.adapter));
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_proxy,
                if_true: BlockTarget {
                    block: proxy_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ordinary_arm,
                    args: vec![],
                },
            },
        );
        // Proxy arm: handler.apply(target, thisArg, argsArray), or a forward
        // to the target when the handler defines no `apply` trap.
        let proxy = body.add_op(
            proxy_arm,
            Operator::RefCast {
                ty: self.repr.proxy_ty(),
            },
            &[callee_v],
            &[self.repr.proxy_ty()],
        );
        let revoked = body.add_op(
            proxy_arm,
            Operator::StructGet {
                sig: self.repr.proxy,
                idx: 2,
            },
            &[proxy],
            &[Type::I32],
        );
        let live = body.add_block();
        let revoked_arm = body.add_block();
        body.set_terminator(
            proxy_arm,
            Terminator::CondBr {
                cond: revoked,
                if_true: BlockTarget {
                    block: revoked_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: live,
                    args: vec![],
                },
            },
        );
        body.set_terminator(revoked_arm, Terminator::Unreachable);
        let target = body.add_op(
            live,
            Operator::StructGet {
                sig: self.repr.proxy,
                idx: 0,
            },
            &[proxy],
            &[self.repr.value],
        );
        let handler = body.add_op(
            live,
            Operator::StructGet {
                sig: self.repr.proxy,
                idx: 1,
            },
            &[proxy],
            &[self.repr.value],
        );
        let name = self.new_string(body, live, b"apply")?;
        let trap_slot = body.add_op(
            live,
            Operator::Call {
                function_index: chain,
            },
            &[handler, name],
            &[self.repr.slot_ty()],
        );
        let (live, trap) = self.unwrap_slot(body, live, trap_slot);
        let no_trap = body.add_op(live, Operator::RefIsNull, &[trap], &[Type::I32]);
        let forward = body.add_block();
        let invoke = body.add_block();
        body.set_terminator(
            live,
            Terminator::CondBr {
                cond: no_trap,
                if_true: BlockTarget {
                    block: forward,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: invoke,
                    args: vec![],
                },
            },
        );
        let target_lv = LowerValue::Wasm {
            value: target,
            kind: ValueKind::Reference,
        };
        let (fctx, fthis, fcode, _) =
            self.callable_parts(body, forward, &target_lv, receiver.clone())?;
        let farray = self.make_arguments(body, forward, values, args)?;
        body.set_terminator(
            forward,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![fctx, fthis, farray, fcode],
                },
            },
        );
        let trap_lv = LowerValue::Wasm {
            value: trap,
            kind: ValueKind::Reference,
        };
        let handler_lv = LowerValue::Wasm {
            value: handler,
            kind: ValueKind::Reference,
        };
        let (tctx, tthis, tcode, _) =
            self.callable_parts(body, invoke, &trap_lv, handler_lv)?;
        let inner_args = self.make_arguments(body, invoke, values, args)?;
        let this_arg = self.box_value(body, invoke, &receiver)?;
        let trap_args = body.add_op(
            invoke,
            Operator::ArrayNewFixed {
                sig: self.repr.arguments,
                num: 3,
            },
            &[target, this_arg, inner_args],
            &[ref_sig(self.repr.arguments)],
        );
        body.set_terminator(
            invoke,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![tctx, tthis, trap_args, tcode],
                },
            },
        );
        // Ordinary arm: the direct function-object extraction.
        let (captured_context, effective_this, code, _) =
            self.callable_parts(body, ordinary_arm, callee, receiver)?;
        let array = self.make_arguments(body, ordinary_arm, values, args)?;
        body.set_terminator(
            ordinary_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![captured_context, effective_this, array, code],
                },
            },
        );
        Ok((join, ctx_out, this_out, array_out, code_out))
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {

    /// The ordinary `new` path: receiver creation, prototype wiring, the
    /// constructor call, and the object/function/proxy result override.
    fn lower_new_ordinary(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        callee: &LowerValue,
        this: LowerValue,
        values: &ContinuationValues,
        args: &[SValueId],
        span: &impl std::fmt::Debug,
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
                let receiver = self.new_object(body, block)?;
                // `new C()`: the receiver's [[Prototype]] is the
                // constructor's `prototype` property when that is an
                // object/function, else null (no Object.prototype
                // primordial exists yet).
                let mut block = block;
                {
                    let (b, prototype) =
                        self.get_property_value_raw(body, block, callee, "prototype")?;
                    block = b;
                    let (prototype, _) = prototype.wasm()?;
                    let proto_is_object = body.add_op(
                        block,
                        Operator::RefTest {
                            ty: self.repr.object_ty(),
                        },
                        &[prototype],
                        &[Type::I32],
                    );
                    let proto_is_function = body.add_op(
                        block,
                        Operator::RefTest {
                            ty: self.repr.function_ty(),
                        },
                        &[prototype],
                        &[Type::I32],
                    );
                    let proto_ok = body.add_op(
                        block,
                        Operator::I32Or,
                        &[proto_is_object, proto_is_function],
                        &[Type::I32],
                    );
                    let null_proto = body.add_op(
                        block,
                        Operator::RefNull {
                            ty: self.repr.value,
                        },
                        &[],
                        &[self.repr.value],
                    );
                    let proto = body.add_op(
                        block,
                        Operator::TypedSelect {
                            ty: self.repr.value,
                        },
                        &[prototype, null_proto, proto_ok],
                        &[self.repr.value],
                    );
                    let (receiver_value, _) = receiver.wasm()?;
                    self.set_object_proto(body, block, receiver_value, proto);
                }
                let (ctx, _default_this, code, arrow) =
                    self.callable_parts(body, block, callee, this.clone())?;
                let is_arrow = arrow;
                // A construct of an arrow function is invalid JavaScript. The
                // generated trap also keeps a dynamic callable from silently
                // acquiring constructor semantics.
                let reject = body.add_block();
                let invoke = body.add_block();
                let join = body.add_block();
                let joined = body.add_blockparam(join, self.repr.value);
                body.set_terminator(
                    block,
                    Terminator::CondBr {
                        cond: is_arrow,
                        if_true: BlockTarget {
                            block: reject,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: invoke,
                            args: vec![],
                        },
                    },
                );
                body.set_terminator(reject, Terminator::Unreachable);
                let array = self.make_plain_arguments(body, invoke, values, args)?;
                let receiver_value = self.box_value(body, invoke, &receiver)?;
                let result = body.add_op(
                    invoke,
                    Operator::CallRef {
                        sig_index: self.repr.adapter,
                    },
                    &[ctx, receiver_value, array, code],
                    &[self.repr.value],
                );
                let (normal, exceptional) = if self.exceptions_enabled {
                    let is_exception = body.add_op(
                        invoke,
                        Operator::RefTest {
                            ty: self.repr.exception_result_non_null_ty(),
                        },
                        &[result],
                        &[Type::I32],
                    );
                    let exceptional = body.add_block();
                    let normal = body.add_block();
                    body.set_terminator(
                        invoke,
                        Terminator::CondBr {
                            cond: is_exception,
                            if_true: BlockTarget {
                                block: exceptional,
                                args: vec![],
                            },
                            if_false: BlockTarget {
                                block: normal,
                                args: vec![],
                            },
                        },
                    );
                    (normal, Some(exceptional))
                } else {
                    (invoke, None)
                };
                let object = body.add_op(
                    normal,
                    Operator::RefTest {
                        // Non-null: `undefined` (the null anyref) must fall
                        // through to the implicit receiver; the nullable
                        // variant would treat it as an explicit object
                        // override and the instance would become null.
                        ty: self.repr.object_non_null_ty(),
                    },
                    &[result],
                    &[Type::I32],
                );
                let function = body.add_op(
                    normal,
                    Operator::RefTest {
                        ty: self.repr.function_non_null_ty(),
                    },
                    &[result],
                    &[Type::I32],
                );
                // A constructor may also produce a proxy (`new Proxy(...)`
                // returns the proxy struct from its native body).
                let proxy = body.add_op(
                    normal,
                    Operator::RefTest {
                        ty: self.repr.proxy_non_null_ty(),
                    },
                    &[result],
                    &[Type::I32],
                );
                let object_or_function =
                    body.add_op(normal, Operator::I32Or, &[object, function], &[Type::I32]);
                let override_result =
                    body.add_op(normal, Operator::I32Or, &[object_or_function, proxy], &[Type::I32]);
                let explicit = body.add_block();
                let implicit = body.add_block();
                body.set_terminator(
                    normal,
                    Terminator::CondBr {
                        cond: override_result,
                        if_true: BlockTarget {
                            block: explicit,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: implicit,
                            args: vec![],
                        },
                    },
                );
                body.set_terminator(
                    explicit,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![result],
                        },
                    },
                );
                let receiver_result = self.box_value(body, implicit, &receiver)?;
                body.set_terminator(
                    implicit,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![receiver_result],
                        },
                    },
                );
                let normal_result = (
                    join,
                    LowerValue::Wasm {
                        value: joined,
                        kind: ValueKind::Reference,
                    },
                );
                if let Some(exceptional) = exceptional {
                    return Ok(vec![
                        (exceptional, LowerValue::CallResult(result)),
                        normal_result,
                    ]);
                }
                return Ok(vec![normal_result]);
    }

    /// `new` dispatch with proxy awareness: a proxied constructor runs the
    /// `construct` trap (`handler.construct(target, argsArray, proxy)`) or —
    /// with no trap — the ordinary path on the target. Both arms return
    /// already-complete continuations, so the results concatenate.
    fn lower_new_dispatch(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        callee: LowerValue,
        this: LowerValue,
        values: &ContinuationValues,
        args: &[SValueId],
        span: &impl std::fmt::Debug,
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let (callee_v, kind) = callee.wasm()?;
        if Self::provably_non_proxy(&callee) || kind != ValueKind::Reference {
            return self.lower_new_ordinary(body, block, &callee, this, values, args, span);
        }
        let chain = self.ensure_chain_lookup()?;
        let is_proxy = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.proxy_non_null_ty(),
            },
            &[callee_v],
            &[Type::I32],
        );
        let proxy_arm = body.add_block();
        let ordinary_arm = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_proxy,
                if_true: BlockTarget {
                    block: proxy_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ordinary_arm,
                    args: vec![],
                },
            },
        );
        let mut results = Vec::new();
        // Proxy arm.
        let proxy = body.add_op(
            proxy_arm,
            Operator::RefCast {
                ty: self.repr.proxy_ty(),
            },
            &[callee_v],
            &[self.repr.proxy_ty()],
        );
        let revoked = body.add_op(
            proxy_arm,
            Operator::StructGet {
                sig: self.repr.proxy,
                idx: 2,
            },
            &[proxy],
            &[Type::I32],
        );
        let live = body.add_block();
        let revoked_arm = body.add_block();
        body.set_terminator(
            proxy_arm,
            Terminator::CondBr {
                cond: revoked,
                if_true: BlockTarget {
                    block: revoked_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: live,
                    args: vec![],
                },
            },
        );
        body.set_terminator(revoked_arm, Terminator::Unreachable);
        let target = body.add_op(
            live,
            Operator::StructGet {
                sig: self.repr.proxy,
                idx: 0,
            },
            &[proxy],
            &[self.repr.value],
        );
        let handler = body.add_op(
            live,
            Operator::StructGet {
                sig: self.repr.proxy,
                idx: 1,
            },
            &[proxy],
            &[self.repr.value],
        );
        let name = self.new_string(body, live, b"construct")?;
        let trap_slot = body.add_op(
            live,
            Operator::Call {
                function_index: chain,
            },
            &[handler, name],
            &[self.repr.slot_ty()],
        );
        let (live, trap) = self.unwrap_slot(body, live, trap_slot);
        let no_trap = body.add_op(live, Operator::RefIsNull, &[trap], &[Type::I32]);
        let forward = body.add_block();
        let invoke = body.add_block();
        body.set_terminator(
            live,
            Terminator::CondBr {
                cond: no_trap,
                if_true: BlockTarget {
                    block: forward,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: invoke,
                    args: vec![],
                },
            },
        );
        let target_lv = LowerValue::Wasm {
            value: target,
            kind: ValueKind::Reference,
        };
        results.extend(self.lower_new_ordinary(
            body,
            forward,
            &target_lv,
            this.clone(),
            values,
            args,
            span,
        )?);
        let trap_lv = LowerValue::Wasm {
            value: trap,
            kind: ValueKind::Reference,
        };
        let handler_lv = LowerValue::Wasm {
            value: handler,
            kind: ValueKind::Reference,
        };
        let (tctx, tthis, tcode, _) = self.callable_parts(body, invoke, &trap_lv, handler_lv)?;
        let inner_args = self.make_plain_arguments(body, invoke, values, args)?;
        let proxy_value = body.add_op(
            invoke,
            Operator::RefCast {
                ty: self.repr.value,
            },
            &[proxy],
            &[self.repr.value],
        );
        let trap_args = body.add_op(
            invoke,
            Operator::ArrayNewFixed {
                sig: self.repr.arguments,
                num: 3,
            },
            &[target, inner_args, proxy_value],
            &[ref_sig(self.repr.arguments)],
        );
        let result = body.add_op(
            invoke,
            Operator::CallRef {
                sig_index: self.repr.adapter,
            },
            &[tctx, tthis, trap_args, tcode],
            &[self.repr.value],
        );
        results.push((
            invoke,
            if self.exceptions_enabled {
                LowerValue::CallResult(result)
            } else {
                LowerValue::Wasm {
                    value: result,
                    kind: ValueKind::Reference,
                }
            },
        ));
        // Ordinary arm.
        results.extend(self.lower_new_ordinary(
            body,
            ordinary_arm,
            &callee,
            this,
            values,
            args,
            span,
        )?);
        Ok(results)
    }

}
