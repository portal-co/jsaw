// Runtime property enumeration.
//
// Nothing elsewhere in this crate walks a trie or a shape's field set at
// runtime — property access is always by a single already-known (or
// already-resolved) key. `Object.keys`/`values`/`entries`/
// `getOwnPropertyNames`/`getOwnPropertyDescriptors`/`assign`/`freeze`/
// `isFrozen` and `Reflect.ownKeys` all need the actual list of an object's
// own property names, so this file adds that one primitive
// (`ensure_object_enumerate_keys`) and everything else reuses the ordinary
// (accessor-aware, writable-respecting) get/set machinery per discovered
// key rather than duplicating that logic here.
//
// Every helper here returns a plain, already-exact-length `arguments`
// array rather than a `(array, count)` pair with spare growth capacity:
// multi-return `Operator::Call` (via `ValueDef::PickOutput`) reliably fails
// this backend's own byte-level Wasm validation, even though the IR-level
// `body.validate()` accepts it and the same pattern is used successfully
// elsewhere in the wider waffle codebase — a version-specific backend gap,
// not a mistake in the surrounding IR construction. Reallocating on every
// push is O(n^2) in an object's own-key count, which is a non-issue at the
// property-count scale this is ever exercised at.
//
// `include!`d as a module-level item (see `conv.rs`), same as
// `primordials.rs`.

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {

/// Iterate `i` from `0` to `len` (exclusive), threading an accumulator of
/// type `acc_ty` through each iteration. `body_fn` receives the current
/// index and accumulator and must return the block it left off on plus the
/// updated accumulator — it must NOT terminate that block itself. Returns
/// the block after the loop plus the final accumulator.
fn for_each_index_fold(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    len: Value,
    init: Value,
    acc_ty: Type,
    mut body_fn: impl FnMut(
        &mut Self,
        &mut FunctionBody,
        Block,
        Value,
        Value,
    ) -> Result<(Block, Value), ConvertError>,
) -> Result<(Block, Value), ConvertError> {
    let loop_block = body.add_block();
    let i = body.add_blockparam(loop_block, Type::I32);
    let acc = body.add_blockparam(loop_block, acc_ty);
    let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    body.set_terminator(
        block,
        Terminator::Br {
            target: BlockTarget {
                block: loop_block,
                args: vec![zero, init],
            },
        },
    );

    let done = body.add_op(loop_block, Operator::I32GeU, &[i, len], &[Type::I32]);
    let body_block = body.add_block();
    let after_block = body.add_block();
    let final_acc = body.add_blockparam(after_block, acc_ty);
    body.set_terminator(
        loop_block,
        Terminator::CondBr {
            cond: done,
            if_true: BlockTarget {
                block: after_block,
                args: vec![acc],
            },
            if_false: BlockTarget {
                block: body_block,
                args: vec![],
            },
        },
    );
    let (final_block, new_acc) = body_fn(self, body, body_block, i, acc)?;
    let one = body.add_op(final_block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let next_i = body.add_op(final_block, Operator::I32Add, &[i, one], &[Type::I32]);
    body.set_terminator(
        final_block,
        Terminator::Br {
            target: BlockTarget {
                block: loop_block,
                args: vec![next_i, new_acc],
            },
        },
    );
    Ok((after_block, final_acc))
}

/// [`Self::for_each_index_fold`] without an accumulator, for loops that
/// only run side effects per index.
fn for_each_index(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    len: Value,
    mut body_fn: impl FnMut(&mut Self, &mut FunctionBody, Block, Value) -> Result<Block, ConvertError>,
) -> Result<Block, ConvertError> {
    let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    let (after, _) = self.for_each_index_fold(
        body,
        block,
        len,
        zero,
        Type::I32,
        |this, body, block, i, acc| {
            let block = body_fn(this, body, block, i)?;
            Ok((block, acc))
        },
    )?;
    Ok(after)
}

/// Append `value` to an `arguments`-typed array by allocating a new,
/// exact-length replacement (see the module-level comment for why this
/// doesn't grow the array with spare capacity instead).
fn ensure_arguments_push(&mut self) -> Result<Func, ConvertError> {
    if let Some(func) = self.arguments_push_helper {
        return Ok(func);
    }
    let sig = self.module.signatures.push(SignatureData::Func {
        params: vec![self.repr.arguments_ty(), self.repr.value],
        returns: vec![self.repr.arguments_ty()],
        shared: false,
    });
    let mut body = FunctionBody::new(self.module, sig);
    let entry = body.entry;
    let array = body.blocks[entry].params[0].1;
    let value = body.blocks[entry].params[1].1;

    let len = body.add_op(entry, Operator::ArrayLen, &[array], &[Type::I32]);
    let one = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let new_len = body.add_op(entry, Operator::I32Add, &[len, one], &[Type::I32]);
    let grown = body.add_op(
        entry,
        Operator::ArrayNewDefault {
            sig: self.repr.arguments,
        },
        &[new_len],
        &[self.repr.arguments_ty()],
    );
    let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    body.add_op(
        entry,
        Operator::ArrayCopy {
            dest: self.repr.arguments,
            src: self.repr.arguments,
        },
        &[grown, zero, array, zero, len],
        &[],
    );
    body.add_op(
        entry,
        Operator::ArraySet {
            sig: self.repr.arguments,
        },
        &[grown, len, value],
        &[],
    );
    body.set_terminator(
        entry,
        Terminator::Return {
            values: vec![grown],
        },
    );

    let func = self.module.funcs.push(FuncDecl::Body(
        sig,
        "js_arguments_push".to_string(),
        body,
    ));
    self.arguments_push_helper = Some(func);
    Ok(func)
}

/// Build a JS string value from `prefix[0..prefix_len]`, box it, and append
/// it to `keys` if `only_enumerable == 0` or the slot's enumerable bit is
/// set. Returns the (possibly extended) array either way.
fn enumerate_maybe_push_key(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    slot: Value,
    key_bytes: Value,
    key_len: Value,
    keys: Value,
    only_enumerable: Value,
) -> Result<(Block, Value), ConvertError> {
    let cast = body.add_op(
        block,
        Operator::RefCast {
            ty: self.repr.slot_non_null_ty(),
        },
        &[slot],
        &[self.repr.slot_non_null_ty()],
    );
    let flags = body.add_op(
        block,
        Operator::StructGet {
            sig: self.repr.slot,
            idx: 1,
        },
        &[cast],
        &[Type::I32],
    );
    let enumerable_bit = self.slot_flag_bit(body, block, flags, crate::repr::SLOT_ENUMERABLE);
    let skip_filter = body.add_op(block, Operator::I32Eqz, &[only_enumerable], &[Type::I32]);
    let passes = body.add_op(block, Operator::I32Or, &[enumerable_bit, skip_filter], &[Type::I32]);

    let do_push = body.add_block();
    let join = body.add_block();
    let keys_out = body.add_blockparam(join, self.repr.arguments_ty());
    body.set_terminator(
        block,
        Terminator::CondBr {
            cond: passes,
            if_true: BlockTarget {
                block: do_push,
                args: vec![],
            },
            if_false: BlockTarget {
                block: join,
                args: vec![keys],
            },
        },
    );

    let trimmed = body.add_op(
        do_push,
        Operator::ArrayNewDefault {
            sig: self.repr.utf8,
        },
        &[key_len],
        &[self.repr.utf8_ty()],
    );
    let zero = body.add_op(do_push, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    body.add_op(
        do_push,
        Operator::ArrayCopy {
            dest: self.repr.utf8,
            src: self.repr.utf8,
        },
        &[trimmed, zero, key_bytes, zero, key_len],
        &[],
    );
    let utf16_null = body.add_op(
        do_push,
        Operator::RefNull {
            ty: self.repr.utf16_ty(),
        },
        &[],
        &[self.repr.utf16_ty()],
    );
    let key_struct = body.add_op(
        do_push,
        Operator::StructNew {
            sig: self.repr.string,
        },
        &[trimmed, utf16_null],
        &[self.repr.string_ty()],
    );
    let key_value = self.anyref(body, do_push, key_struct);
    let push_func = self.ensure_arguments_push()?;
    let pushed = body.add_op(
        do_push,
        Operator::Call {
            function_index: push_func,
        },
        &[keys, key_value],
        &[self.repr.arguments_ty()],
    );
    body.set_terminator(
        do_push,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![pushed],
            },
        },
    );

    Ok((join, keys_out))
}

/// Recursively walk a generic property trie, appending each present slot's
/// key (reconstructed from the accumulated byte path) to `keys`.
/// `only_enumerable` (0 or 1) filters by the slot's enumerable bit; pass 0
/// to include everything (`Reflect.ownKeys`/`getOwnPropertyNames`/
/// `getOwnPropertyDescriptors`/`freeze`/`isFrozen`).
fn ensure_trie_enumerate(&mut self) -> Result<Func, ConvertError> {
    if let Some(func) = self.trie_enumerate_helper {
        return Ok(func);
    }
    let sig = self.module.signatures.push(SignatureData::Func {
        params: vec![
            self.repr.trie_ty(),
            self.repr.utf8_ty(),
            Type::I32,
            self.repr.arguments_ty(),
            Type::I32,
        ],
        returns: vec![self.repr.arguments_ty()],
        shared: false,
    });
    let function = self.module.funcs.push(FuncDecl::Body(
        sig,
        "js_trie_enumerate".to_string(),
        FunctionBody::new(self.module, sig),
    ));
    // Record before filling in the body so the recursive call below can
    // reference this same function, mirroring `ensure_trie_clone_function`.
    self.trie_enumerate_helper = Some(function);

    let mut declaration = std::mem::take(&mut self.module.funcs[function]);
    let result = (|| {
        let body = declaration
            .body_mut()
            .ok_or_else(|| ConvertError::invalid("generated trie-enumerate function has no body"))?;
        let entry = body.entry;
        let trie = body.blocks[entry].params[0].1;
        let prefix = body.blocks[entry].params[1].1;
        let prefix_len = body.blocks[entry].params[2].1;
        let keys = body.blocks[entry].params[3].1;
        let only_enumerable = body.blocks[entry].params[4].1;

        let terminal = body.add_op(
            entry,
            Operator::StructGet {
                sig: self.repr.trie,
                idx: 0,
            },
            &[trie],
            &[self.repr.slot_ty()],
        );
        let is_null = body.add_op(entry, Operator::RefIsNull, &[terminal], &[Type::I32]);
        let has_terminal = body.add_block();
        let after_terminal = body.add_block();
        let keys_after = body.add_blockparam(after_terminal, self.repr.arguments_ty());
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget {
                    block: after_terminal,
                    args: vec![keys],
                },
                if_false: BlockTarget {
                    block: has_terminal,
                    args: vec![],
                },
            },
        );
        let (has_terminal_end, pushed_keys) = self.enumerate_maybe_push_key(
            body,
            has_terminal,
            terminal,
            prefix,
            prefix_len,
            keys,
            only_enumerable,
        )?;
        body.set_terminator(
            has_terminal_end,
            Terminator::Br {
                target: BlockTarget {
                    block: after_terminal,
                    args: vec![pushed_keys],
                },
            },
        );

        let mut current = after_terminal;
        let mut keys_cur = keys_after;
        for raw in 0..=u8::MAX {
            let child = body.add_op(
                current,
                Operator::StructGet {
                    sig: self.repr.trie,
                    idx: usize::from(raw) + 1,
                },
                &[trie],
                &[self.repr.value],
            );
            let child_is_null = body.add_op(current, Operator::RefIsNull, &[child], &[Type::I32]);
            let recurse = body.add_block();
            let next = body.add_block();
            let keys_next = body.add_blockparam(next, self.repr.arguments_ty());
            body.set_terminator(
                current,
                Terminator::CondBr {
                    cond: child_is_null,
                    if_true: BlockTarget {
                        block: next,
                        args: vec![keys_cur],
                    },
                    if_false: BlockTarget {
                        block: recurse,
                        args: vec![],
                    },
                },
            );

            let child_trie = body.add_op(
                recurse,
                Operator::RefCast {
                    ty: self.repr.trie_ty(),
                },
                &[child],
                &[self.repr.trie_ty()],
            );
            let one = body.add_op(recurse, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let new_prefix_len = body.add_op(recurse, Operator::I32Add, &[prefix_len, one], &[Type::I32]);
            let new_prefix = body.add_op(
                recurse,
                Operator::ArrayNewDefault {
                    sig: self.repr.utf8,
                },
                &[new_prefix_len],
                &[self.repr.utf8_ty()],
            );
            let zero = body.add_op(recurse, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            body.add_op(
                recurse,
                Operator::ArrayCopy {
                    dest: self.repr.utf8,
                    src: self.repr.utf8,
                },
                &[new_prefix, zero, prefix, zero, prefix_len],
                &[],
            );
            let byte_const = body.add_op(
                recurse,
                Operator::I32Const {
                    value: u32::from(raw),
                },
                &[],
                &[Type::I32],
            );
            body.add_op(
                recurse,
                Operator::ArraySet {
                    sig: self.repr.utf8,
                },
                &[new_prefix, prefix_len, byte_const],
                &[],
            );
            let recursed = body.add_op(
                recurse,
                Operator::Call {
                    function_index: function,
                },
                &[child_trie, new_prefix, new_prefix_len, keys_cur, only_enumerable],
                &[self.repr.arguments_ty()],
            );
            body.set_terminator(
                recurse,
                Terminator::Br {
                    target: BlockTarget {
                        block: next,
                        args: vec![recursed],
                    },
                },
            );

            current = next;
            keys_cur = keys_next;
        }
        body.set_terminator(
            current,
            Terminator::Return {
                values: vec![keys_cur],
            },
        );
        Ok(())
    })();
    self.module.funcs[function] = declaration;
    result?;
    Ok(function)
}

/// Collect an object's own-property keys, dispatching across every
/// registered shape (whose keys are known at compile time — only the
/// shape's fallback trie needs a runtime walk) plus the fully generic trie
/// case, mirroring the same shape-then-trie dispatch `emit_property_lookup`
/// already uses for ordinary reads.
fn ensure_object_enumerate_keys(&mut self) -> Result<Func, ConvertError> {
    if let Some(func) = self.object_enumerate_keys_helper {
        return Ok(func);
    }
    let sig = self.module.signatures.push(SignatureData::Func {
        params: vec![self.repr.value, Type::I32],
        returns: vec![self.repr.arguments_ty()],
        shared: false,
    });
    let function = self.module.funcs.push(FuncDecl::Body(
        sig,
        "js_object_enumerate_keys".to_string(),
        FunctionBody::new(self.module, sig),
    ));
    self.object_enumerate_keys_helper = Some(function);

    let mut declaration = std::mem::take(&mut self.module.funcs[function]);
    let result = (|| {
        let shapes = self.shapes.clone();
        let trie_enumerate = self.ensure_trie_enumerate()?;
        let body = declaration
            .body_mut()
            .ok_or_else(|| ConvertError::invalid("generated object-enumerate function has no body"))?;
        let entry = body.entry;
        let root = body.blocks[entry].params[0].1;
        let only_enumerable = body.blocks[entry].params[1].1;

        let mut current = entry;
        for shape in &shapes {
            let matches = body.add_op(
                current,
                Operator::RefTest {
                    ty: ref_sig(shape.sig),
                },
                &[root],
                &[Type::I32],
            );
            let found = body.add_block();
            let next = body.add_block();
            body.set_terminator(
                current,
                Terminator::CondBr {
                    cond: matches,
                    if_true: BlockTarget {
                        block: found,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: next,
                        args: vec![],
                    },
                },
            );

            let instance = body.add_op(
                found,
                Operator::RefCast {
                    ty: ref_sig(shape.sig),
                },
                &[root],
                &[ref_sig(shape.sig)],
            );
            let zero_len = body.add_op(found, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let empty = body.add_op(
                found,
                Operator::ArrayNewDefault {
                    sig: self.repr.arguments,
                },
                &[zero_len],
                &[self.repr.arguments_ty()],
            );
            let mut field_block = found;
            let mut keys_cur = empty;
            for (field, name) in shape.keys.iter().enumerate() {
                let slot = body.add_op(
                    field_block,
                    Operator::StructGet {
                        sig: shape.sig,
                        idx: field + 1,
                    },
                    &[instance],
                    &[self.repr.slot_ty()],
                );
                let is_null = body.add_op(field_block, Operator::RefIsNull, &[slot], &[Type::I32]);
                let has_slot = body.add_block();
                let after = body.add_block();
                let keys_after = body.add_blockparam(after, self.repr.arguments_ty());
                body.set_terminator(
                    field_block,
                    Terminator::CondBr {
                        cond: is_null,
                        if_true: BlockTarget {
                            block: after,
                            args: vec![keys_cur],
                        },
                        if_false: BlockTarget {
                            block: has_slot,
                            args: vec![],
                        },
                    },
                );
                let key_string = self.new_string(body, has_slot, name.as_bytes())?;
                let key_bytes = body.add_op(
                    has_slot,
                    Operator::StructGet {
                        sig: self.repr.string,
                        idx: 0,
                    },
                    &[key_string],
                    &[self.repr.utf8_ty()],
                );
                let key_len = body.add_op(has_slot, Operator::ArrayLen, &[key_bytes], &[Type::I32]);
                let (has_slot_end, pushed_keys) = self.enumerate_maybe_push_key(
                    body,
                    has_slot,
                    slot,
                    key_bytes,
                    key_len,
                    keys_cur,
                    only_enumerable,
                )?;
                body.set_terminator(
                    has_slot_end,
                    Terminator::Br {
                        target: BlockTarget {
                            block: after,
                            args: vec![pushed_keys],
                        },
                    },
                );
                field_block = after;
                keys_cur = keys_after;
            }

            let fallback = body.add_op(
                field_block,
                Operator::StructGet {
                    sig: shape.sig,
                    idx: 0,
                },
                &[instance],
                &[self.repr.trie_ty()],
            );
            let zero_len = body.add_op(field_block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let empty_prefix = body.add_op(
                field_block,
                Operator::ArrayNewDefault {
                    sig: self.repr.utf8,
                },
                &[zero_len],
                &[self.repr.utf8_ty()],
            );
            let final_keys = body.add_op(
                field_block,
                Operator::Call {
                    function_index: trie_enumerate,
                },
                &[fallback, empty_prefix, zero_len, keys_cur, only_enumerable],
                &[self.repr.arguments_ty()],
            );
            body.set_terminator(
                field_block,
                Terminator::Return {
                    values: vec![final_keys],
                },
            );

            current = next;
        }

        let trie_root = body.add_op(
            current,
            Operator::RefCast {
                ty: self.repr.trie_ty(),
            },
            &[root],
            &[self.repr.trie_ty()],
        );
        let zero_len = body.add_op(current, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let empty_prefix = body.add_op(
            current,
            Operator::ArrayNewDefault {
                sig: self.repr.utf8,
            },
            &[zero_len],
            &[self.repr.utf8_ty()],
        );
        let empty_keys = body.add_op(
            current,
            Operator::ArrayNewDefault {
                sig: self.repr.arguments,
            },
            &[zero_len],
            &[self.repr.arguments_ty()],
        );
        let final_keys = body.add_op(
            current,
            Operator::Call {
                function_index: trie_enumerate,
            },
            &[trie_root, empty_prefix, zero_len, empty_keys, only_enumerable],
            &[self.repr.arguments_ty()],
        );
        body.set_terminator(
            current,
            Terminator::Return {
                values: vec![final_keys],
            },
        );
        Ok(())
    })();
    self.module.funcs[function] = declaration;
    result?;
    Ok(function)
}

/// Collect an object's own-property keys as an exact-length `arguments`
/// array (not yet wrapped as a JS `Array` object — callers that need a
/// real, user-visible array should pass the result to
/// [`Self::new_array_object`]).
fn enumerate_own_keys(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    target: &LowerValue,
    only_enumerable: bool,
) -> Result<(Block, Value), ConvertError> {
    let (block, root) = self.object_and_root(body, block, target)?;
    let func = self.ensure_object_enumerate_keys()?;
    let flag = body.add_op(
        block,
        Operator::I32Const {
            value: u32::from(only_enumerable),
        },
        &[],
        &[Type::I32],
    );
    let keys = body.add_op(
        block,
        Operator::Call {
            function_index: func,
        },
        &[root, flag],
        &[self.repr.arguments_ty()],
    );
    Ok((block, keys))
}

}
