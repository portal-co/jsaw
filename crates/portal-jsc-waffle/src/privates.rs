// Private class members (DreamComp #33).
//
// Private names resolve at compile time to hidden own-property slots
// derived from the swc hygiene pair `(Atom, SyntaxContext)`: the resolver
// gives every class body's private names a fresh `SyntaxContext`, so two
// class bodies declaring `#x` get disjoint slots while an access site and
// its declaration share one (verified by `private_ctxt_probe`). This is the
// jsaw analogue of DreamComp's shape-hidden `ObjStatePrivateKey` — the key
// is fully resolved at compile time and never collides with a public
// property. The hidden name *embeds* the hygiene pair through a hash; the
// pair is never flattened to the bare symbol, so distinct bindings that
// share text stay distinct.
//
// Storage model:
// - Instance members: a generated per-class-site wrapper constructor
//   (`js_priv_ctor_N`) sits where the constructor function object would.
//   `Item::New` and `super()` construct through it; it stores each member's
//   value (kept in the wrapper's context under `\x01privv<k>`) onto the
//   receiver under its hidden name, then tail-calls the real constructor
//   (`\x01priv_target` in the context). A derived class's wrapper chains
//   through `super()` to the parent's wrapper, so every class in the chain
//   installs its own slots on the one receiver.
// - Static members: installed directly on the constructor object at class
//   creation, mirroring public statics.
//
// Brand checks (`#x in obj`) test the value slot's presence: installation
// always creates a slot — even for a missing or `undefined` initializer —
// and an absent slot is null, so `RefIsNull` on the own-slot lookup is the
// brand test. Reads and writes reuse the public descriptor machinery, so
// private accessors dispatch to their getter/setter exactly like public
// ones.
//
// Deliberate divergences from full JS semantics (matching the class model
// of #31):
// - Reading or writing an uninstalled private slot yields `undefined` /
//   performs the store instead of throwing a TypeError; `#x in obj` is the
//   explicit brand test.
// - Private field initializers are evaluated once at class-creation time
//   (the #31 field-initializer divergence) and the value is installed per
//   instance.
// - Primitive receivers answer "absent" instead of throwing a TypeError.

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// The hidden own-property name for the private member `sym`/`ctxt`.
    ///
    /// The `SyntaxContext` is part of the hashed key material: two class
    /// bodies that both declare `#x` hold distinct marks, and their slots
    /// must not collide.
    fn private_slot_name(sym: &str, ctxt: SyntaxContext) -> String {
        let mut hasher = Sha3_256::new();
        hasher.update(b"portal-jsc-private/v1");
        hasher.update(sym.as_bytes());
        hasher.update(ctxt.as_u32().to_be_bytes());
        let hash = u64::from_be_bytes(hasher.finalize()[..8].try_into().unwrap());
        format!("\x01priv\x01{hash:016x}")
    }

    /// The raw own slot of `key` on `object` (no prototype walk). A null
    /// slot means the property is absent; a present slot may still hold the
    /// `undefined` value.
    fn get_own_property_slot(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
    ) -> Result<(Block, Value), ConvertError> {
        let (block, root) = self.object_and_root(body, block, object)?;
        let helpers = self.ensure_property_helpers()?;
        let key = self.new_string(body, block, key.as_bytes())?;
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let slot = body.add_op(
            block,
            Operator::Call {
                function_index: helpers.lookup,
            },
            &[root, key, zero],
            &[self.repr.slot_ty()],
        );
        Ok((block, slot))
    }

    /// The own-property value of `key` on `object` (no prototype walk),
    /// `undefined` when absent.
    fn get_own_property_value(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let (block, slot) = self.get_own_property_slot(body, block, object, key)?;
        let (block, value) = self.unwrap_slot(body, block, slot);
        Ok((
            block,
            LowerValue::Wasm {
                value,
                kind: ValueKind::Reference,
            },
        ))
    }

    /// `obj.#name`: an own-slot read through the descriptor machinery, so a
    /// private accessor calls its getter and a data/method slot yields its
    /// value.
    fn private_read(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        mem: &portal_jsc_swc_tac::Private,
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let name = Self::private_slot_name(&mem.sym, mem.ctxt);
        let (block, value) = self.get_own_property_value(body, block, object, &name)?;
        self.resolve_property_read(body, block, object, value)
    }

    /// `obj.#name = value`: an own-slot write through the descriptor
    /// machinery, so a private accessor calls its setter and a data slot is
    /// updated in place.
    fn private_write(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        mem: &portal_jsc_swc_tac::Private,
        value: &LowerValue,
    ) -> Result<Vec<Block>, ConvertError> {
        let name = Self::private_slot_name(&mem.sym, mem.ctxt);
        let (block, existing) = self.get_own_property_value(body, block, object, &name)?;
        let (ordinary, mut done) =
            self.split_descriptor_write(body, block, object, existing, value)?;
        done.push(self.set_static_property_value_raw(
            body, ordinary, object, &name, value,
        )?);
        Ok(vec![self.join_write_continuations(body, done)])
    }

    /// `#name in obj`: the own-slot presence test.
    fn private_brand(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        mem: &portal_jsc_swc_tac::Private,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let name = Self::private_slot_name(&mem.sym, mem.ctxt);
        let (block, slot) = self.get_own_property_slot(body, block, object, &name)?;
        let is_null = body.add_op(block, Operator::RefIsNull, &[slot], &[Type::I32]);
        let present = body.add_op(block, Operator::I32Eqz, &[is_null], &[Type::I32]);
        Ok((
            block,
            LowerValue::Wasm {
                value: present,
                kind: ValueKind::Boolean,
            },
        ))
    }

    /// The generated wrapper constructor for a class with instance private
    /// members: install every member's value from the wrapper context onto
    /// the receiver under its hidden name, then tail-call the real
    /// constructor kept under `\x01priv_target`.
    ///
    /// Generated fresh per class-site lowering; all per-evaluation state
    /// (member values, the real constructor) flows through the wrapper's
    /// context object, so repeated evaluations of one class expression get
    /// fresh slots.
    fn private_ctor_wrapper(&mut self, member_names: &[String]) -> Result<Func, ConvertError> {
        let signature = self.repr.adapter;
        let mut body = FunctionBody::new(self.module, signature);
        let entry = body.entry;
        let ctx = body.blocks[entry].params[0].1;
        let this = body.blocks[entry].params[1].1;
        let args = body.blocks[entry].params[2].1;
        let chain = self.ensure_chain_lookup()?;
        let mut block = entry;
        for (index, name) in member_names.iter().enumerate() {
            let key = self.new_string(&mut body, block, format!("\x01privv{index}").as_bytes())?;
            let slot = body.add_op(
                block,
                Operator::Call {
                    function_index: chain,
                },
                &[ctx, key],
                &[self.repr.slot_ty()],
            );
            let (next, value) = self.unwrap_slot(&mut body, block, slot);
            block = next;
            let receiver = LowerValue::Wasm {
                value: this,
                kind: ValueKind::Reference,
            };
            let stored = LowerValue::Wasm {
                value,
                kind: ValueKind::Reference,
            };
            block =
                self.set_static_property_value_raw(&mut body, block, &receiver, name, &stored)?;
        }
        let key = self.new_string(&mut body, block, b"\x01priv_target")?;
        let slot = body.add_op(
            block,
            Operator::Call {
                function_index: chain,
            },
            &[ctx, key],
            &[self.repr.slot_ty()],
        );
        let (next, target) = self.unwrap_slot(&mut body, block, slot);
        block = next;
        let target_fn = body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.function_ty(),
            },
            &[target],
            &[self.repr.function_ty()],
        );
        let target_ctx = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.function,
                idx: 4,
            },
            &[target_fn],
            &[self.repr.object_ty()],
        );
        let target_code = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.function,
                idx: 3,
            },
            &[target_fn],
            &[ref_sig(self.repr.adapter)],
        );
        let result = body.add_op(
            block,
            Operator::CallRef {
                sig_index: self.repr.adapter,
            },
            &[target_ctx, this, args, target_code],
            &[self.repr.value],
        );
        body.set_terminator(block, Terminator::Return {
            values: vec![result],
        });
        let func = self.module.funcs.push(FuncDecl::Body(
            signature,
            format!("js_priv_ctor_{}", self.module.funcs.len()),
            body,
        ));
        // The wrapper's function object references it by `ref.func`, which
        // requires a declared element slot.
        self.declare_function_reference(func);
        Ok(func)
    }
}
