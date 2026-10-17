// JavaScript class support (DreamComp #31): prototype chains, class
// creation, `super`, and `instanceof`. `include!`d into `impl Converter` in
// `conv.rs` alongside `primordials.rs`.
//
// Prototype model: object field 2 and function field 2 (both previously the
// unused `properties` value slot) carry the `[[Prototype]]` reference
// (`null` when absent). Property *reads* walk the chain through the
// generated `js_property_chain_lookup` helper; *writes* stay own-slot, so JS
// shadowing falls out: a write creates/updates the own property and a later
// read finds it before the prototype's.
//
// Deliberate divergences from full JS semantics (documented in #31):
// - Class field initializers are evaluated once at class-creation time (an
//   swc-tac design point: the initializer is lowered in the enclosing flow)
//   and instance-field initial values land on the prototype; constructor
//   assignments then shadow them per instance. A field initialized to a
//   *mutable* object is therefore shared across instances.
// - `super(...)` runs the parent constructor with the receiver the outer
//   `new` already created; TDZ-before-super, `new.target`, and a parent
//   constructor's object-return override are not modeled.
// - `instanceof` traps (rather than throwing TypeError) when the right-hand
//   side is not a function or its `prototype` is not an object, and ignores
//   `Symbol.hasInstance`.

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// The `[[Prototype]]` of a runtime reference value: function field 2 or
    /// object field 2; `null` for anything else (primitives, the i31
    /// sentinel, or a null input).
    fn proto_of(&self, body: &mut FunctionBody, block: Block, value: Value) -> (Block, Value) {
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        let null_arm = body.add_block();
        let not_null = body.add_block();
        let is_null = body.add_op(block, Operator::RefIsNull, &[value], &[Type::I32]);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget {
                    block: null_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_null,
                    args: vec![],
                },
            },
        );
        let is_function = body.add_op(
            not_null,
            Operator::RefTest {
                ty: self.repr.function_non_null_ty(),
            },
            &[value],
            &[Type::I32],
        );
        let function_arm = body.add_block();
        let check_object = body.add_block();
        body.set_terminator(
            not_null,
            Terminator::CondBr {
                cond: is_function,
                if_true: BlockTarget {
                    block: function_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: check_object,
                    args: vec![],
                },
            },
        );
        let function = body.add_op(
            function_arm,
            Operator::RefCast {
                ty: self.repr.function_ty(),
            },
            &[value],
            &[self.repr.function_ty()],
        );
        let function_proto = body.add_op(
            function_arm,
            Operator::StructGet {
                sig: self.repr.function,
                idx: crate::repr::FUNCTION_FIELD_PROTO,
            },
            &[function],
            &[self.repr.value],
        );
        body.set_terminator(
            function_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![function_proto],
                },
            },
        );
        let is_object = body.add_op(
            check_object,
            Operator::RefTest {
                ty: self.repr.object_non_null_ty(),
            },
            &[value],
            &[Type::I32],
        );
        let object_arm = body.add_block();
        body.set_terminator(
            check_object,
            Terminator::CondBr {
                cond: is_object,
                if_true: BlockTarget {
                    block: object_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: null_arm,
                    args: vec![],
                },
            },
        );
        let object = body.add_op(
            object_arm,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[value],
            &[self.repr.object_ty()],
        );
        let object_proto = body.add_op(
            object_arm,
            Operator::StructGet {
                sig: self.repr.object,
                idx: crate::repr::OBJECT_FIELD_PROTO,
            },
            &[object],
            &[self.repr.value],
        );
        body.set_terminator(
            object_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![object_proto],
                },
            },
        );
        let null = body.add_op(
            null_arm,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        );
        body.set_terminator(
            null_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![null],
                },
            },
        );
        (join, result)
    }

    /// Set the `[[Prototype]]` of an object (field 2). `object` must be an
    /// object-typed reference; `proto` is any `anyref` (null allowed).
    fn set_object_proto(
        &self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
        proto: Value,
    ) {
        body.add_op(
            block,
            Operator::StructSet {
                sig: self.repr.object,
                idx: crate::repr::OBJECT_FIELD_PROTO,
            },
            &[object, proto],
            &[],
        );
    }

    /// Set the `[[Prototype]]` of a function (field 2) — used for `extends`
    /// static inheritance (the constructor's parent constructor).
    fn set_function_proto(
        &self,
        body: &mut FunctionBody,
        block: Block,
        function: Value,
        proto: Value,
    ) {
        body.add_op(
            block,
            Operator::StructSet {
                sig: self.repr.function,
                idx: crate::repr::FUNCTION_FIELD_PROTO,
            },
            &[function, proto],
            &[],
        );
    }

    /// The generated prototype-chain lookup:
    /// `js_property_chain_lookup(obj: anyref, key: string) -> slot|null`.
    /// Own properties first, then each `[[Prototype]]` in order; `null` slot
    /// when the whole chain misses.
    fn ensure_chain_lookup(&mut self) -> Result<Func, ConvertError> {
        if let Some(func) = self.chain_lookup {
            return Ok(func);
        }
        let helpers = self.ensure_property_helpers()?;
        let signature = self.module.signatures.push(SignatureData::Func {
            params: vec![self.repr.value, self.repr.string_ty()],
            returns: vec![self.repr.slot_ty()],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, signature);
        let entry = body.entry;
        let key = body.blocks[entry].params[1].1;
        let obj0 = body.blocks[entry].params[0].1;

        let loop_block = body.add_block();
        let cur = body.add_blockparam(loop_block, self.repr.value);
        let have_root = body.add_block();
        let root = body.add_blockparam(have_root, self.repr.value);
        let next = body.add_blockparam(have_root, self.repr.value);
        let found = body.add_block();
        let slot_at_found = body.add_blockparam(found, self.repr.slot_ty());
        let null_arm = body.add_block();

        body.set_terminator(
            entry,
            Terminator::Br {
                target: BlockTarget {
                    block: loop_block,
                    args: vec![obj0],
                },
            },
        );
        // loop: dispatch on the current object's kind.
        let is_null = body.add_op(loop_block, Operator::RefIsNull, &[cur], &[Type::I32]);
        let not_null = body.add_block();
        body.set_terminator(
            loop_block,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget {
                    block: null_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_null,
                    args: vec![],
                },
            },
        );
        let is_function = body.add_op(
            not_null,
            Operator::RefTest {
                ty: self.repr.function_non_null_ty(),
            },
            &[cur],
            &[Type::I32],
        );
        let function_arm = body.add_block();
        let check_object = body.add_block();
        body.set_terminator(
            not_null,
            Terminator::CondBr {
                cond: is_function,
                if_true: BlockTarget {
                    block: function_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: check_object,
                    args: vec![],
                },
            },
        );
        let function = body.add_op(
            function_arm,
            Operator::RefCast {
                ty: self.repr.function_ty(),
            },
            &[cur],
            &[self.repr.function_ty()],
        );
        let function_root = body.add_op(
            function_arm,
            Operator::StructGet {
                sig: self.repr.function,
                idx: 0,
            },
            &[function],
            &[self.repr.value],
        );
        let function_next = body.add_op(
            function_arm,
            Operator::StructGet {
                sig: self.repr.function,
                idx: crate::repr::FUNCTION_FIELD_PROTO,
            },
            &[function],
            &[self.repr.value],
        );
        body.set_terminator(
            function_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: have_root,
                    args: vec![function_root, function_next],
                },
            },
        );
        let is_object = body.add_op(
            check_object,
            Operator::RefTest {
                ty: self.repr.object_non_null_ty(),
            },
            &[cur],
            &[Type::I32],
        );
        let object_arm = body.add_block();
        body.set_terminator(
            check_object,
            Terminator::CondBr {
                cond: is_object,
                if_true: BlockTarget {
                    block: object_arm,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: null_arm,
                    args: vec![],
                },
            },
        );
        let object = body.add_op(
            object_arm,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[cur],
            &[self.repr.object_ty()],
        );
        let object_root = body.add_op(
            object_arm,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 0,
            },
            &[object],
            &[self.repr.value],
        );
        let object_next = body.add_op(
            object_arm,
            Operator::StructGet {
                sig: self.repr.object,
                idx: crate::repr::OBJECT_FIELD_PROTO,
            },
            &[object],
            &[self.repr.value],
        );
        body.set_terminator(
            object_arm,
            Terminator::Br {
                target: BlockTarget {
                    block: have_root,
                    args: vec![object_root, object_next],
                },
            },
        );
        // have_root(root, next): own lookup; miss continues up the chain.
        let zero = body.add_op(have_root, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let slot = body.add_op(
            have_root,
            Operator::Call {
                function_index: helpers.lookup,
            },
            &[root, key, zero],
            &[self.repr.slot_ty()],
        );
        let slot_null = body.add_op(have_root, Operator::RefIsNull, &[slot], &[Type::I32]);
        body.set_terminator(
            have_root,
            Terminator::CondBr {
                cond: slot_null,
                if_true: BlockTarget {
                    block: loop_block,
                    args: vec![next],
                },
                if_false: BlockTarget {
                    block: found,
                    args: vec![slot],
                },
            },
        );
        body.set_terminator(found, Terminator::Return {
            values: vec![slot_at_found],
        });
        let absent = body.add_op(
            null_arm,
            Operator::RefNull {
                ty: self.repr.slot_ty(),
            },
            &[],
            &[self.repr.slot_ty()],
        );
        body.set_terminator(null_arm, Terminator::Return { values: vec![absent] });

        let func = self.module.funcs.push(FuncDecl::Body(
            signature,
            format!("js_property_chain_lookup_{}", self.module.funcs.len()),
            body,
        ));
        self.chain_lookup = Some(func);
        Ok(func)
    }

    /// A function object wrapping a generated (source-less) adapter body —
    /// used for the synthesized default constructors.
    fn function_object_for_native(
        &self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        func: Func,
    ) -> Result<LowerValue, ConvertError> {
        let trie = self.new_trie(body, block)?;
        let elements = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.arguments_ty(),
            },
            &[],
            &[self.repr.arguments_ty()],
        );
        let proto_null = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        );
        let code = body.add_op(
            block,
            Operator::RefFunc { func_index: func },
            &[],
            &[ref_sig(self.repr.adapter)],
        );
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let undef_this = self.undef(body, block);
        let captured_this = self.box_value(body, block, &undef_this)?;
        let trie = self.anyref(body, block, trie);
        let value = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.function,
            },
            &[trie, elements, proto_null, code, context, captured_this, zero, zero],
            &[self.repr.function_ty()],
        );
        Ok(LowerValue::Wasm {
            value,
            kind: ValueKind::Reference,
        })
    }

    /// The generated default constructors. A class with no explicit
    /// constructor gets `constructor() {}`; a derived class gets
    /// `constructor(...args) { super(...args); }` under this file's receiver
    /// model (the parent's body runs with the already-created receiver and
    /// the caller's arguments).
    fn ensure_default_ctor(&mut self, derived: bool) -> Result<Func, ConvertError> {
        if let Some(func) = if derived {
            self.default_ctor_derived
        } else {
            self.default_ctor_base
        } {
            return Ok(func);
        }
        let signature = self.repr.adapter;
        let mut body = FunctionBody::new(self.module, signature);
        let entry = body.entry;
        let ctx = body.blocks[entry].params[0].1;
        let this = body.blocks[entry].params[1].1;
        let args = body.blocks[entry].params[2].1;
        if !derived {
            let undefined = body.add_op(
                entry,
                Operator::RefNull {
                    ty: self.repr.value,
                },
                &[],
                &[self.repr.value],
            );
            body.set_terminator(entry, Terminator::Return {
                values: vec![undefined],
            });
        } else {
            let key = self.new_string(&mut body, entry, b"\x01super")?;
            let chain = self.ensure_chain_lookup()?;
            let slot = body.add_op(
                entry,
                Operator::Call {
                    function_index: chain,
                },
                &[ctx, key],
                &[self.repr.slot_ty()],
            );
            let slot_null = body.add_op(entry, Operator::RefIsNull, &[slot], &[Type::I32]);
            let present = body.add_block();
            let trap = body.add_block();
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: slot_null,
                    if_true: BlockTarget {
                        block: trap,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: present,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(trap, Terminator::Unreachable);
            let parent = body.add_op(
                present,
                Operator::StructGet {
                    sig: self.repr.slot,
                    idx: 0,
                },
                &[slot],
                &[self.repr.value],
            );
            let parent_fn = body.add_op(
                present,
                Operator::RefCast {
                    ty: self.repr.function_ty(),
                },
                &[parent],
                &[self.repr.function_ty()],
            );
            let parent_ctx = body.add_op(
                present,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 4,
                },
                &[parent_fn],
                &[self.repr.object_ty()],
            );
            let parent_code = body.add_op(
                present,
                Operator::StructGet {
                    sig: self.repr.function,
                    idx: 3,
                },
                &[parent_fn],
                &[ref_sig(self.repr.adapter)],
            );
            let result = body.add_op(
                present,
                Operator::CallRef {
                    sig_index: self.repr.adapter,
                },
                &[parent_ctx, this, args, parent_code],
                &[self.repr.value],
            );
            body.set_terminator(present, Terminator::Return { values: vec![result] });
        }
        let func = self.module.funcs.push(FuncDecl::Body(
            signature,
            format!(
                "js_default_ctor_{}_{}",
                if derived { "derived" } else { "base" },
                self.module.funcs.len()
            ),
            body,
        ));
        // The function object references the adapter by `ref.func`, which
        // requires a declared element slot.
        self.declare_function_reference(func);
        if derived {
            self.default_ctor_derived = Some(func);
        } else {
            self.default_ctor_base = Some(func);
        }
        Ok(func)
    }

    /// The generated `instanceof` helper: `js_instanceof(lhs, rhs) -> i32`.
    /// Walks `lhs`'s prototype chain looking for `rhs.prototype`. Traps when
    /// `rhs` is not a function or its `prototype` is not an object/function
    /// (JS would throw TypeError).
    fn ensure_instanceof(&mut self) -> Result<Func, ConvertError> {
        if let Some(func) = self.instanceof_helper {
            return Ok(func);
        }
        let helpers = self.ensure_property_helpers()?;
        let signature = self.module.signatures.push(SignatureData::Func {
            params: vec![self.repr.value, self.repr.value],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, signature);
        let entry = body.entry;
        let lhs = body.blocks[entry].params[0].1;
        let rhs = body.blocks[entry].params[1].1;

        let trap = body.add_block();
        let is_function = body.add_op(
            entry,
            Operator::RefTest {
                ty: self.repr.function_ty(),
            },
            &[rhs],
            &[Type::I32],
        );
        let rhs_function = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: is_function,
                if_true: BlockTarget {
                    block: rhs_function,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: trap,
                    args: vec![],
                },
            },
        );
        let function = body.add_op(
            rhs_function,
            Operator::RefCast {
                ty: self.repr.function_ty(),
            },
            &[rhs],
            &[self.repr.function_ty()],
        );
        let root = body.add_op(
            rhs_function,
            Operator::StructGet {
                sig: self.repr.function,
                idx: 0,
            },
            &[function],
            &[self.repr.value],
        );
        let key = self.new_string(&mut body, rhs_function, b"prototype")?;
        let zero = body.add_op(rhs_function, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let slot = body.add_op(
            rhs_function,
            Operator::Call {
                function_index: helpers.lookup,
            },
            &[root, key, zero],
            &[self.repr.slot_ty()],
        );
        let slot_null = body.add_op(rhs_function, Operator::RefIsNull, &[slot], &[Type::I32]);
        let have_target = body.add_block();
        body.set_terminator(
            rhs_function,
            Terminator::CondBr {
                cond: slot_null,
                if_true: BlockTarget {
                    block: trap,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: have_target,
                    args: vec![],
                },
            },
        );
        let target = body.add_op(
            have_target,
            Operator::StructGet {
                sig: self.repr.slot,
                idx: 0,
            },
            &[slot],
            &[self.repr.value],
        );
        let target_null = body.add_op(have_target, Operator::RefIsNull, &[target], &[Type::I32]);
        let target_is_object = body.add_op(
            have_target,
            Operator::RefTest {
                ty: self.repr.object_ty(),
            },
            &[target],
            &[Type::I32],
        );
        let target_is_function = body.add_op(
            have_target,
            Operator::RefTest {
                ty: self.repr.function_ty(),
            },
            &[target],
            &[Type::I32],
        );
        let target_ref = body.add_op(
            have_target,
            Operator::I32Or,
            &[target_is_object, target_is_function],
            &[Type::I32],
        );
        let one = body.add_op(have_target, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let target_not_ref = body.add_op(have_target, Operator::I32Xor, &[target_ref, one], &[Type::I32]);
        let target_bad = body.add_op(
            have_target,
            Operator::I32Or,
            &[target_null, target_not_ref],
            &[Type::I32],
        );
        let target_ok = body.add_block();
        body.set_terminator(
            have_target,
            Terminator::CondBr {
                cond: target_bad,
                if_true: BlockTarget {
                    block: trap,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: target_ok,
                    args: vec![],
                },
            },
        );
        let eq_non_null = Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Eq,
            nullable: false,
        });
        let target_eq = body.add_op(
            target_ok,
            Operator::RefCast { ty: eq_non_null },
            &[target],
            &[eq_non_null],
        );
        let loop_block = body.add_block();
        let cur = body.add_blockparam(loop_block, self.repr.value);
        body.set_terminator(
            target_ok,
            Terminator::Br {
                target: BlockTarget {
                    block: loop_block,
                    args: vec![lhs],
                },
            },
        );
        let return_false = body.add_block();
        let return_true = body.add_block();
        let is_null = body.add_op(loop_block, Operator::RefIsNull, &[cur], &[Type::I32]);
        let not_null = body.add_block();
        body.set_terminator(
            loop_block,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget {
                    block: return_false,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_null,
                    args: vec![],
                },
            },
        );
        let cur_is_function = body.add_op(
            not_null,
            Operator::RefTest {
                ty: self.repr.function_non_null_ty(),
            },
            &[cur],
            &[Type::I32],
        );
        let cur_function = body.add_block();
        let cur_check_object = body.add_block();
        body.set_terminator(
            not_null,
            Terminator::CondBr {
                cond: cur_is_function,
                if_true: BlockTarget {
                    block: cur_function,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: cur_check_object,
                    args: vec![],
                },
            },
        );
        let cur_f = body.add_op(
            cur_function,
            Operator::RefCast {
                ty: self.repr.function_ty(),
            },
            &[cur],
            &[self.repr.function_ty()],
        );
        let cur_f_next = body.add_op(
            cur_function,
            Operator::StructGet {
                sig: self.repr.function,
                idx: crate::repr::FUNCTION_FIELD_PROTO,
            },
            &[cur_f],
            &[self.repr.value],
        );
        let have_next = body.add_block();
        let next = body.add_blockparam(have_next, self.repr.value);
        body.set_terminator(
            cur_function,
            Terminator::Br {
                target: BlockTarget {
                    block: have_next,
                    args: vec![cur_f_next],
                },
            },
        );
        let cur_is_object = body.add_op(
            cur_check_object,
            Operator::RefTest {
                ty: self.repr.object_non_null_ty(),
            },
            &[cur],
            &[Type::I32],
        );
        let cur_object = body.add_block();
        body.set_terminator(
            cur_check_object,
            Terminator::CondBr {
                cond: cur_is_object,
                if_true: BlockTarget {
                    block: cur_object,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: return_false,
                    args: vec![],
                },
            },
        );
        let cur_o = body.add_op(
            cur_object,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[cur],
            &[self.repr.object_ty()],
        );
        let cur_o_next = body.add_op(
            cur_object,
            Operator::StructGet {
                sig: self.repr.object,
                idx: crate::repr::OBJECT_FIELD_PROTO,
            },
            &[cur_o],
            &[self.repr.value],
        );
        body.set_terminator(
            cur_object,
            Terminator::Br {
                target: BlockTarget {
                    block: have_next,
                    args: vec![cur_o_next],
                },
            },
        );
        // have_next(next): null → false; ref.eq target → true; else loop.
        let next_null = body.add_op(have_next, Operator::RefIsNull, &[next], &[Type::I32]);
        let compare = body.add_block();
        body.set_terminator(
            have_next,
            Terminator::CondBr {
                cond: next_null,
                if_true: BlockTarget {
                    block: return_false,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: compare,
                    args: vec![],
                },
            },
        );
        let next_eq = body.add_op(
            compare,
            Operator::RefCast { ty: eq_non_null },
            &[next],
            &[eq_non_null],
        );
        let equal = body.add_op(compare, Operator::RefEq, &[next_eq, target_eq], &[Type::I32]);
        body.set_terminator(
            compare,
            Terminator::CondBr {
                cond: equal,
                if_true: BlockTarget {
                    block: return_true,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: loop_block,
                    args: vec![next],
                },
            },
        );
        body.set_terminator(trap, Terminator::Unreachable);
        let zero = body.add_op(return_false, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(return_false, Terminator::Return { values: vec![zero] });
        let one = body.add_op(return_true, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        body.set_terminator(return_true, Terminator::Return { values: vec![one] });

        let func = self.module.funcs.push(FuncDecl::Body(
            signature,
            format!("js_instanceof_{}", self.module.funcs.len()),
            body,
        ));
        self.instanceof_helper = Some(func);
        Ok(func)
    }

    /// `lhs instanceof rhs` via the generated chain-walk helper.
    fn instance_of(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        left: &LowerValue,
        right: &LowerValue,
    ) -> Result<LowerValue, ConvertError> {
        let func = self.ensure_instanceof()?;
        let left = self.box_value(body, block, left)?;
        let right = self.box_value(body, block, right)?;
        let value = body.add_op(
            block,
            Operator::Call {
                function_index: func,
            },
            &[left, right],
            &[Type::I32],
        );
        Ok(LowerValue::Wasm {
            value,
            kind: ValueKind::Boolean,
        })
    }

    /// A fresh member context: an empty object whose prototype is the
    /// enclosing context (so captured variables still resolve through the
    /// chain lookup) carrying the hidden `\x01home` binding for `super`.
    fn class_member_context(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        home: &LowerValue,
    ) -> Result<(Block, Value), ConvertError> {
        let ctx = self.new_object(body, block)?;
        let ctx_value = ctx.wasm()?.0;
        self.set_object_proto(body, block, ctx_value, context);
        let block = self.set_static_property_value_raw(body, block, &ctx, "\u{1}home", home)?;
        Ok((block, ctx_value))
    }

    /// Lower `Item::Class`: build the constructor/prototype pair, wire the
    /// `extends` chain, and define every member. Result value is the
    /// constructor function.
    fn class_value(
        &mut self,
        body: &mut FunctionBody,
        mut block: Block,
        context: Value,
        this: LowerValue,
        values: &ContinuationValues,
        class: &'a portal_jsc_swc_tac::TClass<SValueId, SFunc>,
        span: &impl std::fmt::Debug,
    ) -> Result<(Block, LowerValue), ConvertError> {
        use portal_jsc_swc_tac::{MemberFlags, PrivateKind};

        let superclass = match &class.superclass {
            Some(id) => Some(
                values
                    .get(id)
                    .cloned()
                    .ok_or_else(|| ConvertError::invalid("undefined superclass value"))?,
            ),
            None => None,
        };
        let has_super = superclass.is_some();

        // The constructor's context: a fresh child of the enclosing context
        // carrying the hidden `\x01super` binding used by `TCallee::Super`.
        let ctor_ctx = self.new_object(body, block)?;
        let ctor_ctx_value = ctor_ctx.wasm()?.0;
        self.set_object_proto(body, block, ctor_ctx_value, context);
        let super_binding = superclass.clone().unwrap_or_else(|| self.undef(body, block));
        block = self.set_static_property_value_raw(
            body,
            block,
            &ctor_ctx,
            "\u{1}super",
            &super_binding,
        )?;

        // The prototype object; its [[Prototype]] is the superclass's
        // `prototype` property (or null for a base class).
        let proto = self.new_object(body, block)?;
        let proto_value = proto.wasm()?.0;
        if let Some(sup) = &superclass {
            let (b, sup_proto) = self.get_property_value_raw(body, block, sup, "prototype")?;
            block = b;
            let (sup_proto_value, _) = sup_proto.wasm()?;
            self.set_object_proto(body, block, proto_value, sup_proto_value);
        }

        // The constructor function object. A class without an explicit
        // constructor gets the generated default (base or derived).
        let ctor = match &class.constructor {
            Some(func) => {
                let undef = self.undef(body, block);
                self.function_object(body, block, ctor_ctx_value, undef, func, false)?
            }
            None => {
                let helper = self.ensure_default_ctor(has_super)?;
                self.function_object_for_native(body, block, ctor_ctx_value, helper)?
            }
        };
        // Instance private members (#33): when the class declares any, the
        // class's callable identity becomes a generated wrapper constructor
        // that installs the hidden slots on the receiver (values staged in
        // the wrapper's context) before tail-calling the real constructor.
        let mut private_instance_names: Vec<String> = Vec::new();
        for (flags, key, _prop) in &class.members {
            if flags.contains(MemberFlags::STATIC) {
                continue;
            }
            if let PropKey::Lit(sym) = key
                && let PrivateKind::Private(ctxt) = &sym.ctx
            {
                let name = Self::private_slot_name(&sym.sym, *ctxt);
                if !private_instance_names.contains(&name) {
                    private_instance_names.push(name);
                }
            }
        }
        let mut wrapper_ctx: Option<LowerValue> = None;
        let ctor = if private_instance_names.is_empty() {
            ctor
        } else {
            let wctx = self.new_object(body, block)?;
            let wctx_value = wctx.wasm()?.0;
            self.set_object_proto(body, block, wctx_value, context);
            block =
                self.set_static_property_value_raw(body, block, &wctx, "\u{1}priv_target", &ctor)?;
            let wrapper = self.private_ctor_wrapper(&private_instance_names)?;
            let wrapped = self.function_object_for_native(body, block, wctx_value, wrapper)?;
            wrapper_ctx = Some(wctx);
            wrapped
        };
        let (ctor_value, _) = ctor.clone().wasm()?;
        if let Some(sup) = &superclass {
            let (sup_value, _) = sup.clone().wasm()?;
            self.set_function_proto(body, block, ctor_value, sup_value);
        }
        block = self.set_static_property_value_raw(body, block, &ctor, "prototype", &proto)?;
        block = self.set_static_property_value_raw(body, block, &proto, "constructor", &ctor)?;

        // Members. Instance members land on the prototype object, statics on
        // the constructor; each side shares one lazily-created member context
        // carrying its `\x01home` (the prototype, or the constructor).
        let mut instance_ctx: Option<Value> = None;
        let mut static_ctx: Option<Value> = None;
        let mut instance_accessors: std::collections::BTreeMap<String, Value> =
            std::collections::BTreeMap::new();
        let mut static_accessors: std::collections::BTreeMap<String, Value> =
            std::collections::BTreeMap::new();
        for (flags, key, prop) in &class.members {
            let is_static = flags.contains(MemberFlags::STATIC);
            // Private members (#33) resolve to hidden own-slot names derived
            // from the hygiene pair. Statics install on the constructor (the
            // wrapper, when one exists); instance members stage their value
            // in the wrapper's context under `\x01privv<ordinal>`, and the
            // generated wrapper constructor installs them per receiver. A
            // getter/setter pair shares one slot name and one ordinal.
            let mut private_member = false;
            let key_name: Option<String> = match key {
                PropKey::Lit(sym) => match &sym.ctx {
                    PrivateKind::Public => Some(sym.sym.to_string()),
                    PrivateKind::Private(ctxt) => {
                        private_member = true;
                        let hidden = Self::private_slot_name(&sym.sym, *ctxt);
                        if is_static {
                            Some(hidden)
                        } else {
                            let ordinal = private_instance_names
                                .iter()
                                .position(|name| name == &hidden)
                                .ok_or_else(|| {
                                    ConvertError::invalid("private member missing from the pre-scan")
                                })?;
                            Some(format!("\u{1}privv{ordinal}"))
                        }
                    }
                },
                PropKey::Computed(_) => None,
                _ => {
                    return Err(ConvertError::unsupported(
                        "unknown class member key form",
                        span,
                    ));
                }
            };
            let target = if private_member && !is_static {
                wrapper_ctx.as_ref().ok_or_else(|| {
                    ConvertError::invalid("private member without a wrapper context")
                })?
            } else if is_static {
                &ctor
            } else {
                &proto
            };
            let member_ctx = match prop {
                PropVal::Method(_) | PropVal::Getter(_) | PropVal::Setter(_) => {
                    let slot = if is_static {
                        &mut static_ctx
                    } else {
                        &mut instance_ctx
                    };
                    if slot.is_none() {
                        let home = if is_static { &ctor } else { &proto };
                        let (b, value) =
                            self.class_member_context(body, block, context, home)?;
                        block = b;
                        *slot = Some(value);
                    }
                    *slot.as_ref().unwrap()
                }
                _ => context,
            };
            match prop {
                PropVal::Method(func) => {
                    let undef = self.undef(body, block);
                    let method = self.function_object(
                        body,
                        block,
                        member_ctx,
                        undef,
                        func,
                        false,
                    )?;
                    block = match &key_name {
                        Some(name) => {
                            self.set_static_property_value_raw(body, block, target, name, &method)?
                        }
                        None => {
                            let PropKey::Computed(id) = key else {
                                unreachable!("key_name None implies computed key")
                            };
                            let key_value = values.get(id).ok_or_else(|| {
                                ConvertError::invalid("undefined computed class member key")
                            })?;
                            let key_string =
                                self.dynamic_string_key(body, block, key_value)?;
                            let blocks =
                                self.set_string_member(body, block, target, key_string, &method)?;
                            self.join_write_continuations(body, blocks)
                        }
                    };
                }
                PropVal::Getter(func) | PropVal::Setter(func) => {
                    let Some(name) = &key_name else {
                        return Err(ConvertError::unsupported(
                            "computed accessor keys in classes",
                            span,
                        ));
                    };
                    let accessors = if is_static {
                        &mut static_accessors
                    } else {
                        &mut instance_accessors
                    };
                    let descriptor = match accessors.get(name) {
                        Some(descriptor) => *descriptor,
                        None => {
                            let descriptor = self.new_descriptor(body, block);
                            let descriptor_value = self.anyref(body, block, descriptor);
                            let descriptor_value = LowerValue::Wasm {
                                value: descriptor_value,
                                kind: ValueKind::Reference,
                            };
                            block = self.set_static_property_value_raw(
                                body,
                                block,
                                target,
                                name,
                                &descriptor_value,
                            )?;
                            accessors.insert(name.clone(), descriptor);
                            descriptor
                        }
                    };
                    let undef = self.undef(body, block);
                    let function = self.function_object(
                        body,
                        block,
                        member_ctx,
                        undef,
                        func,
                        false,
                    )?;
                    let function = self.box_value(body, block, &function)?;
                    let index = match prop {
                        PropVal::Getter(_) => 0,
                        PropVal::Setter(_) => 1,
                        _ => unreachable!("accessor arm only matches getter/setter"),
                    };
                    body.add_op(
                        block,
                        Operator::StructSet {
                            sig: self.repr.descriptor,
                            idx: index,
                        },
                        &[descriptor, function],
                        &[],
                    );
                }
                PropVal::Item(initializer) => {
                    // Field initializers: evaluated at class-creation time
                    // (swc-tac design point); instance fields land on the
                    // prototype (constructor assignments shadow them per
                    // instance), statics on the constructor.
                    let Some(name) = &key_name else {
                        return Err(ConvertError::unsupported(
                            "computed field keys in classes",
                            span,
                        ));
                    };
                    let initializer = match initializer {
                        Some(id) => values.get(id).cloned().ok_or_else(|| {
                            ConvertError::invalid("undefined class field initializer")
                        })?,
                        // A private field without an initializer still
                        // installs its slot: the slot's presence is the
                        // brand used by `#x in obj`.
                        None if private_member => self.undef(body, block),
                        None => continue, // `field;` reads as undefined anyway
                    };
                    block =
                        self.set_static_property_value_raw(body, block, target, name, &initializer)?;
                }
                other => {
                    return Err(ConvertError::unsupported(
                        format!("class member {other:?}"),
                        span,
                    ));
                }
            }
        }
        Ok((block, ctor))
    }
}
