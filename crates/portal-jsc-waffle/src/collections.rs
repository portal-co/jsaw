// `Map`/`Set`/`WeakMap`/`WeakSet` primordials (DreamComp #32).
// `include!`d into `impl Converter` in `conv.rs` alongside `primordials.rs`.
//
// Representation: a collection instance is an ordinary object whose
// `typed_data` field (object field 3) holds a `map_entries` array of
// `{key, value}` pairs (Set/WeakSet use the key slot only). Methods live on
// a per-constructor prototype object — discovered through the ordinary
// prototype-chain read path from #31 — and are tagged function objects
// wrapping one generated dispatcher per collection kind; the numeric method
// selector rides the method function object's captured context under the
// hidden `\u{1}op` binding. Lookups use SameValueZero (boxed numbers compare
// by value with NaN == NaN; strings by content; everything else by
// identity).
//
// WeakMap/WeakSet are *emulated* as Map/Set in this ordinary-primordial
// tier: entries are strong. The `~weakmap` native-intrinsic tier instead
// stores each entry in a key-owned hidden slot, so no native table retains a
// key; that is the weak ownership edge used by both WasmGC and nano lowering.
// The emulation/native split is documented in the public js-language-spec.

/// Method selectors riding the hidden `\u{1}op` context binding.
const MAP_OP_GET: i32 = 0;
const MAP_OP_SET: i32 = 1;
const MAP_OP_HAS: i32 = 2;
const MAP_OP_DELETE: i32 = 3;
const MAP_OP_SIZE: i32 = 4;
const MAP_OP_CLEAR: i32 = 5;

const SET_OP_ADD: i32 = 0;
const SET_OP_HAS: i32 = 1;
const SET_OP_DELETE: i32 = 2;
const SET_OP_SIZE: i32 = 3;
const SET_OP_CLEAR: i32 = 4;

const WEAKMAP_OP_GET: i32 = 0;
const WEAKMAP_OP_SET: i32 = 1;
const WEAKMAP_OP_HAS: i32 = 2;
const WEAKMAP_OP_DELETE: i32 = 3;

const WEAKSET_OP_ADD: i32 = 0;
const WEAKSET_OP_HAS: i32 = 1;
const WEAKSET_OP_DELETE: i32 = 2;

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// `ref null` as `anyref` (JS `undefined`).
    fn null_ref(&self, body: &mut FunctionBody, block: Block) -> Value {
        body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        )
    }

    /// A boxed boolean constant (0/1) as `anyref`.
    fn bool_ref(&self, body: &mut FunctionBody, block: Block, truth: bool) -> Value {
        let v = body.add_op(
            block,
            Operator::I32Const {
                value: u32::from(truth),
            },
            &[],
            &[Type::I32],
        );
        let b = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.boolean,
            },
            &[v],
            &[ref_sig(self.repr.boolean)],
        );
        body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.value,
            },
            &[b],
            &[self.repr.value],
        )
    }

    /// A boxed number from a raw f64 as `anyref`.
    fn number_ref(&self, body: &mut FunctionBody, block: Block, f: Value) -> Value {
        let n = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.number,
            },
            &[f],
            &[ref_sig(self.repr.number)],
        );
        body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.value,
            },
            &[n],
            &[self.repr.value],
        )
    }

    /// Unbox an `anyref` number to f64 (traps on non-number).
    fn unbox_number(&self, body: &mut FunctionBody, block: Block, value: Value) -> Value {
        let n = body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.number_ty(),
            },
            &[value],
            &[self.repr.number_ty()],
        );
        body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.number,
                idx: 0,
            },
            &[n],
            &[Type::F64],
        )
    }

    /// The generated SameValueZero helper: `js_same_value_zero(a, b) -> i32`.
    /// NaN equals NaN; boxed numbers compare by value; strings by content;
    /// everything else by reference identity.
    fn ensure_same_value_zero(&mut self) -> Result<Func, ConvertError> {
        if let Some(func) = self.same_value_zero {
            return Ok(func);
        }
        let string_equal = self.ensure_string_equal()?;
        let signature = self.module.signatures.push(SignatureData::Func {
            params: vec![self.repr.value, self.repr.value],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, signature);
        let entry = body.entry;
        let a = body.blocks[entry].params[0].1;
        let b = body.blocks[entry].params[1].1;
        let ret = |body: &mut FunctionBody, block: Block, v: i32| {
            let v = body.add_op(
                block,
                Operator::I32Const { value: v as u32 },
                &[],
                &[Type::I32],
            );
            body.set_terminator(block, Terminator::Return { values: vec![v] });
        };

        // ref_eq covers identity and null == null.
        let eq_non_null = Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Eq,
            nullable: false,
        });
        let both_null = body.add_block();
        let a_null = body.add_op(entry, Operator::RefIsNull, &[a], &[Type::I32]);
        let not_null = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: a_null,
                if_true: BlockTarget {
                    block: both_null,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_null,
                    args: vec![],
                },
            },
        );
        // both_null arm: equal iff b is also null.
        let b_null = body.add_op(both_null, Operator::RefIsNull, &[b], &[Type::I32]);
        body.set_terminator(
            both_null,
            Terminator::Return {
                values: vec![b_null],
            },
        );

        // Check boxed primitive payloads before attempting identity. Mutable
        // GC structs such as Number are not necessarily in the `eqref`
        // hierarchy, so casting arbitrary `anyref`s to eqref first can trap.
        let check_number = not_null;
        // both number?
        let a_num = body.add_op(
            check_number,
            Operator::RefTest {
                ty: self.repr.number_non_null_ty(),
            },
            &[a],
            &[Type::I32],
        );
        let numbers = body.add_block();
        let check_string = body.add_block();
        body.set_terminator(
            check_number,
            Terminator::CondBr {
                cond: a_num,
                if_true: BlockTarget {
                    block: numbers,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: check_string,
                    args: vec![],
                },
            },
        );
        let b_num = body.add_op(
            numbers,
            Operator::RefTest {
                ty: self.repr.number_non_null_ty(),
            },
            &[b],
            &[Type::I32],
        );
        let both_numbers = body.add_block();
        let fail_number = body.add_block();
        body.set_terminator(
            numbers,
            Terminator::CondBr {
                cond: b_num,
                if_true: BlockTarget {
                    block: both_numbers,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fail_number,
                    args: vec![],
                },
            },
        );
        let fa = self.unbox_number(&mut body, both_numbers, a);
        let fb = self.unbox_number(&mut body, both_numbers, b);
        let eq = body.add_op(both_numbers, Operator::F64Eq, &[fa, fb], &[Type::I32]);
        let nan_a = body.add_op(both_numbers, Operator::F64Ne, &[fa, fa], &[Type::I32]);
        let nan_b = body.add_op(both_numbers, Operator::F64Ne, &[fb, fb], &[Type::I32]);
        let both_nan = body.add_op(
            both_numbers,
            Operator::I32And,
            &[nan_a, nan_b],
            &[Type::I32],
        );
        let equal = body.add_op(both_numbers, Operator::I32Or, &[eq, both_nan], &[Type::I32]);
        body.set_terminator(
            both_numbers,
            Terminator::Return {
                values: vec![equal],
            },
        );
        ret(&mut body, fail_number, 0);
        // both string?
        let a_str = body.add_op(
            check_string,
            Operator::RefTest {
                ty: self.repr.string_ty(),
            },
            &[a],
            &[Type::I32],
        );
        let strings = body.add_block();
        let check_bool = body.add_block();
        body.set_terminator(
            check_string,
            Terminator::CondBr {
                cond: a_str,
                if_true: BlockTarget {
                    block: strings,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: check_bool,
                    args: vec![],
                },
            },
        );
        let b_str = body.add_op(
            strings,
            Operator::RefTest {
                ty: self.repr.string_ty(),
            },
            &[b],
            &[Type::I32],
        );
        let both_strings = body.add_block();
        let fail_string = body.add_block();
        body.set_terminator(
            strings,
            Terminator::CondBr {
                cond: b_str,
                if_true: BlockTarget {
                    block: both_strings,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fail_string,
                    args: vec![],
                },
            },
        );
        let sa = body.add_op(
            both_strings,
            Operator::RefCast {
                ty: self.repr.string_ty(),
            },
            &[a],
            &[self.repr.string_ty()],
        );
        let sb = body.add_op(
            both_strings,
            Operator::RefCast {
                ty: self.repr.string_ty(),
            },
            &[b],
            &[self.repr.string_ty()],
        );
        let equal = body.add_op(
            both_strings,
            Operator::Call {
                function_index: string_equal,
            },
            &[sa, sb],
            &[Type::I32],
        );
        body.set_terminator(
            both_strings,
            Terminator::Return {
                values: vec![equal],
            },
        );
        ret(&mut body, fail_string, 0);
        // both boolean?
        let a_bool = body.add_op(
            check_bool,
            Operator::RefTest {
                ty: self.repr.boolean_ty(),
            },
            &[a],
            &[Type::I32],
        );
        let bools = body.add_block();
        let fail_bool = body.add_block();
        body.set_terminator(
            check_bool,
            Terminator::CondBr {
                cond: a_bool,
                if_true: BlockTarget {
                    block: bools,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fail_bool,
                    args: vec![],
                },
            },
        );
        let b_bool = body.add_op(
            bools,
            Operator::RefTest {
                ty: self.repr.boolean_non_null_ty(),
            },
            &[b],
            &[Type::I32],
        );
        let both_bools = body.add_block();
        let not_bools = body.add_block();
        body.set_terminator(
            bools,
            Terminator::CondBr {
                cond: b_bool,
                if_true: BlockTarget {
                    block: both_bools,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_bools,
                    args: vec![],
                },
            },
        );
        let ba = body.add_op(
            both_bools,
            Operator::RefCast {
                ty: self.repr.boolean_ty(),
            },
            &[a],
            &[self.repr.boolean_ty()],
        );
        let bb = body.add_op(
            both_bools,
            Operator::RefCast {
                ty: self.repr.boolean_ty(),
            },
            &[b],
            &[self.repr.boolean_ty()],
        );
        let va = body.add_op(
            both_bools,
            Operator::StructGet {
                sig: self.repr.boolean,
                idx: 0,
            },
            &[ba],
            &[Type::I32],
        );
        let vb = body.add_op(
            both_bools,
            Operator::StructGet {
                sig: self.repr.boolean,
                idx: 0,
            },
            &[bb],
            &[Type::I32],
        );
        let equal = body.add_op(both_bools, Operator::I32Eq, &[va, vb], &[Type::I32]);
        body.set_terminator(
            both_bools,
            Terminator::Return {
                values: vec![equal],
            },
        );
        body.set_terminator(
            not_bools,
            Terminator::Br {
                target: BlockTarget {
                    block: fail_bool,
                    args: vec![],
                },
            },
        );
        // If `a` is an object/function/i31 and neither side was a boxed
        // primitive, compare identity. The preceding type tests ensure a
        // number/string/boolean cannot reach these eqref casts on either side.
        let b_null = body.add_op(fail_bool, Operator::RefIsNull, &[b], &[Type::I32]);
        let identity = body.add_block();
        let not_identity = body.add_block();
        body.set_terminator(
            fail_bool,
            Terminator::CondBr {
                cond: b_null,
                if_true: BlockTarget {
                    block: not_identity,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: identity,
                    args: vec![],
                },
            },
        );
        ret(&mut body, not_identity, 0);
        let ref_eq = body.add_op(
            identity,
            Operator::RefCast { ty: eq_non_null },
            &[a],
            &[eq_non_null],
        );
        let ref_eq_b = body.add_op(
            identity,
            Operator::RefCast { ty: eq_non_null },
            &[b],
            &[eq_non_null],
        );
        let identical = body.add_op(identity, Operator::RefEq, &[ref_eq, ref_eq_b], &[Type::I32]);
        body.set_terminator(
            identity,
            Terminator::Return {
                values: vec![identical],
            },
        );

        let func = self.module.funcs.push(FuncDecl::Body(
            signature,
            format!("js_same_value_zero_{}", self.module.funcs.len()),
            body,
        ));
        self.same_value_zero = Some(func);
        Ok(func)
    }

    /// The generated find helper: `js_map_find(entries, key) -> i32`
    /// (index of the entry whose key is SameValueZero-equal, else -1).
    fn ensure_map_find(&mut self) -> Result<Func, ConvertError> {
        if let Some(func) = self.map_find_helper {
            return Ok(func);
        }
        let svz = self.ensure_same_value_zero()?;
        let signature = self.module.signatures.push(SignatureData::Func {
            params: vec![self.repr.map_entries_ty(), self.repr.value],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, signature);
        let entry = body.entry;
        let entries = body.blocks[entry].params[0].1;
        let key = body.blocks[entry].params[1].1;
        let len = body.add_op(entry, Operator::ArrayLen, &[entries], &[Type::I32]);
        let loop_block = body.add_block();
        let i = body.add_blockparam(loop_block, Type::I32);
        let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            entry,
            Terminator::Br {
                target: BlockTarget {
                    block: loop_block,
                    args: vec![zero],
                },
            },
        );
        let done = body.add_block();
        let hit = body.add_block();
        let in_bounds = body.add_op(loop_block, Operator::I32LtU, &[i, len], &[Type::I32]);
        let have_entry = body.add_block();
        body.set_terminator(
            loop_block,
            Terminator::CondBr {
                cond: in_bounds,
                if_true: BlockTarget {
                    block: have_entry,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        let pair = body.add_op(
            have_entry,
            Operator::ArrayGet {
                sig: self.repr.map_entries,
            },
            &[entries, i],
            &[self.repr.map_entry_ty()],
        );
        let candidate = body.add_op(
            have_entry,
            Operator::StructGet {
                sig: self.repr.map_entry,
                idx: 0,
            },
            &[pair],
            &[self.repr.value],
        );
        let equal = body.add_op(
            have_entry,
            Operator::Call {
                function_index: svz,
            },
            &[candidate, key],
            &[Type::I32],
        );
        let next = body.add_block();
        body.set_terminator(
            have_entry,
            Terminator::CondBr {
                cond: equal,
                if_true: BlockTarget {
                    block: hit,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: next,
                    args: vec![],
                },
            },
        );
        let one = body.add_op(next, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let inc = body.add_op(next, Operator::I32Add, &[i, one], &[Type::I32]);
        body.set_terminator(
            next,
            Terminator::Br {
                target: BlockTarget {
                    block: loop_block,
                    args: vec![inc],
                },
            },
        );
        body.set_terminator(hit, Terminator::Return { values: vec![i] });
        let minus_one = body.add_op(
            done,
            Operator::I32Const {
                value: -1i32 as u32,
            },
            &[],
            &[Type::I32],
        );
        body.set_terminator(
            done,
            Terminator::Return {
                values: vec![minus_one],
            },
        );
        let func = self.module.funcs.push(FuncDecl::Body(
            signature,
            format!("js_map_find_{}", self.module.funcs.len()),
            body,
        ));
        self.map_find_helper = Some(func);
        Ok(func)
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// Read `args[i]` from an adapter arguments array; `undefined` (null
    /// anyref) when out of bounds.
    fn arg_or_undef(
        &self,
        body: &mut FunctionBody,
        block: Block,
        args: Value,
        index: u32,
    ) -> (Block, Value) {
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        let len = body.add_op(block, Operator::ArrayLen, &[args], &[Type::I32]);
        let idx = body.add_op(
            block,
            Operator::I32Const { value: index },
            &[],
            &[Type::I32],
        );
        let present = body.add_op(block, Operator::I32LtU, &[idx, len], &[Type::I32]);
        let yes = body.add_block();
        let no = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: present,
                if_true: BlockTarget {
                    block: yes,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: no,
                    args: vec![],
                },
            },
        );
        let v = body.add_op(
            yes,
            Operator::ArrayGet {
                sig: self.repr.arguments,
            },
            &[args, idx],
            &[self.repr.value],
        );
        body.set_terminator(
            yes,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![v],
                },
            },
        );
        let undef = self.null_ref(body, no);
        body.set_terminator(
            no,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![undef],
                },
            },
        );
        (join, result)
    }

    /// The numeric method selector from the dispatcher's captured context
    /// (hidden `\u{1}op` binding installed at method-object creation).
    fn method_selector(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        ctx: Value,
    ) -> Result<Value, ConvertError> {
        let chain = self.ensure_chain_lookup()?;
        let key = self.new_string(body, block, b"\x01op")?;
        let slot = body.add_op(
            block,
            Operator::Call {
                function_index: chain,
            },
            &[ctx, key],
            &[self.repr.slot_ty()],
        );
        let v = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.slot,
                idx: 0,
            },
            &[slot],
            &[self.repr.value],
        );
        let f = self.unbox_number(body, block, v);
        Ok(body.add_op(block, Operator::I32TruncF64S, &[f], &[Type::I32]))
    }

    /// `this` cast to an object; the entries array from field 3, creating
    /// (and storing) an empty one when null. Returns the entries value.
    fn collection_entries(
        &self,
        body: &mut FunctionBody,
        block: Block,
        this: Value,
    ) -> (Block, Value, Value) {
        let this_obj = body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[this],
            &[self.repr.object_ty()],
        );
        let raw = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 3,
            },
            &[this_obj],
            &[self.repr.value],
        );
        let join = body.add_block();
        let entries = body.add_blockparam(join, self.repr.map_entries_ty());
        let missing = body.add_op(block, Operator::RefIsNull, &[raw], &[Type::I32]);
        let init = body.add_block();
        let have = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: missing,
                if_true: BlockTarget {
                    block: init,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: have,
                    args: vec![],
                },
            },
        );
        let empty = body.add_op(
            init,
            Operator::ArrayNewFixed {
                sig: self.repr.map_entries,
                num: 0,
            },
            &[],
            &[self.repr.map_entries_ty()],
        );
        let as_value = body.add_op(
            init,
            Operator::RefCast {
                ty: self.repr.value,
            },
            &[empty],
            &[self.repr.value],
        );
        body.add_op(
            init,
            Operator::StructSet {
                sig: self.repr.object,
                idx: 3,
            },
            &[this_obj, as_value],
            &[],
        );
        body.set_terminator(
            init,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![empty],
                },
            },
        );
        let existing = body.add_op(
            have,
            Operator::RefCast {
                ty: self.repr.map_entries_ty(),
            },
            &[raw],
            &[self.repr.map_entries_ty()],
        );
        body.set_terminator(
            have,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![existing],
                },
            },
        );
        (join, entries, this_obj)
    }

    /// Copy `entries` into a fresh array of `new_len`, inserting `(key,
    /// value)` at `len` (append) or skipping index `skip` (delete). Shared
    /// by the SET/DELETE method arms.
    fn entries_with_mutation(
        &self,
        body: &mut FunctionBody,
        block: Block,
        entries: Value,
        len: Value,
        skip: Option<Value>,
        new_entry: Option<(Value, Value)>,
    ) -> (Block, Value) {
        let new_len = match (skip, &new_entry) {
            (Some(_), None) => {
                let one = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                body.add_op(block, Operator::I32Sub, &[len, one], &[Type::I32])
            }
            (None, Some(_)) => {
                let one = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                body.add_op(block, Operator::I32Add, &[len, one], &[Type::I32])
            }
            _ => unreachable!("exactly one of skip/new_entry"),
        };
        let fresh = body.add_op(
            block,
            Operator::ArrayNewDefault {
                sig: self.repr.map_entries,
            },
            &[new_len],
            &[self.repr.map_entries_ty()],
        );
        let loop_block = body.add_block();
        let i = body.add_blockparam(loop_block, Type::I32);
        let done = body.add_block();
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            block,
            Terminator::Br {
                target: BlockTarget {
                    block: loop_block,
                    args: vec![zero],
                },
            },
        );
        let in_bounds = body.add_op(loop_block, Operator::I32LtU, &[i, len], &[Type::I32]);
        let copy = body.add_block();
        body.set_terminator(
            loop_block,
            Terminator::CondBr {
                cond: in_bounds,
                if_true: BlockTarget {
                    block: copy,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        let pair = body.add_op(
            copy,
            Operator::ArrayGet {
                sig: self.repr.map_entries,
            },
            &[entries, i],
            &[self.repr.map_entry_ty()],
        );
        // Destination index: identity for append; i or i-1 for delete.
        let dest = if let Some(skip_idx) = skip {
            let is_skipped = body.add_op(copy, Operator::I32Eq, &[i, skip_idx], &[Type::I32]);
            let one = body.add_op(copy, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let decremented = body.add_op(copy, Operator::I32Sub, &[i, one], &[Type::I32]);
            let write = body.add_block();
            let advance = body.add_block();
            body.set_terminator(
                copy,
                Terminator::CondBr {
                    cond: is_skipped,
                    if_true: BlockTarget {
                        block: advance,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: write,
                        args: vec![],
                    },
                },
            );
            // Only entries after the removed one shift left. For entries
            // before it, retain the original index (i=0 must not underflow).
            let after = body.add_op(write, Operator::I32GtU, &[i, skip_idx], &[Type::I32]);
            let dest = body.add_op(
                write,
                Operator::TypedSelect { ty: Type::I32 },
                &[decremented, i, after],
                &[Type::I32],
            );
            body.add_op(
                write,
                Operator::ArraySet {
                    sig: self.repr.map_entries,
                },
                &[fresh, dest, pair],
                &[],
            );
            body.set_terminator(
                write,
                Terminator::Br {
                    target: BlockTarget {
                        block: advance,
                        args: vec![],
                    },
                },
            );
            // The common increment/loop edge below is emitted on the
            // returned continuation block.
            advance
        } else {
            body.add_op(
                copy,
                Operator::ArraySet {
                    sig: self.repr.map_entries,
                },
                &[fresh, i, pair],
                &[],
            );
            copy
        };
        let one = body.add_op(dest, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let inc = body.add_op(dest, Operator::I32Add, &[i, one], &[Type::I32]);
        body.set_terminator(
            dest,
            Terminator::Br {
                target: BlockTarget {
                    block: loop_block,
                    args: vec![inc],
                },
            },
        );
        // Append the new entry at `len` for the SET arm.
        if let Some((key, value)) = new_entry {
            let entry = body.add_op(
                done,
                Operator::StructNew {
                    sig: self.repr.map_entry,
                },
                &[key, value],
                &[self.repr.map_entry_ty()],
            );
            body.add_op(
                done,
                Operator::ArraySet {
                    sig: self.repr.map_entries,
                },
                &[fresh, len, entry],
                &[],
            );
        }
        (done, fresh)
    }

    /// The collection helper functions (#32). Method dispatchers are shared:
    /// Map and WeakMap reuse one table core; Set and WeakSet another; the
    /// Weak* variants add the JS primitive-key guard.
    fn ensure_collection_helpers(&mut self) -> Result<CollectionHelpers, ConvertError> {
        if let Some(helpers) = self.collection_helpers {
            return Ok(helpers);
        }
        let find = self.ensure_map_find()?;

        // js_native_map_new: this.typed_data = empty entries; return undefined.
        let adapter = self.repr.adapter;
        let map_new = {
            let mut body = FunctionBody::new(self.module, adapter);
            let entry = body.entry;
            let this = body.blocks[entry].params[1].1;
            let this_obj = body.add_op(
                entry,
                Operator::RefCast {
                    ty: self.repr.object_ty(),
                },
                &[this],
                &[self.repr.object_ty()],
            );
            let empty = body.add_op(
                entry,
                Operator::ArrayNewFixed {
                    sig: self.repr.map_entries,
                    num: 0,
                },
                &[],
                &[self.repr.map_entries_ty()],
            );
            let as_value = body.add_op(
                entry,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[empty],
                &[self.repr.value],
            );
            body.add_op(
                entry,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            // Field 4 is the typed-array brand tag. -1 marks collection
            // instances so the typed-array `set`/index dispatch does not
            // mistake their backing entries array for typed storage.
            let collection_tag = body.add_op(
                entry,
                Operator::I32Const { value: u32::MAX },
                &[],
                &[Type::I32],
            );
            body.add_op(
                entry,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 4,
                },
                &[this_obj, collection_tag],
                &[],
            );
            let undef = self.null_ref(&mut body, entry);
            body.set_terminator(
                entry,
                Terminator::Return {
                    values: vec![undef],
                },
            );
            let func = self.module.funcs.push(FuncDecl::Body(
                adapter,
                format!("js_native_map_new_{}", self.module.funcs.len()),
                body,
            ));
            self.declare_function_reference(func);
            func
        };

        // js_map_method (Map core): GET/SET/HAS/DELETE/SIZE/CLEAR.
        let map_method = self.build_map_method_dispatcher(false, find)?;
        // js_weakmap_method: same table core with the primitive-key guard.
        let weakmap_method = self.build_map_method_dispatcher(true, find)?;
        // js_set_method / js_weakset_method: key-only core (+ guard).
        let set_method = self.build_set_method_dispatcher(false, find)?;
        let weakset_method = self.build_set_method_dispatcher(true, find)?;

        let helpers = CollectionHelpers {
            map_new,
            map_method,
            set_new: map_new,
            set_method,
            weakmap_new: map_new,
            weakmap_method,
            weakset_new: map_new,
            weakset_method,
        };
        self.collection_helpers = Some(helpers);
        Ok(helpers)
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// Build the Map/WeakMap method dispatcher. When `weak` is set, the
    /// primitive-key guard is emitted first (GET → undefined, HAS → false,
    /// SET/DELETE → trap, per JS WeakMap semantics).
    fn build_map_method_dispatcher(
        &mut self,
        weak: bool,
        find: Func,
    ) -> Result<Func, ConvertError> {
        let adapter = self.repr.adapter;
        let mut body = FunctionBody::new(self.module, adapter);
        let entry = body.entry;
        let ctx = body.blocks[entry].params[0].1;
        let this = body.blocks[entry].params[1].1;
        let args = body.blocks[entry].params[2].1;
        let op = self.method_selector(&mut body, entry, ctx)?;
        let (block, key) = self.arg_or_undef(&mut body, entry, args, 0);
        let (block, value) = self.arg_or_undef(&mut body, block, args, 1);

        // Weak-key guard.
        let guarded = if weak {
            let is_obj = body.add_op(
                block,
                Operator::RefTest {
                    ty: self.repr.object_non_null_ty(),
                },
                &[key],
                &[Type::I32],
            );
            let is_fn = body.add_op(
                block,
                Operator::RefTest {
                    ty: self.repr.function_non_null_ty(),
                },
                &[key],
                &[Type::I32],
            );
            let ok = body.add_op(block, Operator::I32Or, &[is_obj, is_fn], &[Type::I32]);
            // Allowed ops on primitive keys: GET (undefined) and HAS (false).
            let get_op = body.add_op(
                block,
                Operator::I32Const {
                    value: WEAKMAP_OP_GET as u32,
                },
                &[],
                &[Type::I32],
            );
            let is_get = body.add_op(block, Operator::I32Eq, &[op, get_op], &[Type::I32]);
            let has_op = body.add_op(
                block,
                Operator::I32Const {
                    value: WEAKMAP_OP_HAS as u32,
                },
                &[],
                &[Type::I32],
            );
            let is_has = body.add_op(block, Operator::I32Eq, &[op, has_op], &[Type::I32]);
            let allowed = body.add_op(block, Operator::I32Or, &[is_get, is_has], &[Type::I32]);
            let primitive_ok = body.add_block();
            let primitive_fail = body.add_block();
            let proceed = body.add_block();
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: ok,
                    if_true: BlockTarget {
                        block: proceed,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: primitive_ok,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(
                primitive_ok,
                Terminator::CondBr {
                    cond: allowed,
                    if_true: BlockTarget {
                        block: proceed,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: primitive_fail,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(primitive_fail, Terminator::Unreachable);
            proceed
        } else {
            block
        };

        let (block, entries, this_obj) = self.collection_entries(&mut body, guarded, this);
        let len = body.add_op(block, Operator::ArrayLen, &[entries], &[Type::I32]);
        let found = body.add_op(
            block,
            Operator::Call {
                function_index: find,
            },
            &[entries, key],
            &[Type::I32],
        );
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let hit = body.add_op(block, Operator::I32GeS, &[found, zero], &[Type::I32]);

        // Dispatch: GET(0) / SET(1) / HAS(2) / DELETE(3) / SIZE(4) / CLEAR(5).
        fn op_const(body: &mut FunctionBody, b: Block, v: i32) -> Value {
            body.add_op(b, Operator::I32Const { value: v as u32 }, &[], &[Type::I32])
        }
        let get_block = body.add_block();
        let set_block = body.add_block();
        let has_block = body.add_block();
        let delete_block = body.add_block();
        let size_block = body.add_block();
        let clear_block = body.add_block();
        let trap_block = body.add_block();
        body.set_terminator(trap_block, Terminator::Unreachable);

        // GET: hit → entry.value; miss → undefined.
        {
            let miss = body.add_block();
            let present = body.add_block();
            body.set_terminator(
                get_block,
                Terminator::CondBr {
                    cond: hit,
                    if_true: BlockTarget {
                        block: present,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: miss,
                        args: vec![],
                    },
                },
            );
            let pair = body.add_op(
                present,
                Operator::ArrayGet {
                    sig: self.repr.map_entries,
                },
                &[entries, found],
                &[self.repr.map_entry_ty()],
            );
            let v = body.add_op(
                present,
                Operator::StructGet {
                    sig: self.repr.map_entry,
                    idx: 1,
                },
                &[pair],
                &[self.repr.value],
            );
            body.set_terminator(present, Terminator::Return { values: vec![v] });
            let undef = self.null_ref(&mut body, miss);
            body.set_terminator(
                miss,
                Terminator::Return {
                    values: vec![undef],
                },
            );
        }
        // SET: hit → overwrite value; miss → append; return `this`.
        {
            let overwrite = body.add_block();
            let append = body.add_block();
            body.set_terminator(
                set_block,
                Terminator::CondBr {
                    cond: hit,
                    if_true: BlockTarget {
                        block: overwrite,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: append,
                        args: vec![],
                    },
                },
            );
            let pair = body.add_op(
                overwrite,
                Operator::ArrayGet {
                    sig: self.repr.map_entries,
                },
                &[entries, found],
                &[self.repr.map_entry_ty()],
            );
            body.add_op(
                overwrite,
                Operator::StructSet {
                    sig: self.repr.map_entry,
                    idx: 1,
                },
                &[pair, value],
                &[],
            );
            body.set_terminator(overwrite, Terminator::Return { values: vec![this] });
            let (append, fresh) = self.entries_with_mutation(
                &mut body,
                append,
                entries,
                len,
                None,
                Some((key, value)),
            );
            let as_value = body.add_op(
                append,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[fresh],
                &[self.repr.value],
            );
            body.add_op(
                append,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            body.set_terminator(append, Terminator::Return { values: vec![this] });
        }
        // HAS: hit ? true : false.
        {
            let yes = self.bool_ref(&mut body, has_block, true);
            let no = self.bool_ref(&mut body, has_block, false);
            let v = body.add_op(
                has_block,
                Operator::TypedSelect {
                    ty: self.repr.value,
                },
                &[yes, no, hit],
                &[self.repr.value],
            );
            body.set_terminator(has_block, Terminator::Return { values: vec![v] });
        }
        // DELETE: hit → splice out, true; miss → false.
        {
            let present = body.add_block();
            let miss = body.add_block();
            body.set_terminator(
                delete_block,
                Terminator::CondBr {
                    cond: hit,
                    if_true: BlockTarget {
                        block: present,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: miss,
                        args: vec![],
                    },
                },
            );
            let (present, fresh) =
                self.entries_with_mutation(&mut body, present, entries, len, Some(found), None);
            let as_value = body.add_op(
                present,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[fresh],
                &[self.repr.value],
            );
            body.add_op(
                present,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            let yes = self.bool_ref(&mut body, present, true);
            body.set_terminator(present, Terminator::Return { values: vec![yes] });
            let no = self.bool_ref(&mut body, miss, false);
            body.set_terminator(miss, Terminator::Return { values: vec![no] });
        }
        // SIZE: boxed length.
        {
            let f = body.add_op(size_block, Operator::F64ConvertI32U, &[len], &[Type::F64]);
            let v = self.number_ref(&mut body, size_block, f);
            body.set_terminator(size_block, Terminator::Return { values: vec![v] });
        }
        // CLEAR: empty entries; undefined.
        {
            let empty = body.add_op(
                clear_block,
                Operator::ArrayNewFixed {
                    sig: self.repr.map_entries,
                    num: 0,
                },
                &[],
                &[self.repr.map_entries_ty()],
            );
            let as_value = body.add_op(
                clear_block,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[empty],
                &[self.repr.value],
            );
            body.add_op(
                clear_block,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            let undef = self.null_ref(&mut body, clear_block);
            body.set_terminator(
                clear_block,
                Terminator::Return {
                    values: vec![undef],
                },
            );
        }

        // The op dispatch ladder.
        let k_is_get = op_const(&mut body, block, MAP_OP_GET);
        let is_get = body.add_op(block, Operator::I32Eq, &[op, k_is_get], &[Type::I32]);
        let not_get = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_get,
                if_true: BlockTarget {
                    block: get_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_get,
                    args: vec![],
                },
            },
        );
        let k_is_set = op_const(&mut body, not_get, MAP_OP_SET);
        let is_set = body.add_op(not_get, Operator::I32Eq, &[op, k_is_set], &[Type::I32]);
        let not_set = body.add_block();
        body.set_terminator(
            not_get,
            Terminator::CondBr {
                cond: is_set,
                if_true: BlockTarget {
                    block: set_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_set,
                    args: vec![],
                },
            },
        );
        let k_is_has = op_const(&mut body, not_set, MAP_OP_HAS);
        let is_has = body.add_op(not_set, Operator::I32Eq, &[op, k_is_has], &[Type::I32]);
        let not_has = body.add_block();
        body.set_terminator(
            not_set,
            Terminator::CondBr {
                cond: is_has,
                if_true: BlockTarget {
                    block: has_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_has,
                    args: vec![],
                },
            },
        );
        let k_is_delete = op_const(&mut body, not_has, MAP_OP_DELETE);
        let is_delete = body.add_op(not_has, Operator::I32Eq, &[op, k_is_delete], &[Type::I32]);
        let not_delete = body.add_block();
        body.set_terminator(
            not_has,
            Terminator::CondBr {
                cond: is_delete,
                if_true: BlockTarget {
                    block: delete_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_delete,
                    args: vec![],
                },
            },
        );
        let k_is_size = op_const(&mut body, not_delete, MAP_OP_SIZE);
        let is_size = body.add_op(not_delete, Operator::I32Eq, &[op, k_is_size], &[Type::I32]);
        let not_size = body.add_block();
        body.set_terminator(
            not_delete,
            Terminator::CondBr {
                cond: is_size,
                if_true: BlockTarget {
                    block: size_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_size,
                    args: vec![],
                },
            },
        );
        let k_is_clear = op_const(&mut body, not_size, MAP_OP_CLEAR);
        let is_clear = body.add_op(not_size, Operator::I32Eq, &[op, k_is_clear], &[Type::I32]);
        body.set_terminator(
            not_size,
            Terminator::CondBr {
                cond: is_clear,
                if_true: BlockTarget {
                    block: clear_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: trap_block,
                    args: vec![],
                },
            },
        );

        let name = if weak {
            "js_weakmap_method"
        } else {
            "js_map_method"
        };
        let func = self.module.funcs.push(FuncDecl::Body(
            adapter,
            format!("{name}_{}", self.module.funcs.len()),
            body,
        ));
        self.declare_function_reference(func);
        Ok(func)
    }

    /// Build the Set/WeakSet method dispatcher (key-only table core).
    fn build_set_method_dispatcher(
        &mut self,
        weak: bool,
        find: Func,
    ) -> Result<Func, ConvertError> {
        let adapter = self.repr.adapter;
        let mut body = FunctionBody::new(self.module, adapter);
        let entry = body.entry;
        let ctx = body.blocks[entry].params[0].1;
        let this = body.blocks[entry].params[1].1;
        let args = body.blocks[entry].params[2].1;
        let op = self.method_selector(&mut body, entry, ctx)?;
        let (block, key) = self.arg_or_undef(&mut body, entry, args, 0);

        let guarded = if weak {
            let is_obj = body.add_op(
                block,
                Operator::RefTest {
                    ty: self.repr.object_non_null_ty(),
                },
                &[key],
                &[Type::I32],
            );
            let is_fn = body.add_op(
                block,
                Operator::RefTest {
                    ty: self.repr.function_non_null_ty(),
                },
                &[key],
                &[Type::I32],
            );
            let ok = body.add_op(block, Operator::I32Or, &[is_obj, is_fn], &[Type::I32]);
            let has_op = body.add_op(
                block,
                Operator::I32Const {
                    value: WEAKSET_OP_HAS as u32,
                },
                &[],
                &[Type::I32],
            );
            let is_has = body.add_op(block, Operator::I32Eq, &[op, has_op], &[Type::I32]);
            let primitive_ok = body.add_block();
            let primitive_fail = body.add_block();
            let proceed = body.add_block();
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: ok,
                    if_true: BlockTarget {
                        block: proceed,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: primitive_ok,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(
                primitive_ok,
                Terminator::CondBr {
                    cond: is_has,
                    if_true: BlockTarget {
                        block: proceed,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: primitive_fail,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(primitive_fail, Terminator::Unreachable);
            proceed
        } else {
            block
        };

        let (block, entries, this_obj) = self.collection_entries(&mut body, guarded, this);
        let len = body.add_op(block, Operator::ArrayLen, &[entries], &[Type::I32]);
        let found = body.add_op(
            block,
            Operator::Call {
                function_index: find,
            },
            &[entries, key],
            &[Type::I32],
        );
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let hit = body.add_op(block, Operator::I32GeS, &[found, zero], &[Type::I32]);
        let undef_for_entry = self.null_ref(&mut body, block);

        fn op_const(body: &mut FunctionBody, b: Block, v: i32) -> Value {
            body.add_op(b, Operator::I32Const { value: v as u32 }, &[], &[Type::I32])
        }
        let add_block = body.add_block();
        let has_block = body.add_block();
        let delete_block = body.add_block();
        let size_block = body.add_block();
        let clear_block = body.add_block();
        let trap_block = body.add_block();
        body.set_terminator(trap_block, Terminator::Unreachable);

        // ADD: hit → no-op; miss → append; return `this`.
        {
            let append = body.add_block();
            let hit_ret = body.add_block();
            body.set_terminator(hit_ret, Terminator::Return { values: vec![this] });
            body.set_terminator(
                add_block,
                Terminator::CondBr {
                    cond: hit,
                    if_true: BlockTarget {
                        block: hit_ret,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: append,
                        args: vec![],
                    },
                },
            );
            let (append, fresh) = self.entries_with_mutation(
                &mut body,
                append,
                entries,
                len,
                None,
                Some((key, undef_for_entry)),
            );
            let as_value = body.add_op(
                append,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[fresh],
                &[self.repr.value],
            );
            body.add_op(
                append,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            body.set_terminator(append, Terminator::Return { values: vec![this] });
        }
        // HAS.
        {
            let yes = self.bool_ref(&mut body, has_block, true);
            let no = self.bool_ref(&mut body, has_block, false);
            let v = body.add_op(
                has_block,
                Operator::TypedSelect {
                    ty: self.repr.value,
                },
                &[yes, no, hit],
                &[self.repr.value],
            );
            body.set_terminator(has_block, Terminator::Return { values: vec![v] });
        }
        // DELETE.
        {
            let present = body.add_block();
            let miss = body.add_block();
            body.set_terminator(
                delete_block,
                Terminator::CondBr {
                    cond: hit,
                    if_true: BlockTarget {
                        block: present,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: miss,
                        args: vec![],
                    },
                },
            );
            let (present, fresh) =
                self.entries_with_mutation(&mut body, present, entries, len, Some(found), None);
            let as_value = body.add_op(
                present,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[fresh],
                &[self.repr.value],
            );
            body.add_op(
                present,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            let yes = self.bool_ref(&mut body, present, true);
            body.set_terminator(present, Terminator::Return { values: vec![yes] });
            let no = self.bool_ref(&mut body, miss, false);
            body.set_terminator(miss, Terminator::Return { values: vec![no] });
        }
        // SIZE.
        {
            let f = body.add_op(size_block, Operator::F64ConvertI32U, &[len], &[Type::F64]);
            let v = self.number_ref(&mut body, size_block, f);
            body.set_terminator(size_block, Terminator::Return { values: vec![v] });
        }
        // CLEAR.
        {
            let empty = body.add_op(
                clear_block,
                Operator::ArrayNewFixed {
                    sig: self.repr.map_entries,
                    num: 0,
                },
                &[],
                &[self.repr.map_entries_ty()],
            );
            let as_value = body.add_op(
                clear_block,
                Operator::RefCast {
                    ty: self.repr.value,
                },
                &[empty],
                &[self.repr.value],
            );
            body.add_op(
                clear_block,
                Operator::StructSet {
                    sig: self.repr.object,
                    idx: 3,
                },
                &[this_obj, as_value],
                &[],
            );
            let undef = self.null_ref(&mut body, clear_block);
            body.set_terminator(
                clear_block,
                Terminator::Return {
                    values: vec![undef],
                },
            );
        }

        let k_is_add = op_const(&mut body, block, SET_OP_ADD);
        let is_add = body.add_op(block, Operator::I32Eq, &[op, k_is_add], &[Type::I32]);
        let not_add = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_add,
                if_true: BlockTarget {
                    block: add_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_add,
                    args: vec![],
                },
            },
        );
        let k_is_has = op_const(&mut body, not_add, SET_OP_HAS);
        let is_has = body.add_op(not_add, Operator::I32Eq, &[op, k_is_has], &[Type::I32]);
        let not_has = body.add_block();
        body.set_terminator(
            not_add,
            Terminator::CondBr {
                cond: is_has,
                if_true: BlockTarget {
                    block: has_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_has,
                    args: vec![],
                },
            },
        );
        let k_is_delete = op_const(&mut body, not_has, SET_OP_DELETE);
        let is_delete = body.add_op(not_has, Operator::I32Eq, &[op, k_is_delete], &[Type::I32]);
        let not_delete = body.add_block();
        body.set_terminator(
            not_has,
            Terminator::CondBr {
                cond: is_delete,
                if_true: BlockTarget {
                    block: delete_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_delete,
                    args: vec![],
                },
            },
        );
        let k_is_size = op_const(&mut body, not_delete, SET_OP_SIZE);
        let is_size = body.add_op(not_delete, Operator::I32Eq, &[op, k_is_size], &[Type::I32]);
        let not_size = body.add_block();
        body.set_terminator(
            not_delete,
            Terminator::CondBr {
                cond: is_size,
                if_true: BlockTarget {
                    block: size_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_size,
                    args: vec![],
                },
            },
        );
        let k_is_clear = op_const(&mut body, not_size, SET_OP_CLEAR);
        let is_clear = body.add_op(not_size, Operator::I32Eq, &[op, k_is_clear], &[Type::I32]);
        body.set_terminator(
            not_size,
            Terminator::CondBr {
                cond: is_clear,
                if_true: BlockTarget {
                    block: clear_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: trap_block,
                    args: vec![],
                },
            },
        );

        let name = if weak {
            "js_weakset_method"
        } else {
            "js_set_method"
        };
        let func = self.module.funcs.push(FuncDecl::Body(
            adapter,
            format!("{name}_{}", self.module.funcs.len()),
            body,
        ));
        self.declare_function_reference(func);
        Ok(func)
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// Install the collection constructors (`Map`, `Set`, `WeakMap`,
    /// `WeakSet`) into a fresh context. Each constructor is a tagged
    /// function object whose `prototype` property carries the methods; each
    /// method is a function object wrapping the shared dispatcher with its
    /// numeric selector in its captured context under `\u{1}op`.
    fn build_collection_primordials(
        &mut self,
        body: &mut FunctionBody,
        mut block: Block,
        context: &LowerValue,
    ) -> Result<Block, ConvertError> {
        let helpers = self.ensure_collection_helpers()?;
        let empty_ctx = self.new_object(body, block)?;
        let empty_ctx_v = empty_ctx.wasm()?.0;

        let specs: [(&str, i32, Func, Func, &[(&str, i32)]); 4] = [
            (
                "Map",
                TAG_MAP_CTOR,
                helpers.map_new,
                helpers.map_method,
                &[
                    ("get", MAP_OP_GET),
                    ("set", MAP_OP_SET),
                    ("has", MAP_OP_HAS),
                    ("delete", MAP_OP_DELETE),
                    ("clear", MAP_OP_CLEAR),
                ],
            ),
            (
                "Set",
                TAG_SET_CTOR,
                helpers.set_new,
                helpers.set_method,
                &[
                    ("add", SET_OP_ADD),
                    ("has", SET_OP_HAS),
                    ("delete", SET_OP_DELETE),
                    ("clear", SET_OP_CLEAR),
                ],
            ),
            (
                "WeakMap",
                TAG_WEAKMAP_CTOR,
                helpers.weakmap_new,
                helpers.weakmap_method,
                &[
                    ("get", WEAKMAP_OP_GET),
                    ("set", WEAKMAP_OP_SET),
                    ("has", WEAKMAP_OP_HAS),
                    ("delete", WEAKMAP_OP_DELETE),
                ],
            ),
            (
                "WeakSet",
                TAG_WEAKSET_CTOR,
                helpers.weakset_new,
                helpers.weakset_method,
                &[
                    ("add", WEAKSET_OP_ADD),
                    ("has", WEAKSET_OP_HAS),
                    ("delete", WEAKSET_OP_DELETE),
                ],
            ),
        ];

        for (name, ctor_tag, ctor_func, method_func, methods) in specs {
            let proto = self.new_object(body, block)?;
            for (method_name, selector) in methods {
                // Method function: context carries the numeric selector.
                let mctx = self.new_object(body, block)?;
                let mctx_v = mctx.wasm()?.0;
                let f = body.add_op(
                    block,
                    Operator::F64Const {
                        value: (*selector as f64).to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                let num = self.number_ref(body, block, f);
                let num_lv = LowerValue::Wasm {
                    value: num,
                    kind: ValueKind::Reference,
                };
                block =
                    self.set_static_property_value_raw(body, block, &mctx, "\u{1}op", &num_lv)?;
                let method = self.primordial_function_value(
                    body,
                    block,
                    mctx_v,
                    method_func,
                    TAG_MAP_METHOD,
                )?;
                block =
                    self.set_static_property_value_raw(body, block, &proto, method_name, &method)?;
            }
            // `size` is an accessor: a getter descriptor wrapping the
            // dispatcher with the size selector.
            {
                let mctx = self.new_object(body, block)?;
                let mctx_v = mctx.wasm()?.0;
                let size_op = match name {
                    "Map" => MAP_OP_SIZE,
                    "Set" => SET_OP_SIZE,
                    _ => 0, // WeakMap/WeakSet have no size; not installed
                };
                let f = body.add_op(
                    block,
                    Operator::F64Const {
                        value: (size_op as f64).to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                let num = self.number_ref(body, block, f);
                let num_lv = LowerValue::Wasm {
                    value: num,
                    kind: ValueKind::Reference,
                };
                block =
                    self.set_static_property_value_raw(body, block, &mctx, "\u{1}op", &num_lv)?;
                let is_weak = name == "WeakMap" || name == "WeakSet";
                let getter_func = if is_weak { method_func } else { method_func };
                let getter = self.primordial_function_value(
                    body,
                    block,
                    mctx_v,
                    getter_func,
                    TAG_MAP_METHOD,
                )?;
                let getter_v = self.box_value(body, block, &getter)?;
                let descriptor = self.new_descriptor(body, block);
                body.add_op(
                    block,
                    Operator::StructSet {
                        sig: self.repr.descriptor,
                        idx: 0,
                    },
                    &[descriptor, getter_v],
                    &[],
                );
                let descriptor_value = self.anyref(body, block, descriptor);
                let descriptor_lv = LowerValue::Wasm {
                    value: descriptor_value,
                    kind: ValueKind::Reference,
                };
                if !is_weak {
                    block = self.set_static_property_value_raw(
                        body,
                        block,
                        &proto,
                        "size",
                        &descriptor_lv,
                    )?;
                }
            }
            let ctor =
                self.primordial_function_value(body, block, empty_ctx_v, ctor_func, ctor_tag)?;
            block = self.set_static_property_value_raw(body, block, &ctor, "prototype", &proto)?;
            block =
                self.set_static_property_value_raw(body, block, &proto, "constructor", &ctor)?;
            block = self.set_static_property_value_raw(body, block, context, name, &ctor)?;
        }
        Ok(block)
    }

    /// Lower a call to a `~weakmap` intrinsic import: `weakSet(wm, key,
    /// value)`, `weakGet(wm, key)`, `weakHas(wm, key)`, or
    /// `weakDelete(wm, key)`. `wm` is a numeric literal table id; the native
    /// tier encodes it into an invisible property name on the object key.
    fn try_weakmap_intrinsic_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        context: Value,
        this: LowerValue,
        values: &ContinuationValues,
        callee_ident: &swc_ecma_ast::Id,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Option<Vec<(Block, LowerValue)>>, ConvertError> {
        let is_weakmap_intrinsic = self
            .import_tables
            .get(&self.current_module)
            .and_then(|table| table.get(callee_ident))
            .is_some_and(|target| {
                matches!(target, ImportTarget::Intrinsic { module, .. } if module == "~weakmap")
            });
        if !is_weakmap_intrinsic {
            return Ok(None);
        }
        let name = callee_ident.0.as_str();
        let (wm, key, value) = match name {
            "weakSet" => {
                if args.len() != 3 {
                    return Err(ConvertError::invalid("weakSet takes (table, key, value)"));
                }
                (
                    values
                        .get(&args[0].value)
                        .cloned()
                        .ok_or_else(|| ConvertError::invalid("weakSet table"))?,
                    values
                        .get(&args[1].value)
                        .cloned()
                        .ok_or_else(|| ConvertError::invalid("weakSet key"))?,
                    Some(
                        values
                            .get(&args[2].value)
                            .cloned()
                            .ok_or_else(|| ConvertError::invalid("weakSet value"))?,
                    ),
                )
            }
            "weakGet" | "weakHas" | "weakDelete" => {
                if args.len() != 2 {
                    return Err(ConvertError::invalid(format!("{name} takes (table, key)")));
                }
                (
                    values
                        .get(&args[0].value)
                        .cloned()
                        .ok_or_else(|| ConvertError::invalid(format!("{name} table")))?,
                    values
                        .get(&args[1].value)
                        .cloned()
                        .ok_or_else(|| ConvertError::invalid(format!("{name} key")))?,
                    None,
                )
            }
            other => {
                return Err(ConvertError::invalid(format!(
                    "unknown ~weakmap intrinsic {other:?} (expected weakSet/weakGet/weakHas/weakDelete)"
                )));
            }
        };
        let table_name = match &wm {
            LowerValue::NumberLiteral { key, .. } => key.clone(),
            _ => {
                return Err(ConvertError::invalid(
                    "~weakmap table handle must be a numeric literal",
                ));
            }
        };
        let result = self.weakmap_intrinsic(
            body,
            block,
            context,
            this,
            name,
            &table_name,
            &key,
            value.as_ref(),
        )?;
        Ok(Some(result))
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// Lower one `~weakmap` intrinsic call to native weak-table ops (#32
    /// native tier).
    ///
    /// The table id is a numeric literal and is encoded in an invisible
    /// `\u{1}weak\u{1}<id>` own property on each object key. The key-owned
    /// slot preserves object identity and, importantly, does not create a
    /// table-to-key strong edge: when the key is unreachable, its slot and
    /// stored value are unreachable with it. This is the weak ownership edge
    /// shared by the WasmGC and nano lowerings without requiring a WasmGC
    /// weak-reference instruction. Keys must be objects (JS WeakMap key
    /// rules); primitives answer absent for GET/HAS and trap for SET/DELETE.
    fn weakmap_intrinsic(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        _context: Value,
        _this: LowerValue,
        name: &str,
        table_name: &str,
        key: &LowerValue,
        value: Option<&LowerValue>,
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        // Native weak storage is represented by an invisible own property on
        // the object key itself. The numeric handle is part of the property
        // name, so handle 0 and handle 1 remain isolated while object
        // identity is preserved by the ordinary property machinery.
        let slot_name = format!("\u{1}weak\u{1}{table_name}");
        let slot_key = self.new_string(body, block, slot_name.as_bytes())?;

        let (key_value, _) = key.clone().wasm()?;
        let is_obj = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.object_non_null_ty(),
            },
            &[key_value],
            &[Type::I32],
        );
        let is_fn = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.function_non_null_ty(),
            },
            &[key_value],
            &[Type::I32],
        );
        let key_ok = body.add_op(block, Operator::I32Or, &[is_obj, is_fn], &[Type::I32]);
        let object_block = body.add_block();
        let primitive_block = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: key_ok,
                if_true: BlockTarget {
                    block: object_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: primitive_block,
                    args: vec![],
                },
            },
        );

        match name {
            "weakGet" | "weakHas" => {
                let join = body.add_block();
                let result = body.add_blockparam(join, self.repr.value);
                let primitive = if name == "weakHas" {
                    self.bool_ref(body, primitive_block, false)
                } else {
                    self.null_ref(body, primitive_block)
                };
                body.set_terminator(
                    primitive_block,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![primitive],
                        },
                    },
                );
                let paths = self.get_string_member(body, object_block, key, slot_key)?;
                for (path, value) in paths {
                    let value = if name == "weakHas" {
                        let (raw, _) = value.wasm()?;
                        let absent = body.add_op(path, Operator::RefIsNull, &[raw], &[Type::I32]);
                        let yes = self.bool_ref(body, path, true);
                        let no = self.bool_ref(body, path, false);
                        body.add_op(
                            path,
                            Operator::TypedSelect {
                                ty: self.repr.value,
                            },
                            &[no, yes, absent],
                            &[self.repr.value],
                        )
                    } else {
                        value.wasm()?.0
                    };
                    body.set_terminator(
                        path,
                        Terminator::Br {
                            target: BlockTarget {
                                block: join,
                                args: vec![value],
                            },
                        },
                    );
                }
                Ok(vec![(
                    join,
                    LowerValue::Wasm {
                        value: result,
                        kind: ValueKind::Reference,
                    },
                )])
            }
            "weakSet" => {
                let value = value.ok_or_else(|| ConvertError::invalid("weakSet value"))?;
                body.set_terminator(primitive_block, Terminator::Unreachable);
                let paths = self.set_string_member(body, object_block, key, slot_key, value)?;
                let join = self.join_write_continuations(body, paths);
                let undef = self.null_ref(body, join);
                Ok(vec![(
                    join,
                    LowerValue::Wasm {
                        value: undef,
                        kind: ValueKind::Reference,
                    },
                )])
            }
            "weakDelete" => {
                body.set_terminator(primitive_block, Terminator::Unreachable);
                let paths = self.get_string_member(body, object_block, key, slot_key)?;
                let join = body.add_block();
                let result = body.add_blockparam(join, self.repr.value);
                for (path, old) in paths {
                    let (raw, _) = old.wasm()?;
                    let absent = body.add_op(path, Operator::RefIsNull, &[raw], &[Type::I32]);
                    let missing = body.add_block();
                    let present = body.add_block();
                    body.set_terminator(
                        path,
                        Terminator::CondBr {
                            cond: absent,
                            if_true: BlockTarget {
                                block: missing,
                                args: vec![],
                            },
                            if_false: BlockTarget {
                                block: present,
                                args: vec![],
                            },
                        },
                    );
                    let no = self.bool_ref(body, missing, false);
                    body.set_terminator(
                        missing,
                        Terminator::Br {
                            target: BlockTarget {
                                block: join,
                                args: vec![no],
                            },
                        },
                    );
                    let undef = LowerValue::Wasm {
                        value: self.null_ref(body, present),
                        kind: ValueKind::Reference,
                    };
                    let writes = self.set_string_member(body, present, key, slot_key, &undef)?;
                    let write_join = self.join_write_continuations(body, writes);
                    let yes = self.bool_ref(body, write_join, true);
                    body.set_terminator(
                        write_join,
                        Terminator::Br {
                            target: BlockTarget {
                                block: join,
                                args: vec![yes],
                            },
                        },
                    );
                }
                Ok(vec![(
                    join,
                    LowerValue::Wasm {
                        value: result,
                        kind: ValueKind::Reference,
                    },
                )])
            }
            other => Err(ConvertError::invalid(format!(
                "unknown ~weakmap intrinsic {other:?}"
            ))),
        }
    }
}
