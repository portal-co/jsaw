// Native (hand-written Wasm) implementations of JavaScript primordials.
//
// This file is `include!`d into `impl Converter` in `conv.rs`; it is not a
// standalone module. Every exported function currently receives a brand new,
// empty lexical `context` object (see `export_numeric_function`'s comment).
// `new_context_with_primordials` is the same construction, pre-populated
// with `Math`/`Array` bindings. Because a later `let Math = ...;` inside the
// function lowers to an ordinary `StoreId` write onto that same context
// object, user shadowing of a primordial name works for free through the
// existing read/write path — no separate name-recognition fast path needed.
//
// Scope for this milestone: `Math` (constants and the methods directly
// expressible with a Wasm float instruction, plus `fround`/`imul` which
// jsaw-core's `Primordial` enum already singled out) and `Array.isArray`.
// `Reflect`, `Object`, and the rest of `Array` are follow-up work — `Object`
// in particular needs a property-enumeration primitive (for `keys`/`values`/
// `entries`) that nothing in this crate implements yet.

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {

/// Build a fresh context object pre-populated with the primordial global
/// bindings. Returns the block to continue lowering from (installing each
/// binding is itself a property write, which may branch) and the resulting
/// context value.
fn new_context_with_primordials(
    &mut self,
    body: &mut FunctionBody,
    mut block: Block,
) -> Result<(Block, Value), ConvertError> {
    let context = self.new_object(body, block)?;

    let math = self.build_math_namespace(body, &mut block)?;
    block = self.set_static_property_value_raw(body, block, &context, "Math", &math)?;

    let array = self.build_array_namespace(body, &mut block)?;
    block = self.set_static_property_value_raw(body, block, &context, "Array", &array)?;

    let reflect = self.build_reflect_namespace(body, &mut block)?;
    block = self.set_static_property_value_raw(body, block, &context, "Reflect", &reflect)?;

    let object_ns = self.build_object_namespace(body, &mut block)?;
    block = self.set_static_property_value_raw(body, block, &context, "Object", &object_ns)?;

    // Typed-array constructors are globals rather than namespace members.
    // Installing them through the ordinary context preserves the same
    // shadowing rules as Math/Array: a later local declaration is just an
    // ordinary property write on this object.
    let typed_constructor = self.typed_array_constructor()?;
    for kind in TypedArrayKind::ALL {
        // Each function value captures a tiny private context containing its
        // kind tag. The adapter body is shared across all nine globals,
        // avoiding nine copies of the source-copy and WasmGC dispatch code.
        let constructor_context = self.new_object(body, block)?;
        let (constructor_context_value, _) = constructor_context.wasm()?;
        let context_object = body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[constructor_context_value],
            &[self.repr.object_ty()],
        );
        let tag = body.add_op(
            block,
            Operator::I32Const {
                value: kind.code() as u32,
            },
            &[],
            &[Type::I32],
        );
        body.add_op(
            block,
            Operator::StructSet {
                sig: self.repr.object,
                idx: 4,
            },
            &[context_object, tag],
            &[],
        );
        let constructor = self.native_function_value(
            body,
            block,
            constructor_context_value,
            typed_constructor,
        )?;
        let bytes = body.add_op(
            block,
            Operator::F64Const {
                value: f64::from(kind.bytes_per_element()).to_bits(),
            },
            &[],
            &[Type::F64],
        );
        block = self.set_static_property_value_raw(
            body,
            block,
            &constructor,
            "BYTES_PER_ELEMENT",
            &LowerValue::Wasm {
                value: bytes,
                kind: ValueKind::Number,
            },
        )?;
        block = self.set_static_property_value_raw(body, block, &context, kind.name(), &constructor)?;
    }

    let (context_value, _) = context.wasm()?;
    Ok((block, context_value))
}

/// Wrap [`Self::new_context_with_primordials`] in its own zero-argument
/// Wasm function, generated once and cached. A call site just emits a
/// single `call` instead of an inlined copy of the entire
/// namespace-construction sequence — with 20+ primordial methods across
/// four namespaces, inlining that at every export made this crate's own
/// validation/emission (and Wasmtime/Node's compilation of the result)
/// scale linearly with export count, which was impractically slow.
fn ensure_context_builder(&mut self) -> Result<Func, ConvertError> {
    if let Some(func) = self.context_builder {
        return Ok(func);
    }
    let sig = self.module.signatures.push(SignatureData::Func {
        params: vec![],
        returns: vec![self.repr.object_ty()],
        shared: false,
    });
    let mut body = FunctionBody::new(self.module, sig);
    let entry = body.entry;
    let (block, context_value) = self.new_context_with_primordials(&mut body, entry)?;
    body.set_terminator(
        block,
        Terminator::Return {
            values: vec![context_value],
        },
    );
    let func = self.module.funcs.push(FuncDecl::Body(
        sig,
        "js_build_primordial_context".to_string(),
        body,
    ));
    self.context_builder = Some(func);
    Ok(func)
}

/// Declare and fully build a Wasm function with the adapter ABI
/// `(context, this, args) -> value`. `build` receives the entry block and
/// the three ABI parameters and is responsible for terminating every block
/// it creates, mirroring `make_adapter`.
fn build_native_adapter(
    &mut self,
    label: &str,
    build: impl FnOnce(&mut Self, &mut FunctionBody, Block, Value, Value, Value) -> Result<(), ConvertError>,
) -> Result<Func, ConvertError> {
    if let Some(func) = self.native_function_cache.get(label) {
        return Ok(*func);
    }
    let mut body = FunctionBody::new(self.module, self.repr.adapter);
    let entry = body.entry;
    let context = body.blocks[entry].params[0].1;
    let this = body.blocks[entry].params[1].1;
    let args = body.blocks[entry].params[2].1;
    build(self, &mut body, entry, context, this, args)?;
    let func = self.module.funcs.push(FuncDecl::Body(
        self.repr.adapter,
        format!("js_native_{label}_{}", self.module.funcs.len()),
        body,
    ));
    self.declare_function_reference(func);
    self.native_function_cache.insert(label.to_owned(), func);
    Ok(func)
}

/// Wrap a declared native adapter as an ordinary (non-arrow, so the call
/// site's own receiver is used as `this`) function value with no own
/// properties. Primordial method bodies never read their `context` field, so
/// reusing the caller's `context` avoids an extra allocation.
fn native_function_value(
    &self,
    body: &mut FunctionBody,
    block: Block,
    context: Value,
    func: Func,
) -> Result<LowerValue, ConvertError> {
    self.primordial_function_value(body, block, context, func, 0)
}

/// Install a declared native adapter as an ordinary function value with a
/// nonzero [`PRIMORDIAL_TAG_BASE`]-offset tag. The tag rides in the final
/// field of the function struct and lets a call site confirm "this callee is
/// still exactly the primordial I expect" with one `i32.eq` — function
/// references themselves are not `eqref`, so `ref.eq` cannot make that
/// comparison. A tag of 0 marks user (non-primordial) functions.
fn primordial_function_value(
    &self,
    body: &mut FunctionBody,
    block: Block,
    context: Value,
    func: Func,
    tag: i32,
) -> Result<LowerValue, ConvertError> {
    let trie = self.new_trie(body, block)?;
    let trie = self.anyref(body, block, trie);
    let elements = body.add_op(
        block,
        Operator::RefNull {
            ty: self.repr.arguments_ty(),
        },
        &[],
        &[self.repr.arguments_ty()],
    );
    let extra = body.add_op(
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
    let captured_this = body.add_op(
        block,
        Operator::RefNull {
            ty: self.repr.value,
        },
        &[],
        &[self.repr.value],
    );
    let arrow = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    let tag = body.add_op(block, Operator::I32Const { value: tag as u32 }, &[], &[Type::I32]);
    let value = body.add_op(
        block,
        Operator::StructNew {
            sig: self.repr.function,
        },
        &[
            trie,
            elements,
            extra,
            code,
            context,
            captured_this,
            arrow,
            tag,
        ],
        &[self.repr.function_ty()],
    );
    Ok(LowerValue::Wasm {
        value,
        kind: ValueKind::Reference,
    })
}

/// Read the `index`-th call argument, coerced to `f64`. A missing argument
/// or an explicit `undefined` becomes `NaN`, matching ordinary JS numeric
/// coercion of `undefined`. Values already known to be numbers (the only
/// case this milestone's callers pass) reuse `as_f64` unchanged.
/// Read the `index`-th call argument as its raw boxed value (nullable
/// `anyref`). A missing argument becomes `undefined` (null), matching an
/// explicit `undefined` argument — this milestone does not distinguish the
/// two, same as the rest of this crate's property-lookup machinery.
fn read_arg_raw(
    &self,
    body: &mut FunctionBody,
    block: Block,
    args: Value,
    index: u32,
) -> (Block, Value) {
    let len = body.add_op(block, Operator::ArrayLen, &[args], &[Type::I32]);
    let idx = body.add_op(
        block,
        Operator::I32Const { value: index },
        &[],
        &[Type::I32],
    );
    let present = body.add_op(block, Operator::I32LtU, &[idx, len], &[Type::I32]);
    let get = body.add_block();
    let missing = body.add_block();
    let join = body.add_block();
    let raw = body.add_blockparam(join, self.repr.value);
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
    (join, raw)
}

/// Read the `index`-th call argument, coerced to `f64`. A missing argument
/// or an explicit `undefined` becomes `NaN`, matching ordinary JS numeric
/// coercion of `undefined`. Values already known to be numbers (the only
/// case this milestone's callers pass) reuse `as_f64` unchanged.
fn read_arg_number(
    &self,
    body: &mut FunctionBody,
    block: Block,
    args: Value,
    index: u32,
) -> Result<(Block, Value), ConvertError> {
    let (join, raw) = self.read_arg_raw(body, block, args, index);

    let is_null = body.add_op(join, Operator::RefIsNull, &[raw], &[Type::I32]);
    let has_value = body.add_block();
    let is_nan = body.add_block();
    let result_join = body.add_block();
    let result = body.add_blockparam(result_join, Type::F64);
    body.set_terminator(
        join,
        Terminator::CondBr {
            cond: is_null,
            if_true: BlockTarget {
                block: is_nan,
                args: vec![],
            },
            if_false: BlockTarget {
                block: has_value,
                args: vec![],
            },
        },
    );
    let nan = body.add_op(
        is_nan,
        Operator::F64Const {
            value: f64::NAN.to_bits(),
        },
        &[],
        &[Type::F64],
    );
    body.set_terminator(
        is_nan,
        Terminator::Br {
            target: BlockTarget {
                block: result_join,
                args: vec![nan],
            },
        },
    );
    let number = self.as_f64(
        body,
        has_value,
        &LowerValue::Wasm {
            value: raw,
            kind: ValueKind::Reference,
        },
    )?;
    body.set_terminator(
        has_value,
        Terminator::Br {
            target: BlockTarget {
                block: result_join,
                args: vec![number],
            },
        },
    );
    Ok((result_join, result))
}

/// Read the `index`-th call argument as a runtime string property key
/// (`Reflect`/`Object` methods take an already-computed key, unlike ordinary
/// `obj.prop` member access). Traps at runtime if the argument isn't
/// actually a string — full `ToPropertyKey` coercion (numbers, symbols) is
/// out of scope for this milestone.
fn read_arg_string_key(
    &self,
    body: &mut FunctionBody,
    block: Block,
    args: Value,
    index: u32,
) -> Result<(Block, Value), ConvertError> {
    let (block, raw) = self.read_arg_raw(body, block, args, index);
    let key = self.dynamic_string_key(
        body,
        block,
        &LowerValue::Wasm {
            value: raw,
            kind: ValueKind::Reference,
        },
    )?;
    Ok((block, key))
}

/// Box an `f64` result and return it, terminating `block`.
fn return_number(&self, body: &mut FunctionBody, block: Block, value: Value) {
    let boxed = self.box_value(
        body,
        block,
        &LowerValue::Wasm {
            value,
            kind: ValueKind::Number,
        },
    );
    // `box_value` on a `Number` kind never fails (no property lookups
    // involved), so the only error path is unreachable here.
    let boxed = boxed.expect("boxing a numeric value cannot fail");
    body.set_terminator(
        block,
        Terminator::Return {
            values: vec![boxed],
        },
    );
}

/// Install a unary `f64 -> f64` Math method backed directly by a Wasm float
/// instruction (`sqrt`, `abs`, `floor`, `ceil`, `trunc`, ...).
fn math_unary_method(&mut self, name: &str, op: Operator) -> Result<Func, ConvertError> {
    self.build_native_adapter(name, move |this, body, entry, _context, _this_val, args| {
        let (block, x) = this.read_arg_number(body, entry, args, 0)?;
        let result = body.add_op(block, op, &[x], &[Type::F64]);
        this.return_number(body, block, result);
        Ok(())
    })
}

/// Install a binary `(f64, f64) -> f64` Math method backed by a Wasm float
/// instruction (`min`, `max`).
fn math_binary_method(&mut self, name: &str, op: Operator) -> Result<Func, ConvertError> {
    self.build_native_adapter(name, move |this, body, entry, _context, _this_val, args| {
        let (block, x) = this.read_arg_number(body, entry, args, 0)?;
        let (block, y) = this.read_arg_number(body, block, args, 1)?;
        let result = body.add_op(block, op, &[x, y], &[Type::F64]);
        this.return_number(body, block, result);
        Ok(())
    })
}

fn math_sign_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter("sign", |this, body, entry, _context, _this_val, args| {
        let (block, x) = this.read_arg_number(body, entry, args, 0)?;
        let zero = body.add_op(block, Operator::F64Const { value: 0 }, &[], &[Type::F64]);
        let is_positive = body.add_op(block, Operator::F64Gt, &[x, zero], &[Type::I32]);
        let is_negative = body.add_op(block, Operator::F64Lt, &[x, zero], &[Type::I32]);
        let one = body.add_op(
            block,
            Operator::F64Const {
                value: 1.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let minus_one = body.add_op(
            block,
            Operator::F64Const {
                value: (-1.0f64).to_bits(),
            },
            &[],
            &[Type::F64],
        );
        // `x` itself is the correct result whenever it is `0`, `-0`, or
        // `NaN`; only the strictly-positive/negative cases need overriding.
        let negative_or_x = body.add_op(
            block,
            Operator::TypedSelect { ty: Type::F64 },
            &[minus_one, x, is_negative],
            &[Type::F64],
        );
        let result = body.add_op(
            block,
            Operator::TypedSelect { ty: Type::F64 },
            &[one, negative_or_x, is_positive],
            &[Type::F64],
        );
        this.return_number(body, block, result);
        Ok(())
    })
}

/// `Math.round`: round-half-up (towards `+Infinity`), matching JS semantics
/// rather than Wasm's own round-half-to-even `f64.nearest`.
fn math_round_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter("round", |this, body, entry, _context, _this_val, args| {
        let (block, x) = this.read_arg_number(body, entry, args, 0)?;
        let half = body.add_op(
            block,
            Operator::F64Const {
                value: 0.5f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let shifted = body.add_op(block, Operator::F64Add, &[x, half], &[Type::F64]);
        let result = body.add_op(block, Operator::F64Floor, &[shifted], &[Type::F64]);
        this.return_number(body, block, result);
        Ok(())
    })
}

/// `Math.fround`: round-trip through `f32` to truncate to single precision.
fn math_fround_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter("fround", |this, body, entry, _context, _this_val, args| {
        let (block, x) = this.read_arg_number(body, entry, args, 0)?;
        let narrowed = body.add_op(block, Operator::F32DemoteF64, &[x], &[Type::F32]);
        let result = body.add_op(block, Operator::F64PromoteF32, &[narrowed], &[Type::F64]);
        this.return_number(body, block, result);
        Ok(())
    })
}

/// `Math.imul`: 32-bit integer multiplication of both operands, truncated
/// per ECMAScript's `ToInt32`.
fn math_imul_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter("imul", |this, body, entry, _context, _this_val, args| {
        let (block, x) = this.read_arg_number(body, entry, args, 0)?;
        let (block, y) = this.read_arg_number(body, block, args, 1)?;
        let x = body.add_op(block, Operator::I32TruncSatF64S, &[x], &[Type::I32]);
        let y = body.add_op(block, Operator::I32TruncSatF64S, &[y], &[Type::I32]);
        let product = body.add_op(block, Operator::I32Mul, &[x, y], &[Type::I32]);
        let result = body.add_op(block, Operator::F64ConvertI32S, &[product], &[Type::F64]);
        this.return_number(body, block, result);
        Ok(())
    })
}

fn build_math_namespace(
    &mut self,
    body: &mut FunctionBody,
    block: &mut Block,
) -> Result<LowerValue, ConvertError> {
    let math = self.new_object(body, *block)?;

    const CONSTANTS: &[(&str, f64)] = &[
        ("PI", std::f64::consts::PI),
        ("E", std::f64::consts::E),
        ("LN2", std::f64::consts::LN_2),
        ("LN10", std::f64::consts::LN_10),
        ("LOG2E", std::f64::consts::LOG2_E),
        ("LOG10E", std::f64::consts::LOG10_E),
        ("SQRT2", std::f64::consts::SQRT_2),
        ("SQRT1_2", std::f64::consts::FRAC_1_SQRT_2),
    ];
    for (name, constant) in CONSTANTS {
        let raw = body.add_op(
            *block,
            Operator::F64Const {
                value: constant.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let value = LowerValue::Wasm {
            value: raw,
            kind: ValueKind::Number,
        };
        *block = self.set_static_property_value_raw(body, *block, &math, name, &value)?;
    }

    const UNARY: &[(&str, Operator)] = &[
        ("abs", Operator::F64Abs),
        ("floor", Operator::F64Floor),
        ("ceil", Operator::F64Ceil),
        ("trunc", Operator::F64Trunc),
        ("sqrt", Operator::F64Sqrt),
    ];
    for (name, op) in UNARY {
        let func = self.math_unary_method(name, *op)?;
        let (context, _) = math.wasm()?;
        let value = self.primordial_function_value(
            body,
            *block,
            context,
            func,
            static_primordial_tag("Math", name).unwrap_or(0),
        )?;
        *block = self.set_static_property_value_raw(body, *block, &math, name, &value)?;
    }

    const BINARY: &[(&str, Operator)] = &[("min", Operator::F64Min), ("max", Operator::F64Max)];
    for (name, op) in BINARY {
        let func = self.math_binary_method(name, *op)?;
        let (context, _) = math.wasm()?;
        let value = self.primordial_function_value(
            body,
            *block,
            context,
            func,
            static_primordial_tag("Math", name).unwrap_or(0),
        )?;
        *block = self.set_static_property_value_raw(body, *block, &math, name, &value)?;
    }

    for (name, func) in [
        ("sign", self.math_sign_method()?),
        ("round", self.math_round_method()?),
        ("fround", self.math_fround_method()?),
        ("imul", self.math_imul_method()?),
    ] {
        let (context, _) = math.wasm()?;
        let value = self.primordial_function_value(
            body,
            *block,
            context,
            func,
            static_primordial_tag("Math", name).unwrap_or(0),
        )?;
        *block = self.set_static_property_value_raw(body, *block, &math, name, &value)?;
    }

    Ok(math)
}

fn build_array_namespace(
    &mut self,
    body: &mut FunctionBody,
    block: &mut Block,
) -> Result<LowerValue, ConvertError> {
    let array = self.new_object(body, *block)?;
    let func = self.build_native_adapter(
        "array_is_array",
        |this, body, entry, _context, _this_val, args| {
            let (join, raw) = this.read_arg_raw(body, entry, args, 0);
            let is_null = body.add_op(join, Operator::RefIsNull, &[raw], &[Type::I32]);
            let non_null = body.add_block();
            let is_object = body.add_block();
            let not_array = body.add_block();
            let result_join = body.add_block();
            let result = body.add_blockparam(result_join, Type::I32);
            body.set_terminator(
                join,
                Terminator::CondBr {
                    cond: is_null,
                    if_true: BlockTarget {
                        block: not_array,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: non_null,
                        args: vec![],
                    },
                },
            );
            let matches_object = body.add_op(
                non_null,
                Operator::RefTest {
                    ty: this.repr.object_ty(),
                },
                &[raw],
                &[Type::I32],
            );
            body.set_terminator(
                non_null,
                Terminator::CondBr {
                    cond: matches_object,
                    if_true: BlockTarget {
                        block: is_object,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: not_array,
                        args: vec![],
                    },
                },
            );
            let plain = body.add_op(
                is_object,
                Operator::RefCast {
                    ty: this.repr.object_ty(),
                },
                &[raw],
                &[this.repr.object_ty()],
            );
            let elements = body.add_op(
                is_object,
                Operator::StructGet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain],
                &[this.repr.arguments_ty()],
            );
            let has_elements = body.add_op(is_object, Operator::RefIsNull, &[elements], &[Type::I32]);
            let is_array = body.add_op(is_object, Operator::I32Eqz, &[has_elements], &[Type::I32]);
            body.set_terminator(
                is_object,
                Terminator::Br {
                    target: BlockTarget {
                        block: result_join,
                        args: vec![is_array],
                    },
                },
            );
            let zero = body.add_op(not_array, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            body.set_terminator(
                not_array,
                Terminator::Br {
                    target: BlockTarget {
                        block: result_join,
                        args: vec![zero],
                    },
                },
            );

            let boxed = this.box_value(
                body,
                result_join,
                &LowerValue::Wasm {
                    value: result,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                result_join,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = array.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &array, "isArray", &value)?;
    Ok(array)
}

// ---------------------------------------------------------------------
// Reflect / Object: descriptor manipulation
//
// `keys`/`values`/`entries`/`getOwnPropertyNames`/`assign`/`freeze`/
// `isFrozen`/`ownKeys`/`construct` are not implemented yet — they need a
// runtime property-enumeration primitive (walking every trie bucket and
// every registered shape's field set) that nothing in this crate builds
// today. `getPrototypeOf`/`setPrototypeOf`/`isExtensible`/
// `preventExtensions` are accepted but not enforced, since there is no real
// prototype-chain or extensibility tracking anywhere in this crate.
// ---------------------------------------------------------------------

/// `flags & mask != 0` as a clean JS boolean (0/1) i32.
fn slot_flag_bit(&self, body: &mut FunctionBody, block: Block, flags: Value, mask: i32) -> Value {
    let mask = body.add_op(
        block,
        Operator::I32Const {
            value: mask as u32,
        },
        &[],
        &[Type::I32],
    );
    let and = body.add_op(block, Operator::I32And, &[flags, mask], &[Type::I32]);
    let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    body.add_op(block, Operator::I32Ne, &[and, zero], &[Type::I32])
}

/// `value != null` as a clean JS boolean (0/1) i32 — "is this call argument
/// actually present" (used to distinguish a data vs. accessor descriptor).
fn is_present(&self, body: &mut FunctionBody, block: Block, value: Value) -> Value {
    let is_null = body.add_op(block, Operator::RefIsNull, &[value], &[Type::I32]);
    body.add_op(block, Operator::I32Eqz, &[is_null], &[Type::I32])
}

/// `Object.getOwnPropertyDescriptor(target, key)` / `Reflect.
/// getOwnPropertyDescriptor(target, key)`. Reads the raw stored slot
/// (bypassing accessor invocation) and reports its real, tracked
/// `writable`/`enumerable`/`configurable` bits — no defaulting needed, since
/// those attributes are genuinely stored per property.
fn object_get_own_property_descriptor(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    target: &LowerValue,
    key: Value,
) -> Result<(Block, LowerValue), ConvertError> {
    let (block, root) = self.object_and_root(body, block, target)?;
    let helpers = self.ensure_property_helpers()?;
    let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    let slot = body.add_op(
        block,
        Operator::Call {
            function_index: helpers.lookup,
        },
        &[root, key, zero],
        &[self.repr.slot_ty()],
    );
    let is_null = body.add_op(block, Operator::RefIsNull, &[slot], &[Type::I32]);
    let absent = body.add_block();
    let present = body.add_block();
    let join = body.add_block();
    let result = body.add_blockparam(join, self.repr.value);
    body.set_terminator(
        block,
        Terminator::CondBr {
            cond: is_null,
            if_true: BlockTarget {
                block: absent,
                args: vec![],
            },
            if_false: BlockTarget {
                block: present,
                args: vec![],
            },
        },
    );
    let undef = body.add_op(
        absent,
        Operator::RefNull {
            ty: self.repr.value,
        },
        &[],
        &[self.repr.value],
    );
    body.set_terminator(
        absent,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![undef],
            },
        },
    );

    let cast = body.add_op(
        present,
        Operator::RefCast {
            ty: self.repr.slot_non_null_ty(),
        },
        &[slot],
        &[self.repr.slot_non_null_ty()],
    );
    let raw_value = body.add_op(
        present,
        Operator::StructGet {
            sig: self.repr.slot,
            idx: 0,
        },
        &[cast],
        &[self.repr.value],
    );
    let flags = body.add_op(
        present,
        Operator::StructGet {
            sig: self.repr.slot,
            idx: 1,
        },
        &[cast],
        &[Type::I32],
    );
    let enumerable = self.slot_flag_bit(body, present, flags, crate::repr::SLOT_ENUMERABLE);
    let configurable = self.slot_flag_bit(body, present, flags, crate::repr::SLOT_CONFIGURABLE);
    let enumerable = self.box_value(
        body,
        present,
        &LowerValue::Wasm {
            value: enumerable,
            kind: ValueKind::Boolean,
        },
    )?;
    let configurable = self.box_value(
        body,
        present,
        &LowerValue::Wasm {
            value: configurable,
            kind: ValueKind::Boolean,
        },
    )?;

    let is_descriptor = body.add_op(
        present,
        Operator::RefTest {
            ty: self.repr.descriptor_non_null_ty(),
        },
        &[raw_value],
        &[Type::I32],
    );
    let accessor = body.add_block();
    let data = body.add_block();
    body.set_terminator(
        present,
        Terminator::CondBr {
            cond: is_descriptor,
            if_true: BlockTarget {
                block: accessor,
                args: vec![],
            },
            if_false: BlockTarget {
                block: data,
                args: vec![],
            },
        },
    );

    let descriptor_cast = body.add_op(
        accessor,
        Operator::RefCast {
            ty: self.repr.descriptor_ty(),
        },
        &[raw_value],
        &[self.repr.descriptor_ty()],
    );
    let getter = body.add_op(
        accessor,
        Operator::StructGet {
            sig: self.repr.descriptor,
            idx: 0,
        },
        &[descriptor_cast],
        &[self.repr.value],
    );
    let setter = body.add_op(
        accessor,
        Operator::StructGet {
            sig: self.repr.descriptor,
            idx: 1,
        },
        &[descriptor_cast],
        &[self.repr.value],
    );
    let accessor_result = self.new_object(body, accessor)?;
    let mut accessor_block = accessor;
    accessor_block = self.set_static_property_value_raw(
        body,
        accessor_block,
        &accessor_result,
        "get",
        &LowerValue::Wasm {
            value: getter,
            kind: ValueKind::Reference,
        },
    )?;
    accessor_block = self.set_static_property_value_raw(
        body,
        accessor_block,
        &accessor_result,
        "set",
        &LowerValue::Wasm {
            value: setter,
            kind: ValueKind::Reference,
        },
    )?;
    accessor_block = self.set_static_property_value_raw(
        body,
        accessor_block,
        &accessor_result,
        "enumerable",
        &LowerValue::Wasm {
            value: enumerable,
            kind: ValueKind::Reference,
        },
    )?;
    accessor_block = self.set_static_property_value_raw(
        body,
        accessor_block,
        &accessor_result,
        "configurable",
        &LowerValue::Wasm {
            value: configurable,
            kind: ValueKind::Reference,
        },
    )?;
    let accessor_result_value = self.box_value(body, accessor_block, &accessor_result)?;
    body.set_terminator(
        accessor_block,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![accessor_result_value],
            },
        },
    );

    let writable = self.slot_flag_bit(body, data, flags, crate::repr::SLOT_WRITABLE);
    let writable = self.box_value(
        body,
        data,
        &LowerValue::Wasm {
            value: writable,
            kind: ValueKind::Boolean,
        },
    )?;
    let data_result = self.new_object(body, data)?;
    let mut data_block = data;
    data_block = self.set_static_property_value_raw(
        body,
        data_block,
        &data_result,
        "value",
        &LowerValue::Wasm {
            value: raw_value,
            kind: ValueKind::Reference,
        },
    )?;
    data_block = self.set_static_property_value_raw(
        body,
        data_block,
        &data_result,
        "writable",
        &LowerValue::Wasm {
            value: writable,
            kind: ValueKind::Reference,
        },
    )?;
    data_block = self.set_static_property_value_raw(
        body,
        data_block,
        &data_result,
        "enumerable",
        &LowerValue::Wasm {
            value: enumerable,
            kind: ValueKind::Reference,
        },
    )?;
    data_block = self.set_static_property_value_raw(
        body,
        data_block,
        &data_result,
        "configurable",
        &LowerValue::Wasm {
            value: configurable,
            kind: ValueKind::Reference,
        },
    )?;
    let data_result_value = self.box_value(body, data_block, &data_result)?;
    body.set_terminator(
        data_block,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![data_result_value],
            },
        },
    );

    Ok((
        join,
        LowerValue::Wasm {
            value: result,
            kind: ValueKind::Reference,
        },
    ))
}

/// `Object.defineProperty(target, key, descriptor)` / `Reflect.
/// defineProperty(target, key, descriptor)`. Installs a data or accessor
/// property (whichever the descriptor specifies) with real, tracked
/// attribute flags. Attributes absent from `descriptor` default to `false`,
/// matching spec behavior for a freshly-defined property; this milestone
/// does not merge with an existing property's attributes on a partial
/// update.
fn object_define_property(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    target: &LowerValue,
    key: Value,
    descriptor: &LowerValue,
) -> Result<Block, ConvertError> {
    let (block, value_prop) = self.get_property_value_raw(body, block, descriptor, "value")?;
    let (block, get_prop) = self.get_property_value_raw(body, block, descriptor, "get")?;
    let (block, set_prop) = self.get_property_value_raw(body, block, descriptor, "set")?;
    let (block, writable_prop) =
        self.get_property_value_raw(body, block, descriptor, "writable")?;
    let (block, enumerable_prop) =
        self.get_property_value_raw(body, block, descriptor, "enumerable")?;
    let (block, configurable_prop) =
        self.get_property_value_raw(body, block, descriptor, "configurable")?;

    let writable = self.as_condition(body, block, &writable_prop)?;
    let enumerable = self.as_condition(body, block, &enumerable_prop)?;
    let configurable = self.as_condition(body, block, &configurable_prop)?;
    let one = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
    let two = body.add_op(block, Operator::I32Const { value: 2 }, &[], &[Type::I32]);
    let enumerable_bit = body.add_op(block, Operator::I32Shl, &[enumerable, one], &[Type::I32]);
    let configurable_bit = body.add_op(block, Operator::I32Shl, &[configurable, two], &[Type::I32]);
    let flags = body.add_op(block, Operator::I32Or, &[writable, enumerable_bit], &[Type::I32]);
    let flags = body.add_op(block, Operator::I32Or, &[flags, configurable_bit], &[Type::I32]);

    let (get_raw, _) = get_prop.wasm()?;
    let (set_raw, _) = set_prop.wasm()?;
    let has_get = self.is_present(body, block, get_raw);
    let has_set = self.is_present(body, block, set_raw);
    let is_accessor = body.add_op(block, Operator::I32Or, &[has_get, has_set], &[Type::I32]);

    let accessor = body.add_block();
    let data = body.add_block();
    let join = body.add_block();
    let final_value = body.add_blockparam(join, self.repr.value);
    body.set_terminator(
        block,
        Terminator::CondBr {
            cond: is_accessor,
            if_true: BlockTarget {
                block: accessor,
                args: vec![],
            },
            if_false: BlockTarget {
                block: data,
                args: vec![],
            },
        },
    );
    let descriptor_struct = self.new_descriptor(body, accessor);
    body.add_op(
        accessor,
        Operator::StructSet {
            sig: self.repr.descriptor,
            idx: 0,
        },
        &[descriptor_struct, get_raw],
        &[],
    );
    body.add_op(
        accessor,
        Operator::StructSet {
            sig: self.repr.descriptor,
            idx: 1,
        },
        &[descriptor_struct, set_raw],
        &[],
    );
    let descriptor_any = self.anyref(body, accessor, descriptor_struct);
    body.set_terminator(
        accessor,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![descriptor_any],
            },
        },
    );
    let (value_raw, _) = value_prop.wasm()?;
    body.set_terminator(
        data,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![value_raw],
            },
        },
    );

    let slot = self.new_slot(body, join, final_value, flags);
    let (join, root) = self.object_and_root(body, join, target)?;
    let helpers = self.ensure_property_helpers()?;
    let zero = body.add_op(join, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    body.add_op(
        join,
        Operator::Call {
            function_index: helpers.set,
        },
        &[root, key, slot, zero],
        &[],
    );
    Ok(join)
}

fn build_reflect_namespace(
    &mut self,
    body: &mut FunctionBody,
    block: &mut Block,
) -> Result<LowerValue, ConvertError> {
    let reflect = self.new_object(body, *block)?;

    let func = self.build_native_adapter(
        "reflect_get",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, raw) = this.get_string_member_raw(body, block, &target, key)?;
            let (block, resolved) = this.resolve_property_read_join(body, block, &target, raw)?;
            let boxed = this.box_value(body, block, &resolved)?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "get", &value)?;

    let func = self.build_native_adapter(
        "reflect_set",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let (block, new_value) = this.read_arg_raw(body, block, args, 2);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let new_value = LowerValue::Wasm {
                value: new_value,
                kind: ValueKind::Reference,
            };
            let mut written = this.set_string_member(body, block, &target, key, &new_value)?;
            let block = written
                .pop()
                .ok_or_else(|| ConvertError::invalid("Reflect.set produced no continuation"))?;
            let true_value = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let boxed = this.box_value(
                body,
                block,
                &LowerValue::Wasm {
                    value: true_value,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "set", &value)?;

    let func = self.build_native_adapter(
        "reflect_has",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, raw) = this.get_string_member_raw(body, block, &target, key)?;
            let (raw_value, _) = raw.wasm()?;
            let has = this.is_present(body, block, raw_value);
            let boxed = this.box_value(
                body,
                block,
                &LowerValue::Wasm {
                    value: has,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "has", &value)?;

    let func = self.build_native_adapter(
        "reflect_delete_property",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, root) = this.object_and_root(body, block, &target)?;
            let helpers = this.ensure_property_helpers()?;
            let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let null_slot = body.add_op(
                block,
                Operator::RefNull {
                    ty: this.repr.slot_ty(),
                },
                &[],
                &[this.repr.slot_ty()],
            );
            body.add_op(
                block,
                Operator::Call {
                    function_index: helpers.set,
                },
                &[root, key, null_slot, zero],
                &[],
            );
            let true_value = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let boxed = this.box_value(
                body,
                block,
                &LowerValue::Wasm {
                    value: true_value,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "deleteProperty", &value)?;

    let func = self.build_native_adapter(
        "reflect_apply",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, this_arg_raw) = this.read_arg_raw(body, block, args, 1);
            let (block, args_array_raw) = this.read_arg_raw(body, block, args, 2);
            let is_object = body.add_op(
                block,
                Operator::RefTest {
                    ty: this.repr.object_ty(),
                },
                &[args_array_raw],
                &[Type::I32],
            );
            let has_array = body.add_block();
            let no_array = body.add_block();
            let elements_join = body.add_block();
            let elements = body.add_blockparam(elements_join, this.repr.arguments_ty());
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: is_object,
                    if_true: BlockTarget {
                        block: has_array,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: no_array,
                        args: vec![],
                    },
                },
            );
            let plain = body.add_op(
                has_array,
                Operator::RefCast {
                    ty: this.repr.object_ty(),
                },
                &[args_array_raw],
                &[this.repr.object_ty()],
            );
            let found_elements = body.add_op(
                has_array,
                Operator::StructGet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain],
                &[this.repr.arguments_ty()],
            );
            body.set_terminator(
                has_array,
                Terminator::Br {
                    target: BlockTarget {
                        block: elements_join,
                        args: vec![found_elements],
                    },
                },
            );
            let empty = body.add_op(
                no_array,
                Operator::ArrayNewFixed {
                    sig: this.repr.arguments,
                    num: 0,
                },
                &[],
                &[this.repr.arguments_ty()],
            );
            body.set_terminator(
                no_array,
                Terminator::Br {
                    target: BlockTarget {
                        block: elements_join,
                        args: vec![empty],
                    },
                },
            );

            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let this_arg = LowerValue::Wasm {
                value: this_arg_raw,
                kind: ValueKind::Reference,
            };
            let (context, call_this, code, _arrow) =
                this.callable_parts(body, elements_join, &target, this_arg)?;
            let result = body.add_op(
                elements_join,
                Operator::CallRef {
                    sig_index: this.repr.adapter,
                },
                &[context, call_this, elements, code],
                &[this.repr.value],
            );
            body.set_terminator(
                elements_join,
                Terminator::Return {
                    values: vec![result],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "apply", &value)?;

    let func = self.build_native_adapter(
        "reflect_define_property",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let (block, descriptor_raw) = this.read_arg_raw(body, block, args, 2);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let descriptor = LowerValue::Wasm {
                value: descriptor_raw,
                kind: ValueKind::Reference,
            };
            let block = this.object_define_property(body, block, &target, key, &descriptor)?;
            let true_value = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let boxed = this.box_value(
                body,
                block,
                &LowerValue::Wasm {
                    value: true_value,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "defineProperty", &value)?;

    let func = self.build_native_adapter(
        "reflect_get_own_property_descriptor",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, result) = this.object_get_own_property_descriptor(body, block, &target, key)?;
            let boxed = this.box_value(body, block, &result)?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(
        body,
        *block,
        &reflect,
        "getOwnPropertyDescriptor",
        &value,
    )?;

    for (name, literal) in [
        ("setPrototypeOf", true),
        ("isExtensible", true),
        ("preventExtensions", true),
    ] {
        let func = self.build_native_adapter(name, move |this, body, entry, _c, _t, _a| {
            let bit = body.add_op(
                entry,
                Operator::I32Const {
                    value: u32::from(literal),
                },
                &[],
                &[Type::I32],
            );
            let boxed = this.box_value(
                body,
                entry,
                &LowerValue::Wasm {
                    value: bit,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                entry,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        })?;
        let (context, _) = reflect.wasm()?;
        let value = self.native_function_value(body, *block, context, func)?;
        *block = self.set_static_property_value_raw(body, *block, &reflect, name, &value)?;
    }

    let func = self.build_native_adapter(
        "reflect_get_prototype_of",
        |this, body, entry, _c, _t, _a| {
            let undef = body.add_op(
                entry,
                Operator::RefNull {
                    ty: this.repr.value,
                },
                &[],
                &[this.repr.value],
            );
            body.set_terminator(entry, Terminator::Return { values: vec![undef] });
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "getPrototypeOf", &value)?;

    let func = self.build_native_adapter(
        "reflect_own_keys",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &target, false)?;
            let array_obj = this.new_array_object(body, block, keys)?;
            let boxed = this.anyref(body, block, array_obj);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = reflect.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &reflect, "ownKeys", &value)?;

    Ok(reflect)
}

fn build_object_namespace(
    &mut self,
    body: &mut FunctionBody,
    block: &mut Block,
) -> Result<LowerValue, ConvertError> {
    let object_ns = self.new_object(body, *block)?;

    let func = self.build_native_adapter(
        "object_define_property",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let (block, descriptor_raw) = this.read_arg_raw(body, block, args, 2);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let descriptor = LowerValue::Wasm {
                value: descriptor_raw,
                kind: ValueKind::Reference,
            };
            let block = this.object_define_property(body, block, &target, key, &descriptor)?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![target_raw],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block =
        self.set_static_property_value_raw(body, *block, &object_ns, "defineProperty", &value)?;

    let func = self.build_native_adapter(
        "object_get_own_property_descriptor",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, key) = this.read_arg_string_key(body, block, args, 1)?;
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, result) = this.object_get_own_property_descriptor(body, block, &target, key)?;
            let boxed = this.box_value(body, block, &result)?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(
        body,
        *block,
        &object_ns,
        "getOwnPropertyDescriptor",
        &value,
    )?;

    let func = self.build_native_adapter("object_create", |this, body, entry, _c, _t, _a| {
        let created = this.new_object(body, entry)?;
        let boxed = this.box_value(body, entry, &created)?;
        body.set_terminator(entry, Terminator::Return { values: vec![boxed] });
        Ok(())
    })?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "create", &value)?;

    let func = self.build_native_adapter(
        "object_get_prototype_of",
        |this, body, entry, _c, _t, _a| {
            let undef = body.add_op(
                entry,
                Operator::RefNull {
                    ty: this.repr.value,
                },
                &[],
                &[this.repr.value],
            );
            body.set_terminator(entry, Terminator::Return { values: vec![undef] });
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block =
        self.set_static_property_value_raw(body, *block, &object_ns, "getPrototypeOf", &value)?;

    let func = self.build_native_adapter(
        "object_set_prototype_of",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![target_raw],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block =
        self.set_static_property_value_raw(body, *block, &object_ns, "setPrototypeOf", &value)?;

    let func = self.build_native_adapter(
        "object_keys",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &target, true)?;
            let array_obj = this.new_array_object(body, block, keys)?;
            let boxed = this.anyref(body, block, array_obj);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "keys", &value)?;

    let func = self.build_native_adapter(
        "object_values",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &target, true)?;
            let len = body.add_op(block, Operator::ArrayLen, &[keys], &[Type::I32]);
            let values_array = body.add_op(
                block,
                Operator::ArrayNewDefault {
                    sig: this.repr.arguments,
                },
                &[len],
                &[this.repr.arguments_ty()],
            );
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let key = body.add_op(
                    block,
                    Operator::ArrayGet {
                        sig: this.repr.arguments,
                    },
                    &[keys, i],
                    &[this.repr.value],
                );
                let key_string = body.add_op(
                    block,
                    Operator::RefCast {
                        ty: this.repr.string_ty(),
                    },
                    &[key],
                    &[this.repr.string_ty()],
                );
                let (block, raw) = this.get_string_member_raw(body, block, &target, key_string)?;
                let (block, resolved) = this.resolve_property_read_join(body, block, &target, raw)?;
                let boxed = this.box_value(body, block, &resolved)?;
                body.add_op(
                    block,
                    Operator::ArraySet {
                        sig: this.repr.arguments,
                    },
                    &[values_array, i, boxed],
                    &[],
                );
                Ok(block)
            })?;
            let array_obj = this.new_array_object(body, block, values_array)?;
            let boxed = this.anyref(body, block, array_obj);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "values", &value)?;

    let func = self.build_native_adapter(
        "object_entries",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &target, true)?;
            let len = body.add_op(block, Operator::ArrayLen, &[keys], &[Type::I32]);
            let entries_array = body.add_op(
                block,
                Operator::ArrayNewDefault {
                    sig: this.repr.arguments,
                },
                &[len],
                &[this.repr.arguments_ty()],
            );
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let key = body.add_op(
                    block,
                    Operator::ArrayGet {
                        sig: this.repr.arguments,
                    },
                    &[keys, i],
                    &[this.repr.value],
                );
                let key_string = body.add_op(
                    block,
                    Operator::RefCast {
                        ty: this.repr.string_ty(),
                    },
                    &[key],
                    &[this.repr.string_ty()],
                );
                let (block, raw) = this.get_string_member_raw(body, block, &target, key_string)?;
                let (block, resolved) = this.resolve_property_read_join(body, block, &target, raw)?;
                let value_boxed = this.box_value(body, block, &resolved)?;
                let pair = body.add_op(
                    block,
                    Operator::ArrayNewFixed {
                        sig: this.repr.arguments,
                        num: 2,
                    },
                    &[key, value_boxed],
                    &[this.repr.arguments_ty()],
                );
                let pair_obj = this.new_array_object(body, block, pair)?;
                let pair_boxed = this.anyref(body, block, pair_obj);
                body.add_op(
                    block,
                    Operator::ArraySet {
                        sig: this.repr.arguments,
                    },
                    &[entries_array, i, pair_boxed],
                    &[],
                );
                Ok(block)
            })?;
            let array_obj = this.new_array_object(body, block, entries_array)?;
            let boxed = this.anyref(body, block, array_obj);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "entries", &value)?;

    let func = self.build_native_adapter(
        "object_get_own_property_names",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &target, false)?;
            let array_obj = this.new_array_object(body, block, keys)?;
            let boxed = this.anyref(body, block, array_obj);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(
        body,
        *block,
        &object_ns,
        "getOwnPropertyNames",
        &value,
    )?;

    let func = self.build_native_adapter(
        "object_get_own_property_descriptors",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &target, false)?;
            let len = body.add_op(block, Operator::ArrayLen, &[keys], &[Type::I32]);
            let result = this.new_object(body, block)?;
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let key = body.add_op(
                    block,
                    Operator::ArrayGet {
                        sig: this.repr.arguments,
                    },
                    &[keys, i],
                    &[this.repr.value],
                );
                let key_string = body.add_op(
                    block,
                    Operator::RefCast {
                        ty: this.repr.string_ty(),
                    },
                    &[key],
                    &[this.repr.string_ty()],
                );
                let (block, descriptor) =
                    this.object_get_own_property_descriptor(body, block, &target, key_string)?;
                this.set_dynamic_property_raw(body, block, &result, key_string, &descriptor)
            })?;
            let boxed = this.box_value(body, block, &result)?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(
        body,
        *block,
        &object_ns,
        "getOwnPropertyDescriptors",
        &value,
    )?;

    let func = self.build_native_adapter(
        "object_assign",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let (block, source_raw) = this.read_arg_raw(body, block, args, 1);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let source = LowerValue::Wasm {
                value: source_raw,
                kind: ValueKind::Reference,
            };
            let (block, keys) = this.enumerate_own_keys(body, block, &source, true)?;
            let len = body.add_op(block, Operator::ArrayLen, &[keys], &[Type::I32]);
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let key = body.add_op(
                    block,
                    Operator::ArrayGet {
                        sig: this.repr.arguments,
                    },
                    &[keys, i],
                    &[this.repr.value],
                );
                let key_string = body.add_op(
                    block,
                    Operator::RefCast {
                        ty: this.repr.string_ty(),
                    },
                    &[key],
                    &[this.repr.string_ty()],
                );
                let (block, raw) = this.get_string_member_raw(body, block, &source, key_string)?;
                let (block, resolved) = this.resolve_property_read_join(body, block, &source, raw)?;
                let mut written = this.set_string_member(body, block, &target, key_string, &resolved)?;
                written
                    .pop()
                    .ok_or_else(|| ConvertError::invalid("Object.assign produced no continuation"))
            })?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![target_raw],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "assign", &value)?;

    let func = self.build_native_adapter(
        "object_freeze",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, root) = this.object_and_root(body, block, &target)?;
            let helpers = this.ensure_property_helpers()?;
            let (block, keys) = this.enumerate_own_keys(body, block, &target, false)?;
            let len = body.add_op(block, Operator::ArrayLen, &[keys], &[Type::I32]);
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let key = body.add_op(
                    block,
                    Operator::ArrayGet {
                        sig: this.repr.arguments,
                    },
                    &[keys, i],
                    &[this.repr.value],
                );
                let key_string = body.add_op(
                    block,
                    Operator::RefCast {
                        ty: this.repr.string_ty(),
                    },
                    &[key],
                    &[this.repr.string_ty()],
                );
                let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
                let slot = body.add_op(
                    block,
                    Operator::Call {
                        function_index: helpers.lookup,
                    },
                    &[root, key_string, zero],
                    &[this.repr.slot_ty()],
                );
                let cast = body.add_op(
                    block,
                    Operator::RefCast {
                        ty: this.repr.slot_non_null_ty(),
                    },
                    &[slot],
                    &[this.repr.slot_non_null_ty()],
                );
                let value = body.add_op(
                    block,
                    Operator::StructGet {
                        sig: this.repr.slot,
                        idx: 0,
                    },
                    &[cast],
                    &[this.repr.value],
                );
                let flags = body.add_op(
                    block,
                    Operator::StructGet {
                        sig: this.repr.slot,
                        idx: 1,
                    },
                    &[cast],
                    &[Type::I32],
                );
                let mask = body.add_op(
                    block,
                    Operator::I32Const {
                        value: !(crate::repr::SLOT_WRITABLE | crate::repr::SLOT_CONFIGURABLE) as u32,
                    },
                    &[],
                    &[Type::I32],
                );
                let new_flags = body.add_op(block, Operator::I32And, &[flags, mask], &[Type::I32]);
                let new_slot = this.new_slot(body, block, value, new_flags);
                body.add_op(
                    block,
                    Operator::Call {
                        function_index: helpers.set,
                    },
                    &[root, key_string, new_slot, zero],
                    &[],
                );
                Ok(block)
            })?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![target_raw],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "freeze", &value)?;

    let func = self.build_native_adapter(
        "object_is_frozen",
        |this, body, entry, _context, _this_val, args| {
            let (block, target_raw) = this.read_arg_raw(body, entry, args, 0);
            let target = LowerValue::Wasm {
                value: target_raw,
                kind: ValueKind::Reference,
            };
            let (block, root) = this.object_and_root(body, block, &target)?;
            let helpers = this.ensure_property_helpers()?;
            let (block, keys) = this.enumerate_own_keys(body, block, &target, false)?;
            let len = body.add_op(block, Operator::ArrayLen, &[keys], &[Type::I32]);
            let one = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let (block, frozen) = this.for_each_index_fold(
                body,
                block,
                len,
                one,
                Type::I32,
                |this, body, block, i, acc| {
                    let key = body.add_op(
                        block,
                        Operator::ArrayGet {
                            sig: this.repr.arguments,
                        },
                        &[keys, i],
                        &[this.repr.value],
                    );
                    let key_string = body.add_op(
                        block,
                        Operator::RefCast {
                            ty: this.repr.string_ty(),
                        },
                        &[key],
                        &[this.repr.string_ty()],
                    );
                    let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
                    let slot = body.add_op(
                        block,
                        Operator::Call {
                            function_index: helpers.lookup,
                        },
                        &[root, key_string, zero],
                        &[this.repr.slot_ty()],
                    );
                    let cast = body.add_op(
                        block,
                        Operator::RefCast {
                            ty: this.repr.slot_non_null_ty(),
                        },
                        &[slot],
                        &[this.repr.slot_non_null_ty()],
                    );
                    let flags = body.add_op(
                        block,
                        Operator::StructGet {
                            sig: this.repr.slot,
                            idx: 1,
                        },
                        &[cast],
                        &[Type::I32],
                    );
                    let writable = this.slot_flag_bit(body, block, flags, crate::repr::SLOT_WRITABLE);
                    let configurable =
                        this.slot_flag_bit(body, block, flags, crate::repr::SLOT_CONFIGURABLE);
                    let either = body.add_op(block, Operator::I32Or, &[writable, configurable], &[Type::I32]);
                    let this_frozen = body.add_op(block, Operator::I32Eqz, &[either], &[Type::I32]);
                    let new_acc = body.add_op(block, Operator::I32And, &[acc, this_frozen], &[Type::I32]);
                    Ok((block, new_acc))
                },
            )?;
            let boxed = this.box_value(
                body,
                block,
                &LowerValue::Wasm {
                    value: frozen,
                    kind: ValueKind::Boolean,
                },
            )?;
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )?;
    let (context, _) = object_ns.wasm()?;
    let value = self.native_function_value(body, *block, context, func)?;
    *block = self.set_static_property_value_raw(body, *block, &object_ns, "isFrozen", &value)?;

    Ok(object_ns)
}

// ---------------------------------------------------------------------
// Array instance methods (`push`/`pop`/`map`/`forEach`) — dispatched by
// `Converter::get_array_method_or_property` in conv.rs whenever a receiver
// with a non-null `elements` field is asked for one of these names. See
// that function's doc comment for why this is intrinsic dispatch rather
// than a real, inheritable `Array.prototype`.
// ---------------------------------------------------------------------

fn ensure_array_instance_method(&mut self, key: &str) -> Result<Func, ConvertError> {
    match key {
        "push" => self.array_push_method(),
        "pop" => self.array_pop_method(),
        "map" => self.array_map_method(),
        "forEach" => self.array_for_each_method(),
        _ => Err(ConvertError::invalid(format!(
            "{key:?} is not a recognized array instance method"
        ))),
    }
}

fn array_push_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter(
        "array_push",
        |this, body, entry, _context, this_val, args| {
            let plain = body.add_op(
                entry,
                Operator::RefCast {
                    ty: this.repr.object_ty(),
                },
                &[this_val],
                &[this.repr.object_ty()],
            );
            let old_elements = body.add_op(
                entry,
                Operator::StructGet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain],
                &[this.repr.arguments_ty()],
            );
            let old_len = body.add_op(entry, Operator::ArrayLen, &[old_elements], &[Type::I32]);
            let items_len = body.add_op(entry, Operator::ArrayLen, &[args], &[Type::I32]);
            let new_len = body.add_op(entry, Operator::I32Add, &[old_len, items_len], &[Type::I32]);
            let new_elements = body.add_op(
                entry,
                Operator::ArrayNewDefault {
                    sig: this.repr.arguments,
                },
                &[new_len],
                &[this.repr.arguments_ty()],
            );
            let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            body.add_op(
                entry,
                Operator::ArrayCopy {
                    dest: this.repr.arguments,
                    src: this.repr.arguments,
                },
                &[new_elements, zero, old_elements, zero, old_len],
                &[],
            );
            body.add_op(
                entry,
                Operator::ArrayCopy {
                    dest: this.repr.arguments,
                    src: this.repr.arguments,
                },
                &[new_elements, old_len, args, zero, items_len],
                &[],
            );
            body.add_op(
                entry,
                Operator::StructSet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain, new_elements],
                &[],
            );
            let new_len_f64 = body.add_op(entry, Operator::F64ConvertI32S, &[new_len], &[Type::F64]);
            this.return_number(body, entry, new_len_f64);
            Ok(())
        },
    )
}

fn array_pop_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter(
        "array_pop",
        |this, body, entry, _context, this_val, _args| {
            let plain = body.add_op(
                entry,
                Operator::RefCast {
                    ty: this.repr.object_ty(),
                },
                &[this_val],
                &[this.repr.object_ty()],
            );
            let elements = body.add_op(
                entry,
                Operator::StructGet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain],
                &[this.repr.arguments_ty()],
            );
            let len = body.add_op(entry, Operator::ArrayLen, &[elements], &[Type::I32]);
            let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let is_empty = body.add_op(entry, Operator::I32Eq, &[len, zero], &[Type::I32]);
            let empty_block = body.add_block();
            let nonempty_block = body.add_block();
            let join = body.add_block();
            let result = body.add_blockparam(join, this.repr.value);
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: is_empty,
                    if_true: BlockTarget {
                        block: empty_block,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: nonempty_block,
                        args: vec![],
                    },
                },
            );
            let undef = body.add_op(
                empty_block,
                Operator::RefNull {
                    ty: this.repr.value,
                },
                &[],
                &[this.repr.value],
            );
            body.set_terminator(
                empty_block,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![undef],
                    },
                },
            );
            let one = body.add_op(nonempty_block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let new_len = body.add_op(nonempty_block, Operator::I32Sub, &[len, one], &[Type::I32]);
            let last = body.add_op(
                nonempty_block,
                Operator::ArrayGet {
                    sig: this.repr.arguments,
                },
                &[elements, new_len],
                &[this.repr.value],
            );
            let trimmed = body.add_op(
                nonempty_block,
                Operator::ArrayNewDefault {
                    sig: this.repr.arguments,
                },
                &[new_len],
                &[this.repr.arguments_ty()],
            );
            body.add_op(
                nonempty_block,
                Operator::ArrayCopy {
                    dest: this.repr.arguments,
                    src: this.repr.arguments,
                },
                &[trimmed, zero, elements, zero, new_len],
                &[],
            );
            body.add_op(
                nonempty_block,
                Operator::StructSet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain, trimmed],
                &[],
            );
            body.set_terminator(
                nonempty_block,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![last],
                    },
                },
            );
            body.set_terminator(join, Terminator::Return { values: vec![result] });
            Ok(())
        },
    )
}

/// Shared by `map`/`forEach`: invoke `callback(element, index, array)` (JS
/// `Array` callback signature; `thisArg` isn't supported) for element `i`
/// of `elements`, returning the raw call result.
fn array_invoke_callback(
    &mut self,
    body: &mut FunctionBody,
    block: Block,
    callback: &LowerValue,
    elements: Value,
    this_val: Value,
    i: Value,
) -> Result<(Block, Value), ConvertError> {
    let item = body.add_op(
        block,
        Operator::ArrayGet {
            sig: self.repr.arguments,
        },
        &[elements, i],
        &[self.repr.value],
    );
    let i_f64 = body.add_op(block, Operator::F64ConvertI32S, &[i], &[Type::F64]);
    let i_boxed = self.box_value(
        body,
        block,
        &LowerValue::Wasm {
            value: i_f64,
            kind: ValueKind::Number,
        },
    )?;
    let call_args = body.add_op(
        block,
        Operator::ArrayNewFixed {
            sig: self.repr.arguments,
            num: 3,
        },
        &[item, i_boxed, this_val],
        &[self.repr.arguments_ty()],
    );
    let undef_this = self.undef(body, block);
    let (context, call_this, code, _arrow) = self.callable_parts(body, block, callback, undef_this)?;
    let result = body.add_op(
        block,
        Operator::CallRef {
            sig_index: self.repr.adapter,
        },
        &[context, call_this, call_args, code],
        &[self.repr.value],
    );
    Ok((block, result))
}

fn array_for_each_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter(
        "array_for_each",
        |this, body, entry, _context, this_val, args| {
            let (block, callback_raw) = this.read_arg_raw(body, entry, args, 0);
            let callback = LowerValue::Wasm {
                value: callback_raw,
                kind: ValueKind::Reference,
            };
            let plain = body.add_op(
                block,
                Operator::RefCast {
                    ty: this.repr.object_ty(),
                },
                &[this_val],
                &[this.repr.object_ty()],
            );
            let elements = body.add_op(
                block,
                Operator::StructGet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain],
                &[this.repr.arguments_ty()],
            );
            let len = body.add_op(block, Operator::ArrayLen, &[elements], &[Type::I32]);
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let (block, _result) =
                    this.array_invoke_callback(body, block, &callback, elements, this_val, i)?;
                Ok(block)
            })?;
            let undef = body.add_op(
                block,
                Operator::RefNull {
                    ty: this.repr.value,
                },
                &[],
                &[this.repr.value],
            );
            body.set_terminator(block, Terminator::Return { values: vec![undef] });
            Ok(())
        },
    )
}

fn array_map_method(&mut self) -> Result<Func, ConvertError> {
    self.build_native_adapter(
        "array_map",
        |this, body, entry, _context, this_val, args| {
            let (block, callback_raw) = this.read_arg_raw(body, entry, args, 0);
            let callback = LowerValue::Wasm {
                value: callback_raw,
                kind: ValueKind::Reference,
            };
            let plain = body.add_op(
                block,
                Operator::RefCast {
                    ty: this.repr.object_ty(),
                },
                &[this_val],
                &[this.repr.object_ty()],
            );
            let elements = body.add_op(
                block,
                Operator::StructGet {
                    sig: this.repr.object,
                    idx: 1,
                },
                &[plain],
                &[this.repr.arguments_ty()],
            );
            let len = body.add_op(block, Operator::ArrayLen, &[elements], &[Type::I32]);
            let results = body.add_op(
                block,
                Operator::ArrayNewDefault {
                    sig: this.repr.arguments,
                },
                &[len],
                &[this.repr.arguments_ty()],
            );
            let block = this.for_each_index(body, block, len, |this, body, block, i| {
                let (block, result) =
                    this.array_invoke_callback(body, block, &callback, elements, this_val, i)?;
                body.add_op(
                    block,
                    Operator::ArraySet {
                        sig: this.repr.arguments,
                    },
                    &[results, i, result],
                    &[],
                );
                Ok(block)
            })?;
            let array_obj = this.new_array_object(body, block, results)?;
            let boxed = this.anyref(body, block, array_obj);
            body.set_terminator(
                block,
                Terminator::Return {
                    values: vec![boxed],
                },
            );
            Ok(())
        },
    )
}

}
/// Stable tag constants for primordials with a fast core. These flow into
/// the function struct's final i32 field (see `primordial_function_value`)
/// and let a call site verify "this callee is still exactly this primordial"
/// with one `i32.eq` — function references are not `eqref`, so `ref.eq`
/// cannot make the comparison. Tags must stay in sync across compiles of one
/// module only; they are never persisted.
pub(crate) const PRIMORDIAL_TAG_BASE: i32 = 1;

pub(crate) const TAG_MATH_ABS: i32 = PRIMORDIAL_TAG_BASE;
pub(crate) const TAG_MATH_FLOOR: i32 = PRIMORDIAL_TAG_BASE + 1;
pub(crate) const TAG_MATH_CEIL: i32 = PRIMORDIAL_TAG_BASE + 2;
pub(crate) const TAG_MATH_TRUNC: i32 = PRIMORDIAL_TAG_BASE + 3;
pub(crate) const TAG_MATH_SQRT: i32 = PRIMORDIAL_TAG_BASE + 4;
pub(crate) const TAG_MATH_MIN: i32 = PRIMORDIAL_TAG_BASE + 5;
pub(crate) const TAG_MATH_MAX: i32 = PRIMORDIAL_TAG_BASE + 6;
pub(crate) const TAG_MATH_SIGN: i32 = PRIMORDIAL_TAG_BASE + 7;
pub(crate) const TAG_MATH_ROUND: i32 = PRIMORDIAL_TAG_BASE + 8;
pub(crate) const TAG_MATH_FROUND: i32 = PRIMORDIAL_TAG_BASE + 9;
pub(crate) const TAG_MATH_IMUL: i32 = PRIMORDIAL_TAG_BASE + 10;
pub(crate) const TAG_ARRAY_IS_ARRAY: i32 = PRIMORDIAL_TAG_BASE + 11;

/// Static namespace-member tag lookup. `None` means the primordial has no
/// fast core (its calls keep the generic dispatch path).
fn static_primordial_tag(namespace: &str, member: &str) -> Option<i32> {
    match (namespace, member) {
        ("Math", "abs") => Some(TAG_MATH_ABS),
        ("Math", "floor") => Some(TAG_MATH_FLOOR),
        ("Math", "ceil") => Some(TAG_MATH_CEIL),
        ("Math", "trunc") => Some(TAG_MATH_TRUNC),
        ("Math", "sqrt") => Some(TAG_MATH_SQRT),
        ("Math", "min") => Some(TAG_MATH_MIN),
        ("Math", "max") => Some(TAG_MATH_MAX),
        ("Math", "sign") => Some(TAG_MATH_SIGN),
        ("Math", "round") => Some(TAG_MATH_ROUND),
        ("Math", "fround") => Some(TAG_MATH_FROUND),
        ("Math", "imul") => Some(TAG_MATH_IMUL),
        ("Array", "isArray") => Some(TAG_ARRAY_IS_ARRAY),
        _ => None,
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// Build (or fetch from cache) the fast core for a primordial tag: a
    /// plain Wasm function with an unboxed signature that the guarded and
    /// provable call paths invoke directly, skipping the generic adapter's
    /// arguments-array packing and `CallRef`. Every core has the ABI
    /// `(f64...) -> f64` for the numeric surface; the arguments are missing
    /// -argument/`undefined`-coerced to `NaN` by construction of the fast
    /// path (the provable path statically knows arity, and the guarded path
    /// passes NaN padding for absent arguments).
    fn ensure_primordial_fast_core(&mut self, tag: i32) -> Result<Func, ConvertError> {
        if let Some(func) = self.primordial_fast_cores.get(&tag) {
            return Ok(*func);
        }
        let (name, build): (&str, Box<dyn FnOnce(&mut FunctionBody, Block, &[Value])>) =
            match tag {
                TAG_MATH_ABS => ("math_abs", Box::new(|body, block, args| unary_core(body, block, &args[0], Operator::F64Abs))),
                TAG_MATH_FLOOR => ("math_floor", Box::new(|body, block, args| unary_core(body, block, &args[0], Operator::F64Floor))),
                TAG_MATH_CEIL => ("math_ceil", Box::new(|body, block, args| unary_core(body, block, &args[0], Operator::F64Ceil))),
                TAG_MATH_TRUNC => ("math_trunc", Box::new(|body, block, args| unary_core(body, block, &args[0], Operator::F64Trunc))),
                TAG_MATH_SQRT => ("math_sqrt", Box::new(|body, block, args| unary_core(body, block, &args[0], Operator::F64Sqrt))),
                TAG_MATH_MIN => ("math_min", Box::new(|body, block, args| binary_core(body, block, &args[0], &args[1], Operator::F64Min))),
                TAG_MATH_MAX => ("math_max", Box::new(|body, block, args| binary_core(body, block, &args[0], &args[1], Operator::F64Max))),
                TAG_MATH_SIGN => ("math_sign", Box::new(sign_core)),
                TAG_MATH_ROUND => ("math_round", Box::new(round_core)),
                TAG_MATH_FROUND => ("math_fround", Box::new(fround_core)),
                TAG_MATH_IMUL => ("math_imul", Box::new(imul_core)),
                _ => return Err(ConvertError::invalid("unknown primordial fast-core tag")),
            };
        let _ = name;
        let arity = match tag {
            TAG_MATH_MIN | TAG_MATH_MAX | TAG_MATH_IMUL => 2,
            _ => 1,
        };
        if self.primordial_fast_cores.contains_key(&tag) {
            return Ok(self.primordial_fast_cores[&tag]);
        }
        let sig = self.module.signatures.push(SignatureData::Func {
            params: vec![Type::F64; arity],
            returns: vec![Type::F64],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, sig);
        let entry = body.entry;
        let args: Vec<Value> = body.blocks[entry]
            .params
            .iter()
            .map(|(_, value)| *value)
            .collect();
        build(&mut body, entry, &args);
        let func = self.module.funcs.push(FuncDecl::Body(
            sig,
            format!("js_fast_core_{tag}"),
            body,
        ));
        self.primordial_fast_cores.insert(tag, func);
        Ok(func)
    }

    /// `Array.isArray` fast core: `(anyref) -> i32` on the unboxed value,
    /// mirroring the generic adapter's body without the boxing round trip.
    fn ensure_is_array_fast_core(&mut self) -> Result<Func, ConvertError> {
        if let Some(func) = self.primordial_fast_cores.get(&TAG_ARRAY_IS_ARRAY) {
            return Ok(*func);
        }
        let sig = self.module.signatures.push(SignatureData::Func {
            params: vec![self.repr.value],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, sig);
        let entry = body.entry;
        let raw = body.blocks[entry].params[0].1;
        let is_null = body.add_op(entry, Operator::RefIsNull, &[raw], &[Type::I32]);
        let non_null = body.add_block();
        let is_object = body.add_block();
        let not_array = body.add_block();
        let result_join = body.add_block();
        let result = body.add_blockparam(result_join, Type::I32);
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget {
                    block: not_array,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: non_null,
                    args: vec![],
                },
            },
        );
        let matches_object = body.add_op(
            non_null,
            Operator::RefTest {
                ty: self.repr.object_ty(),
            },
            &[raw],
            &[Type::I32],
        );
        body.set_terminator(
            non_null,
            Terminator::CondBr {
                cond: matches_object,
                if_true: BlockTarget {
                    block: is_object,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: not_array,
                    args: vec![],
                },
            },
        );
        let plain = body.add_op(
            is_object,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[raw],
            &[self.repr.object_ty()],
        );
        let elements = body.add_op(
            is_object,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 1,
            },
            &[plain],
            &[self.repr.arguments_ty()],
        );
        let has_elements = body.add_op(is_object, Operator::RefIsNull, &[elements], &[Type::I32]);
        let is_array = body.add_op(is_object, Operator::I32Eqz, &[has_elements], &[Type::I32]);
        body.set_terminator(
            is_object,
            Terminator::Br {
                target: BlockTarget {
                    block: result_join,
                    args: vec![is_array],
                },
            },
        );
        let zero = body.add_op(not_array, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            not_array,
            Terminator::Br {
                target: BlockTarget {
                    block: result_join,
                    args: vec![zero],
                },
            },
        );
        body.set_terminator(
            result_join,
            Terminator::Return {
                values: vec![result],
            },
        );
        let func = self.module.funcs.push(FuncDecl::Body(
            sig,
            "js_fast_core_is_array".to_string(),
            body,
        ));
        self.primordial_fast_cores
            .insert(TAG_ARRAY_IS_ARRAY, func);
        Ok(func)
    }

    /// Emit a guarded direct call to a primordial fast core. The callee value
    /// is checked at runtime: `ref.test` that it is a function struct, read
    /// its primordial tag, and `i32.eq` against the expected tag. On match,
    /// arguments are converted with the raw numeric/string coercions and the
    /// core is called directly; on mismatch, `fallback` produces the blocks
    /// for the ordinary generic-dispatch path.
    #[allow(clippy::too_many_arguments)]
    fn guarded_primordial_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        callee: &LowerValue,
        expected_tag: i32,
        args: &[LowerValue],
        fallback: impl FnOnce(
            &mut Self,
            &mut FunctionBody,
            Block,
        ) -> Result<Vec<(Block, LowerValue)>, ConvertError>,
    ) -> Result<Vec<(Block, LowerValue)>, ConvertError> {
        let (callee_value, kind) = callee.wasm()?;
        if kind != ValueKind::Reference {
            return fallback(self, body, block);
        }
        let is_function = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.function_non_null_ty(),
            },
            &[callee_value],
            &[Type::I32],
        );
        let check = body.add_block();
        let slow = body.add_block();
        let fast = body.add_block();
        let join = body.add_block();
        let joined = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_function,
                if_true: BlockTarget {
                    block: check,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: slow,
                    args: vec![],
                },
            },
        );
        let function = body.add_op(
            check,
            Operator::RefCast {
                ty: self.repr.function_ty(),
            },
            &[callee_value],
            &[self.repr.function_ty()],
        );
        let tag = body.add_op(
            check,
            Operator::StructGet {
                sig: self.repr.function,
                idx: FUNCTION_FIELD_TAG,
            },
            &[function],
            &[Type::I32],
        );
        let expected = body.add_op(
            check,
            Operator::I32Const {
                value: expected_tag as u32,
            },
            &[],
            &[Type::I32],
        );
        let matches = body.add_op(check, Operator::I32Eq, &[tag, expected], &[Type::I32]);
        body.set_terminator(
            check,
            Terminator::CondBr {
                cond: matches,
                if_true: BlockTarget {
                    block: fast,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: slow,
                    args: vec![],
                },
            },
        );
        // Fast path: convert each argument at its position (missing/undefined
        // → NaN matches the generic adapters' `read_arg_number` semantics) and
        // call the unboxed core.
        let mut blocks = vec![fast];
        let mut core_args = Vec::with_capacity(args.len());
        for arg in args.iter() {
            let block = *blocks.last().expect("non-empty");
            let (next, converted) = self.unbox_or_nan(body, block, arg)?;
            core_args.push(converted);
            blocks.push(next);
        }
        let core = self.ensure_primordial_fast_core(expected_tag)?;
        let last = *blocks.last().expect("non-empty");
        let call_args = self.pad_core_args(body, last, &core_args, core)?;
        let result = body.add_op(
            last,
            Operator::Call {
                function_index: core,
            },
            &call_args,
            &[Type::F64],
        );
        let boxed = self.box_value(
            body,
            last,
            &LowerValue::Wasm {
                value: result,
                kind: ValueKind::Number,
            },
        )?;
        body.set_terminator(
            last,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![boxed],
                },
            },
        );
        for (slow_block, slow_value) in fallback(self, body, slow)? {
            let slow_value = self.box_value(body, slow_block, &slow_value)?;
            body.set_terminator(
                slow_block,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![slow_value],
                    },
                },
            );
        }
        Ok(vec![(
            join,
            LowerValue::Wasm {
                value: joined,
                kind: ValueKind::Reference,
            },
        )])
    }

    /// Convert one call argument to the fast core's `f64` ABI. A missing or
    /// `undefined` argument becomes `NaN`, matching `read_arg_number`.
    fn unbox_or_nan(
        &self,
        body: &mut FunctionBody,
        block: Block,
        arg: &LowerValue,
    ) -> Result<(Block, Value), ConvertError> {
        let (value, kind) = arg.wasm()?;
        Ok(match kind {
            ValueKind::Number => (block, value),
            ValueKind::Boolean | ValueKind::Integer => (
                block,
                body.add_op(block, Operator::F64ConvertI32S, &[value], &[Type::F64]),
            ),
            ValueKind::Reference => {
                let is_null = body.add_op(block, Operator::RefIsNull, &[value], &[Type::I32]);
                let convert = body.add_block();
                let nan = body.add_block();
                let join = body.add_block();
                let result = body.add_blockparam(join, Type::F64);
                body.set_terminator(
                    block,
                    Terminator::CondBr {
                        cond: is_null,
                        if_true: BlockTarget {
                            block: nan,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: convert,
                            args: vec![],
                        },
                    },
                );
                let nan_value = body.add_op(
                    nan,
                    Operator::F64Const {
                        value: f64::NAN.to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                body.set_terminator(
                    nan,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![nan_value],
                        },
                    },
                );
                let number = self.as_f64(
                    body,
                    convert,
                    &LowerValue::Wasm {
                        value,
                        kind: ValueKind::Reference,
                    },
                )?;
                body.set_terminator(
                    convert,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![number],
                        },
                    },
                );
                (join, result)
            }
        })
    }

    /// Pad core arguments with NaN constants out to the core's arity.
    fn pad_core_args(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        core_args: &[Value],
        core: Func,
    ) -> Result<Vec<Value>, ConvertError> {
        let sig = self.module.funcs[core].sig();
        let arity = match &self.module.signatures[sig] {
            SignatureData::Func { params, .. } => params.len(),
            _ => return Err(ConvertError::invalid("fast core signature is not a func")),
        };
        let mut args = core_args.to_vec();
        while args.len() < arity {
            let nan = body.add_op(
                block,
                Operator::F64Const {
                    value: f64::NAN.to_bits(),
                },
                &[],
                &[Type::F64],
            );
            args.push(nan);
        }
        Ok(args)
    }
}

fn unary_core(body: &mut FunctionBody, block: Block, x: &Value, op: Operator) {
    let result = body.add_op(block, op, &[*x], &[Type::F64]);
    body.set_terminator(block, Terminator::Return { values: vec![result] });
}

fn binary_core(body: &mut FunctionBody, block: Block, x: &Value, y: &Value, op: Operator) {
    let result = body.add_op(block, op, &[*x, *y], &[Type::F64]);
    body.set_terminator(block, Terminator::Return { values: vec![result] });
}

fn sign_core(body: &mut FunctionBody, block: Block, args: &[Value]) {
    let x = args[0];
    let zero = body.add_op(block, Operator::F64Const { value: 0 }, &[], &[Type::F64]);
    let is_positive = body.add_op(block, Operator::F64Gt, &[x, zero], &[Type::I32]);
    let is_negative = body.add_op(block, Operator::F64Lt, &[x, zero], &[Type::I32]);
    let one = body.add_op(
        block,
        Operator::F64Const {
            value: 1.0f64.to_bits(),
        },
        &[],
        &[Type::F64],
    );
    let minus_one = body.add_op(
        block,
        Operator::F64Const {
            value: (-1.0f64).to_bits(),
        },
        &[],
        &[Type::F64],
    );
    let negative_or_x = body.add_op(
        block,
        Operator::TypedSelect { ty: Type::F64 },
        &[minus_one, x, is_negative],
        &[Type::F64],
    );
    let result = body.add_op(
        block,
        Operator::TypedSelect { ty: Type::F64 },
        &[one, negative_or_x, is_positive],
        &[Type::F64],
    );
    body.set_terminator(block, Terminator::Return { values: vec![result] });
}

fn round_core(body: &mut FunctionBody, block: Block, args: &[Value]) {
    let x = args[0];
    let half = body.add_op(
        block,
        Operator::F64Const {
            value: 0.5f64.to_bits(),
        },
        &[],
        &[Type::F64],
    );
    let shifted = body.add_op(block, Operator::F64Add, &[x, half], &[Type::F64]);
    let result = body.add_op(block, Operator::F64Floor, &[shifted], &[Type::F64]);
    body.set_terminator(block, Terminator::Return { values: vec![result] });
}

fn fround_core(body: &mut FunctionBody, block: Block, args: &[Value]) {
    let x = args[0];
    let narrowed = body.add_op(block, Operator::F32DemoteF64, &[x], &[Type::F32]);
    let result = body.add_op(block, Operator::F64PromoteF32, &[narrowed], &[Type::F64]);
    body.set_terminator(block, Terminator::Return { values: vec![result] });
}

fn imul_core(body: &mut FunctionBody, block: Block, args: &[Value]) {
    let x = args[0];
    let y = args[1];
    let x = body.add_op(block, Operator::I32TruncSatF64S, &[x], &[Type::I32]);
    let y = body.add_op(block, Operator::I32TruncSatF64S, &[y], &[Type::I32]);
    let product = body.add_op(block, Operator::I32Mul, &[x, y], &[Type::I32]);
    let result = body.add_op(block, Operator::F64ConvertI32S, &[product], &[Type::F64]);
    body.set_terminator(block, Terminator::Return { values: vec![result] });
}
impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// The provable direct-call path (plan Part 2, item 2). Called from
    /// `lower_call_parts` before the ordinary lookup. Fires when:
    ///   1. the callee receiver is a `LowerValue::ReferenceKey` produced by
    ///      `LoadId` of a primordial namespace identifier (`Math`, ...),
    ///   2. the member key is a static string with a registered fast core,
    ///   3. the shadowing scan proves the namespace identifier is never
    ///      assigned anywhere in the function (or its nested closures).
    ///
    /// Under those conditions the context property is still exactly the
    /// primordial value `new_context_with_primordials` installed, so the
    /// lookup, the namespace materialization the reader would keep alive,
    /// the arguments array, and the `CallRef` dispatch all vanish. Each
    /// argument is unboxed at its position and the typed fast core is
    /// called directly.
    ///
    /// Returns `None` when any condition fails, leaving the ordinary path
    /// completely in charge (including its block state — `block` is not
    /// touched unless `Some` is returned).
    #[allow(clippy::too_many_arguments)]
    fn try_provable_primordial_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        receiver: &LowerValue,
        key: &LowerValue,
        values: &BTreeMap<SValueId, LowerValue>,
        args: &[portal_jsc_swc_tac::SpreadOr<SValueId>],
    ) -> Result<Option<(Block, LowerValue)>, ConvertError> {
        // 1. Receiver must be the unshadowed identifier read.
        let LowerValue::ReferenceKey { key: namespace, .. } = receiver else {
            return Ok(None);
        };
        let provable = self.primordial_is_provable(namespace);
        // 2. Static member key with a fast core.
        let Ok(member) = self.key_of(key) else {
            return Ok(None);
        };
        let Some(tag) = static_primordial_tag(namespace, &member) else {
            return Ok(None);
        };
        // Spread arguments have no position-wise unboxing; leave them generic.
        if args.iter().any(|arg| arg.is_spread) {
            return Ok(None);
        }
        let mut core_args = Vec::with_capacity(args.len());
        for arg in args {
            let value = values
                .get(&arg.value)
                .ok_or_else(|| ConvertError::invalid("undefined call argument"))?;
            core_args.push(value.clone());
        }
        if !provable {
            // Guarded path (plan Part 2, item 3): shadowing could not be
            // ruled out statically, so verify the callee's primordial tag at
            // runtime and fall back to generic dispatch on mismatch. One
            // predictable branch replaces the boxing + `CallRef` cost
            // whenever the callee really is the primordial.
            let callees = self.get_property_value(body, block, receiver, &member)?;
            let mut continuations = Vec::with_capacity(callees.len());
            for (callee_block, callee) in callees {
                let fallback = |this: &mut Self,
                                body: &mut FunctionBody,
                                slow: Block|
                 -> Result<Vec<(Block, LowerValue)>, ConvertError> {
                    let (ctx, this_v, code, _arrow) =
                        this.callable_parts(body, slow, &callee, receiver.clone())?;
                    let array = this.make_arguments(body, slow, values, args)?;
                    let result = body.add_op(
                        slow,
                        Operator::CallRef {
                            sig_index: this.repr.adapter,
                        },
                        &[ctx, this_v, array, code],
                        &[this.repr.value],
                    );
                    Ok(vec![(
                        slow,
                        LowerValue::Wasm {
                            value: result,
                            kind: ValueKind::Reference,
                        },
                    )])
                };
                let continuation =
                    self.guarded_primordial_call(body, callee_block, &callee, tag, &core_args, fallback)?;
                continuations.extend(continuation);
            }
            return Ok(Some((continuations[0].0, continuations[0].1.clone())));
        }
        // 3. Direct dispatch: unbox each argument, call the core, box the
        // result. The receiver of an unshadowed primordial method is the
        // namespace object itself; `this` is unused by every fast core, so
        // the undefined receiver stays undefined exactly as the generic
        // adapters see it.
        //
        // `Array.isArray` has a dedicated boolean `(anyref) -> i32` core; the
        // numeric surface shares the uniform `(f64...) -> f64` ABI.
        if tag == TAG_ARRAY_IS_ARRAY {
            let core = self.ensure_is_array_fast_core()?;
            let raw = match core_args.first() {
                Some(arg) => self.box_value(body, block, arg)?,
                None => body.add_op(
                    block,
                    Operator::RefNull {
                        ty: self.repr.value,
                    },
                    &[],
                    &[self.repr.value],
                ),
            };
            let result = body.add_op(
                block,
                Operator::Call {
                    function_index: core,
                },
                &[raw],
                &[Type::I32],
            );
            let boxed = self.box_value(
                body,
                block,
                &LowerValue::Wasm {
                    value: result,
                    kind: ValueKind::Boolean,
                },
            )?;
            return Ok(Some((
                block,
                LowerValue::Wasm {
                    value: boxed,
                    kind: ValueKind::Reference,
                },
            )));
        }
        let mut current = block;
        let mut numeric_args = Vec::with_capacity(core_args.len());
        for arg in &core_args {
            let (next, converted) = self.unbox_or_nan(body, current, arg)?;
            numeric_args.push(converted);
            current = next;
        }
        let block = current;
        let core = self.ensure_primordial_fast_core(tag)?;
        let call_args = self.pad_core_args(body, block, &numeric_args, core)?;
        let result = body.add_op(
            block,
            Operator::Call {
                function_index: core,
            },
            &call_args,
            &[Type::F64],
        );
        let boxed = self.box_value(
            body,
            block,
            &LowerValue::Wasm {
                value: result,
                kind: ValueKind::Number,
            },
        )?;
        Ok(Some((
            block,
            LowerValue::Wasm {
                value: boxed,
                kind: ValueKind::Reference,
            },
        )))
    }
}


impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    /// The provable direct-construction path for typed arrays (plan Part 2,
    /// item 4). Fires for `new Int8Array(...)`-style call sites when the
    /// constructor identifier is provably unshadowed. Skips the pre-allocated
    /// receiver object, the arrow-reject branch, the constructor-returned
    /// object override check, the arguments array, and the `CallRef`: the
    /// per-kind fast core builds and returns the typed array directly.
    ///
    /// `context` must still be consulted for the fallback path only; on the
    /// fast path none of the generic `new` machinery is materialized.
    fn try_provable_typed_constructor(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        callee: &LowerValue,
        values: &BTreeMap<SValueId, LowerValue>,
        args: &[SValueId],
    ) -> Result<Option<(Block, LowerValue)>, ConvertError> {
        let LowerValue::ReferenceKey { key: name, .. } = callee else {
            return Ok(None);
        };
        if !self.primordial_is_provable(name) {
            return Ok(None);
        }
        let Some(kind) = TypedArrayKind::from_name(name) else {
            return Ok(None);
        };
        if !args.is_empty() && args.len() != 1 {
            // Multi-argument forms (typed-array-of-iterable with mapping
            // functions) stay on the generic path.
            return Ok(None);
        }
        let source = match args.first() {
            Some(arg) => values
                .get(arg)
                .cloned()
                .ok_or_else(|| ConvertError::invalid("undefined constructor argument"))?,
            None => self.undef(body, block),
        };
        let boxed_source = self.box_value(body, block, &source)?;
        let core = self.typed_array_constructor_core(kind)?;
        let result = body.add_op(
            block,
            Operator::Call {
                function_index: core,
            },
            &[boxed_source],
            &[self.repr.value],
        );
        Ok(Some((
            block,
            LowerValue::Wasm {
                value: result,
                kind: ValueKind::Reference,
            },
        )))
    }
}

/// Namespace-level tag lookup used by the provable path: is this identifier
/// a primordial global at all (`Math`, `Array`, ...), and if so, which
/// member tags does it own? The member map lets a call site combine a
/// provably-unshadowed namespace with a statically-known member name.
pub(crate) fn static_primordial_tag_namespace(name: &str) -> Option<()> {
    match name {
        "Math" | "Array" | "Reflect" | "Object" => Some(()),
        "Int8Array" | "Uint8Array" | "Uint8ClampedArray" | "Int16Array" | "Uint16Array"
        | "Int32Array" | "Uint32Array" | "Float32Array" | "Float64Array" => Some(()),
        _ => None,
    }
}
