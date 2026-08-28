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

    let (context_value, _) = context.wasm()?;
    Ok((block, context_value))
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
    let value = body.add_op(
        block,
        Operator::StructNew {
            sig: self.repr.function,
        },
        &[trie, elements, extra, code, context, captured_this, arrow],
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
fn read_arg_number(
    &self,
    body: &mut FunctionBody,
    block: Block,
    args: Value,
    index: u32,
) -> Result<(Block, Value), ConvertError> {
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
        let value = self.native_function_value(body, *block, context, func)?;
        *block = self.set_static_property_value_raw(body, *block, &math, name, &value)?;
    }

    const BINARY: &[(&str, Operator)] = &[("min", Operator::F64Min), ("max", Operator::F64Max)];
    for (name, op) in BINARY {
        let func = self.math_binary_method(name, *op)?;
        let (context, _) = math.wasm()?;
        let value = self.native_function_value(body, *block, context, func)?;
        *block = self.set_static_property_value_raw(body, *block, &math, name, &value)?;
    }

    for (name, func) in [
        ("sign", self.math_sign_method()?),
        ("round", self.math_round_method()?),
        ("fround", self.math_fround_method()?),
        ("imul", self.math_imul_method()?),
    ] {
        let (context, _) = math.wasm()?;
        let value = self.native_function_value(body, *block, context, func)?;
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
            let len = body.add_op(entry, Operator::ArrayLen, &[args], &[Type::I32]);
            let zero_idx = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let has_arg = body.add_op(entry, Operator::I32GtU, &[len, zero_idx], &[Type::I32]);
            let read = body.add_block();
            let no_arg = body.add_block();
            let join = body.add_block();
            let raw = body.add_blockparam(join, this.repr.value);
            body.set_terminator(
                entry,
                Terminator::CondBr {
                    cond: has_arg,
                    if_true: BlockTarget {
                        block: read,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: no_arg,
                        args: vec![],
                    },
                },
            );
            let value = body.add_op(
                read,
                Operator::ArrayGet {
                    sig: this.repr.arguments,
                },
                &[args, zero_idx],
                &[this.repr.value],
            );
            body.set_terminator(
                read,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![value],
                    },
                },
            );
            let undef = body.add_op(
                no_arg,
                Operator::RefNull {
                    ty: this.repr.value,
                },
                &[],
                &[this.repr.value],
            );
            body.set_terminator(
                no_arg,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![undef],
                    },
                },
            );

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

}
