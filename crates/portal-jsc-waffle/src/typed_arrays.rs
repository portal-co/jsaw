// Number-backed typed arrays.
//
// This implementation deliberately uses the common object header rather than
// introducing nine user-visible object layouts.  Callers cross the same small
// property seam as ordinary arrays; only this module knows how the tagged
// `typed_data` reference maps to packed WasmGC storage.


/// One DataView accessor operation. Determines the byte width, the result
/// kind (unsigned/signed int, float, or BigInt), and whether it reads or
/// writes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum DataViewOp {
    GetUint8,
    GetInt8,
    GetUint16,
    GetInt16,
    GetUint32,
    GetInt32,
    GetFloat64,
    GetBigUint64,
    SetUint8,
    SetUint16,
    SetUint32,
    SetFloat64,
    SetBigUint64,
}

/// Is `key` a DataView accessor method name? Used to widen the member-read
/// dispatch so these reach the DataView path.
pub(crate) fn is_dataview_method(key: &str) -> bool {
    matches!(
        key,
        "getUint8"
            | "getInt8"
            | "getUint16"
            | "getInt16"
            | "getUint32"
            | "getInt32"
            | "getFloat64"
            | "getBigUint64"
            | "setUint8"
            | "setInt8"
            | "setUint16"
            | "setInt16"
            | "setUint32"
            | "setInt32"
            | "setFloat64"
            | "setBigUint64"
    )
}

impl DataViewOp {
    fn bytes(self) -> u32 {
        match self {
            DataViewOp::GetUint8 | DataViewOp::GetInt8 | DataViewOp::SetUint8 => 1,
            DataViewOp::GetUint16 | DataViewOp::GetInt16 | DataViewOp::SetUint16 => 2,
            DataViewOp::GetUint32 | DataViewOp::GetInt32 | DataViewOp::SetUint32 => 4,
            DataViewOp::GetFloat64
            | DataViewOp::GetBigUint64
            | DataViewOp::SetFloat64
            | DataViewOp::SetBigUint64 => 8,
        }
    }

    fn is_write(self) -> bool {
        matches!(
            self,
            DataViewOp::SetUint8
                | DataViewOp::SetUint16
                | DataViewOp::SetUint32
                | DataViewOp::SetFloat64
                | DataViewOp::SetBigUint64
        )
    }
}

impl<'a, 'module, 'wasm> Converter<'a, 'module, 'wasm> {
    fn typed_array_sig(&self, kind: TypedArrayKind) -> portal_pc_waffle::Signature {
        match kind {
            TypedArrayKind::Int8 | TypedArrayKind::Uint8 | TypedArrayKind::Uint8Clamped => {
                self.repr.typed_i8
            }
            TypedArrayKind::Int16 | TypedArrayKind::Uint16 => self.repr.typed_i16,
            TypedArrayKind::Int32 | TypedArrayKind::Uint32 => self.repr.typed_i32,
            TypedArrayKind::Float32 => self.repr.typed_f32,
            TypedArrayKind::Float64 => self.repr.typed_f64,
        }
    }

    fn typed_array_ty(&self, kind: TypedArrayKind) -> Type {
        ref_sig(self.typed_array_sig(kind))
    }

    /// Build one typed-array object around already-allocated native storage.
    /// `data` must be a concrete WasmGC array for `kind`; it is immediately
    /// widened to `anyref` so every later operation can share the common header.
    fn new_typed_array_object(
        &self,
        body: &mut FunctionBody,
        block: Block,
        kind: TypedArrayKind,
        data: Value,
        offset: Value,
        length: Value,
    ) -> Result<LowerValue, ConvertError> {
        let trie = self.new_trie(body, block)?;
        let root = self.anyref(body, block, trie);
        let elements = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.arguments_ty(),
            },
            &[],
            &[self.repr.arguments_ty()],
        );
        let properties = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        );
        let data = self.anyref(body, block, data);
        let tag = body.add_op(
            block,
            Operator::I32Const {
                value: kind.code() as u32,
            },
            &[],
            &[Type::I32],
        );
        let object = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.object,
            },
            &[root, elements, properties, data, tag, offset, length],
            &[self.repr.object_ty()],
        );
        Ok(LowerValue::Wasm {
            value: object,
            kind: ValueKind::Reference,
        })
    }

    fn typed_new_backing(
        &self,
        body: &mut FunctionBody,
        block: Block,
        kind: TypedArrayKind,
        length: Value,
    ) -> Value {
        let sig = self.typed_array_sig(kind);
        body.add_op(
            block,
            Operator::ArrayNewDefault { sig },
            &[length],
            &[self.typed_array_ty(kind)],
        )
    }

    /// Read the tagged typed-array fields. The caller has already established
    /// that the object is a typed array, avoiding duplicate shape checks in every
    /// storage operation.
    fn typed_array_parts(
        &self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
    ) -> (Value, Value, Value, Value) {
        let object = body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[object],
            &[self.repr.object_ty()],
        );
        let data = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 3,
            },
            &[object],
            &[self.repr.value],
        );
        let kind = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 4,
            },
            &[object],
            &[Type::I32],
        );
        let offset = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 5,
            },
            &[object],
            &[Type::I32],
        );
        let length = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 6,
            },
            &[object],
            &[Type::I32],
        );
        (data, kind, offset, length)
    }

    fn typed_object_test(
        &self,
        body: &mut FunctionBody,
        block: Block,
        value: Value,
    ) -> (Block, Value) {
        let object = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.object_non_null_ty(),
            },
            &[value],
            &[Type::I32],
        );
        // A typed array is an object with non-null typed storage. `RefTest` on
        // the common object type cannot alone distinguish ordinary arrays.
        let object_block = body.add_block();
        let absent = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, Type::I32);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: object,
                if_true: BlockTarget {
                    block: object_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: absent,
                    args: vec![],
                },
            },
        );
        let plain = body.add_op(
            object_block,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[value],
            &[self.repr.object_ty()],
        );
        let data = body.add_op(
            object_block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 3,
            },
            &[plain],
            &[self.repr.value],
        );
        let null = body.add_op(object_block, Operator::RefIsNull, &[data], &[Type::I32]);
        let present = body.add_op(object_block, Operator::I32Eqz, &[null], &[Type::I32]);
        body.set_terminator(
            object_block,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![present],
                },
            },
        );
        let zero = body.add_op(absent, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            absent,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![zero],
                },
            },
        );
        (join, result)
    }

    /// Dispatch a runtime tag to its concrete WasmGC array type and merge an
    /// `anyref` result. The callback owns the typed operation; all callers share
    /// the same test/cast ladder, which keeps the storage representation local.
    fn typed_kind_value(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        tag: Value,
        data: Value,
        mut build: impl FnMut(
            &mut Self,
            &mut FunctionBody,
            Block,
            TypedArrayKind,
            Value,
        ) -> Result<(Block, Value), ConvertError>,
    ) -> Result<(Block, Value), ConvertError> {
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        let mut current = block;
        for kind in TypedArrayKind::ALL {
            let code = body.add_op(
                current,
                Operator::I32Const {
                    value: kind.code() as u32,
                },
                &[],
                &[Type::I32],
            );
            let matches = body.add_op(current, Operator::I32Eq, &[tag, code], &[Type::I32]);
            let matched = body.add_block();
            let next = body.add_block();
            body.set_terminator(
                current,
                Terminator::CondBr {
                    cond: matches,
                    if_true: BlockTarget {
                        block: matched,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: next,
                        args: vec![],
                    },
                },
            );
            let typed_data = body.add_op(
                matched,
                Operator::RefCast {
                    ty: self.typed_array_ty(kind),
                },
                &[data],
                &[self.typed_array_ty(kind)],
            );
            let (end, value) = build(self, body, matched, kind, typed_data)?;
            body.set_terminator(
                end,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![value],
                    },
                },
            );
            current = next;
        }
        body.set_terminator(current, Terminator::Unreachable);
        Ok((join, result))
    }

    fn typed_kind_effect(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        tag: Value,
        data: Value,
        mut build: impl FnMut(
            &mut Self,
            &mut FunctionBody,
            Block,
            TypedArrayKind,
            Value,
        ) -> Result<Block, ConvertError>,
    ) -> Result<Block, ConvertError> {
        let join = body.add_block();
        let mut current = block;
        for kind in TypedArrayKind::ALL {
            let code = body.add_op(
                current,
                Operator::I32Const {
                    value: kind.code() as u32,
                },
                &[],
                &[Type::I32],
            );
            let matches = body.add_op(current, Operator::I32Eq, &[tag, code], &[Type::I32]);
            let matched = body.add_block();
            let next = body.add_block();
            body.set_terminator(
                current,
                Terminator::CondBr {
                    cond: matches,
                    if_true: BlockTarget {
                        block: matched,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: next,
                        args: vec![],
                    },
                },
            );
            let typed_data = body.add_op(
                matched,
                Operator::RefCast {
                    ty: self.typed_array_ty(kind),
                },
                &[data],
                &[self.typed_array_ty(kind)],
            );
            let end = build(self, body, matched, kind, typed_data)?;
            body.set_terminator(
                end,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![],
                    },
                },
            );
            current = next;
        }
        body.set_terminator(current, Terminator::Unreachable);
        Ok(join)
    }

    fn typed_read_kind(
        &self,
        body: &mut FunctionBody,
        block: Block,
        kind: TypedArrayKind,
        data: Value,
        index: Value,
    ) -> Result<Value, ConvertError> {
        let sig = self.typed_array_sig(kind);
        let value = match kind {
            TypedArrayKind::Int8 => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGetS { sig },
                    &[data, index],
                    &[Type::I32],
                );
                body.add_op(block, Operator::F64ConvertI32S, &[value], &[Type::F64])
            }
            TypedArrayKind::Uint8 | TypedArrayKind::Uint8Clamped => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGetU { sig },
                    &[data, index],
                    &[Type::I32],
                );
                body.add_op(block, Operator::F64ConvertI32U, &[value], &[Type::F64])
            }
            TypedArrayKind::Int16 => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGetS { sig },
                    &[data, index],
                    &[Type::I32],
                );
                body.add_op(block, Operator::F64ConvertI32S, &[value], &[Type::F64])
            }
            TypedArrayKind::Uint16 => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGetU { sig },
                    &[data, index],
                    &[Type::I32],
                );
                body.add_op(block, Operator::F64ConvertI32U, &[value], &[Type::F64])
            }
            TypedArrayKind::Int32 => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGet { sig },
                    &[data, index],
                    &[Type::I32],
                );
                body.add_op(block, Operator::F64ConvertI32S, &[value], &[Type::F64])
            }
            TypedArrayKind::Uint32 => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGet { sig },
                    &[data, index],
                    &[Type::I32],
                );
                body.add_op(block, Operator::F64ConvertI32U, &[value], &[Type::F64])
            }
            TypedArrayKind::Float32 => {
                let value = body.add_op(
                    block,
                    Operator::ArrayGet { sig },
                    &[data, index],
                    &[Type::F32],
                );
                body.add_op(block, Operator::F64PromoteF32, &[value], &[Type::F64])
            }
            TypedArrayKind::Float64 => body.add_op(
                block,
                Operator::ArrayGet { sig },
                &[data, index],
                &[Type::F64],
            ),
        };
        self.box_value(
            body,
            block,
            &LowerValue::Wasm {
                value,
                kind: ValueKind::Number,
            },
        )
    }

    /// JavaScript integer typed arrays retain the low 32 bits of `ToInteger`.
    /// Do the modulo in f64 before converting: Wasm's saturating truncation
    /// would otherwise make values outside the signed/unsigned i32 range clamp
    /// instead of wrap (for example `Uint8Array([4294967551])` must contain
    /// 255).
    fn typed_integer_bits(&self, body: &mut FunctionBody, block: Block, number: Value) -> Value {
        let modulus = body.add_op(
            block,
            Operator::F64Const {
                value: 4_294_967_296.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let zero = body.add_op(
            block,
            Operator::F64Const {
                value: 0.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let integer = body.add_op(block, Operator::F64Trunc, &[number], &[Type::F64]);
        let quotient = body.add_op(block, Operator::F64Div, &[integer, modulus], &[Type::F64]);
        let quotient = body.add_op(block, Operator::F64Trunc, &[quotient], &[Type::F64]);
        let product = body.add_op(block, Operator::F64Mul, &[quotient, modulus], &[Type::F64]);
        let remainder = body.add_op(block, Operator::F64Sub, &[integer, product], &[Type::F64]);
        let negative = body.add_op(block, Operator::F64Lt, &[remainder, zero], &[Type::I32]);
        let wrapped = body.add_op(block, Operator::F64Add, &[remainder, modulus], &[Type::F64]);
        let normalized = body.add_op(
            block,
            Operator::TypedSelect { ty: Type::F64 },
            &[wrapped, remainder, negative],
            &[Type::F64],
        );
        body.add_op(
            block,
            Operator::I32TruncSatF64U,
            &[normalized],
            &[Type::I32],
        )
    }

    fn typed_clamped_u8(&self, body: &mut FunctionBody, block: Block, number: Value) -> Value {
        let zero = body.add_op(
            block,
            Operator::F64Const {
                value: 0.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let max = body.add_op(
            block,
            Operator::F64Const {
                value: 255.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let half = body.add_op(
            block,
            Operator::F64Const {
                value: 0.5f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let bounded = body.add_op(block, Operator::F64Min, &[number, max], &[Type::F64]);
        let bounded = body.add_op(block, Operator::F64Max, &[bounded, zero], &[Type::F64]);
        let floor = body.add_op(block, Operator::F64Floor, &[bounded], &[Type::F64]);
        let fraction = body.add_op(block, Operator::F64Sub, &[bounded, floor], &[Type::F64]);
        let floor_i32 = body.add_op(block, Operator::I32TruncSatF64U, &[floor], &[Type::I32]);
        let one = body.add_op(block, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let rounded_up = body.add_op(block, Operator::I32Add, &[floor_i32, one], &[Type::I32]);
        let gt_half = body.add_op(block, Operator::F64Gt, &[fraction, half], &[Type::I32]);
        let eq_half = body.add_op(block, Operator::F64Eq, &[fraction, half], &[Type::I32]);
        let odd = body.add_op(block, Operator::I32And, &[floor_i32, one], &[Type::I32]);
        let zero_i32 = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let odd = body.add_op(block, Operator::I32Ne, &[odd, zero_i32], &[Type::I32]);
        let tie_up = body.add_op(block, Operator::I32And, &[eq_half, odd], &[Type::I32]);
        let up = body.add_op(block, Operator::I32Or, &[gt_half, tie_up], &[Type::I32]);
        body.add_op(
            block,
            Operator::TypedSelect { ty: Type::I32 },
            &[rounded_up, floor_i32, up],
            &[Type::I32],
        )
    }

    fn typed_write_kind(
        &self,
        body: &mut FunctionBody,
        block: Block,
        kind: TypedArrayKind,
        data: Value,
        index: Value,
        number: Value,
    ) -> Result<Block, ConvertError> {
        let sig = self.typed_array_sig(kind);
        match kind {
            TypedArrayKind::Float32 => {
                let value = body.add_op(block, Operator::F32DemoteF64, &[number], &[Type::F32]);
                body.add_op(
                    block,
                    Operator::ArraySet { sig },
                    &[data, index, value],
                    &[],
                );
            }
            TypedArrayKind::Float64 => {
                body.add_op(
                    block,
                    Operator::ArraySet { sig },
                    &[data, index, number],
                    &[],
                );
            }
            TypedArrayKind::Uint8Clamped => {
                let value = self.typed_clamped_u8(body, block, number);
                body.add_op(
                    block,
                    Operator::ArraySet { sig },
                    &[data, index, value],
                    &[],
                );
            }
            TypedArrayKind::Uint32 => {
                let value = self.typed_integer_bits(body, block, number);
                body.add_op(
                    block,
                    Operator::ArraySet { sig },
                    &[data, index, value],
                    &[],
                );
            }
            _ => {
                let value = self.typed_integer_bits(body, block, number);
                body.add_op(
                    block,
                    Operator::ArraySet { sig },
                    &[data, index, value],
                    &[],
                );
            }
        }
        Ok(block)
    }

    /// A bounds-checked element read. Missing elements use the backend's shared
    /// null/undefined representation, never exposing a Wasm array out-of-bounds
    /// trap to source JavaScript.
    fn typed_array_read(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
        index: Value,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let (data, tag, offset, length) = self.typed_array_parts(body, block, object);
        let in_bounds = body.add_op(block, Operator::I32LtU, &[index, length], &[Type::I32]);
        let present = body.add_block();
        let missing = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: in_bounds,
                if_true: BlockTarget {
                    block: present,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: missing,
                    args: vec![],
                },
            },
        );
        let physical = body.add_op(present, Operator::I32Add, &[offset, index], &[Type::I32]);
        let (present, value) =
            self.typed_kind_value(body, present, tag, data, |this, body, block, kind, data| {
                let value = this.typed_read_kind(body, block, kind, data, physical)?;
                Ok((block, value))
            })?;
        body.set_terminator(
            present,
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
        Ok((
            join,
            LowerValue::Wasm {
                value: result,
                kind: ValueKind::Reference,
            },
        ))
    }

    fn typed_array_length_value(
        &self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
    ) -> Result<LowerValue, ConvertError> {
        let (_, _, _, length) = self.typed_array_parts(body, block, object);
        let length = body.add_op(block, Operator::F64ConvertI32U, &[length], &[Type::F64]);
        Ok(LowerValue::Wasm {
            value: length,
            kind: ValueKind::Number,
        })
    }

    fn typed_array_byte_length_value(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let (data, tag, _offset, length) = self.typed_array_parts(body, block, object);
        let (block, bytes) =
            self.typed_kind_value(body, block, tag, data, |this, body, block, kind, _| {
                let size = body.add_op(
                    block,
                    Operator::I32Const {
                        value: kind.bytes_per_element() as u32,
                    },
                    &[],
                    &[Type::I32],
                );
                let bytes = body.add_op(block, Operator::I32Mul, &[length, size], &[Type::I32]);
                let bytes = body.add_op(block, Operator::F64ConvertI32U, &[bytes], &[Type::F64]);
                this.box_value(
                    body,
                    block,
                    &LowerValue::Wasm {
                        value: bytes,
                        kind: ValueKind::Number,
                    },
                )
                .map(|value| (block, value))
            })?;
        Ok((
            block,
            LowerValue::Wasm {
                value: bytes,
                kind: ValueKind::Reference,
            },
        ))
    }

    /// A bounds-checked write. Typed arrays are fixed length: unlike ordinary
    /// arrays, a missing index is intentionally a no-op and never allocates.
    fn typed_array_write(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
        index: Value,
        value: &LowerValue,
    ) -> Result<Block, ConvertError> {
        let number = self.as_f64(body, block, value)?;
        let (data, tag, offset, length) = self.typed_array_parts(body, block, object);
        let in_bounds = body.add_op(block, Operator::I32LtU, &[index, length], &[Type::I32]);
        let write = body.add_block();
        let done = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: in_bounds,
                if_true: BlockTarget {
                    block: write,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        let physical = body.add_op(write, Operator::I32Add, &[offset, index], &[Type::I32]);
        let write =
            self.typed_kind_effect(body, write, tag, data, |this, body, block, kind, data| {
                this.typed_write_kind(body, block, kind, data, physical, number)
            })?;
        body.set_terminator(
            write,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        Ok(done)
    }

    /// Allocate a typed array for a runtime constructor tag. The context carried
    /// by each constructor function supplies that tag, allowing all nine global
    /// constructor values to share one native adapter body rather than cloning a
    /// large source-copy implementation nine times.
    fn new_typed_array_from_tag(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        tag: Value,
        length: Value,
    ) -> Result<(Block, Value), ConvertError> {
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        let mut current = block;
        for kind in TypedArrayKind::ALL {
            let code = body.add_op(
                current,
                Operator::I32Const {
                    value: kind.code() as u32,
                },
                &[],
                &[Type::I32],
            );
            let matches = body.add_op(current, Operator::I32Eq, &[tag, code], &[Type::I32]);
            let matched = body.add_block();
            let next = body.add_block();
            body.set_terminator(
                current,
                Terminator::CondBr {
                    cond: matches,
                    if_true: BlockTarget {
                        block: matched,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: next,
                        args: vec![],
                    },
                },
            );
            let zero = body.add_op(matched, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
            let backing = self.typed_new_backing(body, matched, kind, length);
            let object = self.new_typed_array_object(body, matched, kind, backing, zero, length)?;
            let object = self.box_value(body, matched, &object)?;
            body.set_terminator(
                matched,
                Terminator::Br {
                    target: BlockTarget {
                        block: join,
                        args: vec![object],
                    },
                },
            );
            current = next;
        }
        body.set_terminator(current, Terminator::Unreachable);
        Ok((join, result))
    }

    fn typed_array_constructor(&mut self) -> Result<Func, ConvertError> {
        self.build_native_adapter(
            "typed_array_ctor",
            |this, body, entry, context, this_val, args| {
                // Calling a typed-array constructor without `new` is a TypeError in
                // JavaScript. The backend has no exception object, so retain its
                // established failure representation: an unreachable trap.
                let constructor_context = body.add_op(
                    entry,
                    Operator::RefCast {
                        ty: this.repr.object_ty(),
                    },
                    &[context],
                    &[this.repr.object_ty()],
                );
                let constructor_tag = body.add_op(
                    entry,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 4,
                    },
                    &[constructor_context],
                    &[Type::I32],
                );
                let missing_this =
                    body.add_op(entry, Operator::RefIsNull, &[this_val], &[Type::I32]);
                let invalid_call = body.add_block();
                let construct = body.add_block();
                body.set_terminator(
                    entry,
                    Terminator::CondBr {
                        cond: missing_this,
                        if_true: BlockTarget {
                            block: invalid_call,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: construct,
                            args: vec![],
                        },
                    },
                );
                body.set_terminator(invalid_call, Terminator::Unreachable);

                let (construct, source) = this.read_arg_raw(body, construct, args, 0);
                let absent = body.add_op(construct, Operator::RefIsNull, &[source], &[Type::I32]);
                let empty = body.add_block();
                let present = body.add_block();
                let join = body.add_block();
                let result = body.add_blockparam(join, this.repr.value);
                body.set_terminator(
                    construct,
                    Terminator::CondBr {
                        cond: absent,
                        if_true: BlockTarget {
                            block: empty,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: present,
                            args: vec![],
                        },
                    },
                );
                let zero = body.add_op(empty, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
                let (empty, array) =
                    this.new_typed_array_from_tag(body, empty, constructor_tag, zero)?;
                body.set_terminator(
                    empty,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![array],
                        },
                    },
                );

                let is_number = body.add_op(
                    present,
                    Operator::RefTest {
                        ty: this.repr.number_ty(),
                    },
                    &[source],
                    &[Type::I32],
                );
                let numeric = body.add_block();
                let source_object = body.add_block();
                body.set_terminator(
                    present,
                    Terminator::CondBr {
                        cond: is_number,
                        if_true: BlockTarget {
                            block: numeric,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: source_object,
                            args: vec![],
                        },
                    },
                );
                let length = this.as_f64(
                    body,
                    numeric,
                    &LowerValue::Wasm {
                        value: source,
                        kind: ValueKind::Reference,
                    },
                )?;
                // `ToIndex` accepts NaN as zero but rejects negative and infinite /
                // out-of-range lengths. Saturating conversion alone would silently
                // turn a negative length into zero, so guard its finite index range
                // before allocating native storage.
                let zero_number = body.add_op(
                    numeric,
                    Operator::F64Const {
                        value: 0.0f64.to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                let max_length = body.add_op(
                    numeric,
                    Operator::F64Const {
                        value: 4_294_967_296.0f64.to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                let negative = body.add_op(
                    numeric,
                    Operator::F64Lt,
                    &[length, zero_number],
                    &[Type::I32],
                );
                let out_of_range = body.add_op(
                    numeric,
                    Operator::F64Ge,
                    &[length, max_length],
                    &[Type::I32],
                );
                let invalid_length = body.add_op(
                    numeric,
                    Operator::I32Or,
                    &[negative, out_of_range],
                    &[Type::I32],
                );
                let valid_length = body.add_block();
                let invalid_length_block = body.add_block();
                body.set_terminator(
                    numeric,
                    Terminator::CondBr {
                        cond: invalid_length,
                        if_true: BlockTarget {
                            block: invalid_length_block,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: valid_length,
                            args: vec![],
                        },
                    },
                );
                body.set_terminator(invalid_length_block, Terminator::Unreachable);
                let length = body.add_op(
                    valid_length,
                    Operator::I32TruncSatF64U,
                    &[length],
                    &[Type::I32],
                );
                let (valid_length, array) =
                    this.new_typed_array_from_tag(body, valid_length, constructor_tag, length)?;
                body.set_terminator(
                    valid_length,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![array],
                        },
                    },
                );

                // Constructors accept ordinary arrays and any supported typed-array
                // view. The two source forms share the destination write path, so
                // every element receives the target kind's conversion semantics.
                let is_object = body.add_op(
                    source_object,
                    Operator::RefTest {
                        ty: this.repr.object_ty(),
                    },
                    &[source],
                    &[Type::I32],
                );
                let object = body.add_block();
                let unsupported = body.add_block();
                body.set_terminator(
                    source_object,
                    Terminator::CondBr {
                        cond: is_object,
                        if_true: BlockTarget {
                            block: object,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: unsupported,
                            args: vec![],
                        },
                    },
                );
                let source_plain = body.add_op(
                    object,
                    Operator::RefCast {
                        ty: this.repr.object_ty(),
                    },
                    &[source],
                    &[this.repr.object_ty()],
                );
                let source_data = body.add_op(
                    object,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 3,
                    },
                    &[source_plain],
                    &[this.repr.value],
                );
                let no_typed_data =
                    body.add_op(object, Operator::RefIsNull, &[source_data], &[Type::I32]);
                let ordinary_source = body.add_block();
                let typed_source = body.add_block();
                body.set_terminator(
                    object,
                    Terminator::CondBr {
                        cond: no_typed_data,
                        if_true: BlockTarget {
                            block: ordinary_source,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: typed_source,
                            args: vec![],
                        },
                    },
                );

                let source_length = body.add_op(
                    typed_source,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 6,
                    },
                    &[source_plain],
                    &[Type::I32],
                );
                let (typed_source, target_value) = this.new_typed_array_from_tag(
                    body,
                    typed_source,
                    constructor_tag,
                    source_length,
                )?;
                let typed_source = this.for_each_index(
                    body,
                    typed_source,
                    source_length,
                    |this, body, block, i| {
                        let (block, element) = this.typed_array_read(body, block, source, i)?;
                        this.typed_array_write(body, block, target_value, i, &element)
                    },
                )?;
                body.set_terminator(
                    typed_source,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![target_value],
                        },
                    },
                );

                let elements = body.add_op(
                    ordinary_source,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 1,
                    },
                    &[source_plain],
                    &[this.repr.arguments_ty()],
                );
                let no_elements = body.add_op(
                    ordinary_source,
                    Operator::RefIsNull,
                    &[elements],
                    &[Type::I32],
                );
                let copy_ordinary = body.add_block();
                body.set_terminator(
                    ordinary_source,
                    Terminator::CondBr {
                        cond: no_elements,
                        if_true: BlockTarget {
                            block: unsupported,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: copy_ordinary,
                            args: vec![],
                        },
                    },
                );
                let source_length =
                    body.add_op(copy_ordinary, Operator::ArrayLen, &[elements], &[Type::I32]);
                let (copy_ordinary, target_value) = this.new_typed_array_from_tag(
                    body,
                    copy_ordinary,
                    constructor_tag,
                    source_length,
                )?;
                let copy_ordinary = this.for_each_index(
                    body,
                    copy_ordinary,
                    source_length,
                    |this, body, block, i| {
                        let element = body.add_op(
                            block,
                            Operator::ArrayGet {
                                sig: this.repr.arguments,
                            },
                            &[elements, i],
                            &[this.repr.value],
                        );
                        this.typed_array_write(
                            body,
                            block,
                            target_value,
                            i,
                            &LowerValue::Wasm {
                                value: element,
                                kind: ValueKind::Reference,
                            },
                        )
                    },
                )?;
                body.set_terminator(
                    copy_ordinary,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![target_value],
                        },
                    },
                );

                body.set_terminator(unsupported, Terminator::Unreachable);
                body.set_terminator(
                    join,
                    Terminator::Return {
                        values: vec![result],
                    },
                );
                Ok(())
            },
        )
    }

    /// Fast core for a statically-known typed-array constructor: skip the
    /// generic adapter entirely and build the array for `kind` directly from
    /// the (possibly missing) single source argument. Missing arguments are
    /// an empty array, matching the generic constructor's `undefined`
    /// handling. The result is the already-boxed typed-array value.
    fn typed_array_constructor_core(&mut self, kind: TypedArrayKind) -> Result<Func, ConvertError> {
        let tag_key = format!("typed_array_ctor_core_{}", kind.name());
        if let Some(func) = self.native_function_cache.get(&tag_key) {
            return Ok(*func);
        }
        let value = self.repr.value;
        let sig = self.module.signatures.push(SignatureData::Func {
            params: vec![value],
            returns: vec![value],
            shared: false,
        });
        let mut body = FunctionBody::new(self.module, sig);
        let entry = body.entry;
        // ABI: one boxed `source`. Null (missing / `undefined` argument)
        // builds an empty array; a number builds a zeroed array of that
        // length; an ordinary or typed array is copied element-wise — the
        // same three cases the generic constructor adapter implements.
        let source = body.blocks[entry].params[0].1;
        let absent = body.add_op(entry, Operator::RefIsNull, &[source], &[Type::I32]);
        let empty = body.add_block();
        let present = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        // Emitted in the entry block so every branch below dominates it.
        let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: absent,
                if_true: BlockTarget {
                    block: empty,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: present,
                    args: vec![],
                },
            },
        );
        let backing = self.typed_new_backing(&mut body, empty, kind, zero);
        let object = self.new_typed_array_object(&mut body, empty, kind, backing, zero, zero)?;
        let object = self.box_value(&mut body, empty, &object)?;
        body.set_terminator(
            empty,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![object],
                },
            },
        );

        // Present source: number -> length form; object -> copy form.
        let is_number = body.add_op(
            present,
            Operator::RefTest {
                ty: self.repr.number_non_null_ty(),
            },
            &[source],
            &[Type::I32],
        );
        let numeric = body.add_block();
        let source_object = body.add_block();
        body.set_terminator(
            present,
            Terminator::CondBr {
                cond: is_number,
                if_true: BlockTarget {
                    block: numeric,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: source_object,
                    args: vec![],
                },
            },
        );
        let length = self.as_f64(
            &mut body,
            numeric,
            &LowerValue::Wasm {
                value: source,
                kind: ValueKind::Reference,
            },
        )?;
        let zero_number = body.add_op(
            numeric,
            Operator::F64Const {
                value: 0.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let max_length = body.add_op(
            numeric,
            Operator::F64Const {
                value: 4_294_967_296.0f64.to_bits(),
            },
            &[],
            &[Type::F64],
        );
        let negative = body.add_op(numeric, Operator::F64Lt, &[length, zero_number], &[Type::I32]);
        let out_of_range = body.add_op(numeric, Operator::F64Ge, &[length, max_length], &[Type::I32]);
        let invalid_length = body.add_op(numeric, Operator::I32Or, &[negative, out_of_range], &[Type::I32]);
        let valid_length = body.add_block();
        let invalid_length_block = body.add_block();
        body.set_terminator(
            numeric,
            Terminator::CondBr {
                cond: invalid_length,
                if_true: BlockTarget {
                    block: invalid_length_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: valid_length,
                    args: vec![],
                },
            },
        );
        body.set_terminator(invalid_length_block, Terminator::Unreachable);
        let length = body.add_op(
            valid_length,
            Operator::I32TruncSatF64U,
            &[length],
            &[Type::I32],
        );
        let backing = self.typed_new_backing(&mut body, valid_length, kind, length);
        let object = self.new_typed_array_object(&mut body, valid_length, kind, backing, zero, length)?;
        let object = self.box_value(&mut body, valid_length, &object)?;
        body.set_terminator(
            valid_length,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![object],
                },
            },
        );

        let is_object = body.add_op(
            source_object,
            Operator::RefTest {
                ty: self.repr.object_ty(),
            },
            &[source],
            &[Type::I32],
        );
        let object = body.add_block();
        let unsupported = body.add_block();
        body.set_terminator(
            source_object,
            Terminator::CondBr {
                cond: is_object,
                if_true: BlockTarget {
                    block: object,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: unsupported,
                    args: vec![],
                },
            },
        );
        let source_plain = body.add_op(
            object,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[source],
            &[self.repr.object_ty()],
        );
        let source_data = body.add_op(
            object,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 3,
            },
            &[source_plain],
            &[self.repr.value],
        );
        let no_typed_data =
            body.add_op(object, Operator::RefIsNull, &[source_data], &[Type::I32]);
        let ordinary_source = body.add_block();
        let typed_source = body.add_block();
        body.set_terminator(
            object,
            Terminator::CondBr {
                cond: no_typed_data,
                if_true: BlockTarget {
                    block: ordinary_source,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: typed_source,
                    args: vec![],
                },
            },
        );

        let source_length = body.add_op(
            typed_source,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 6,
            },
            &[source_plain],
            &[Type::I32],
        );
        let backing = self.typed_new_backing(&mut body, typed_source, kind, source_length);
        let target_object =
            self.new_typed_array_object(&mut body, typed_source, kind, backing, zero, source_length)?;
        let target_value = self.box_value(&mut body, typed_source, &target_object)?;
        let copied = self.for_each_index(
            &mut body,
            typed_source,
            source_length,
            |this, body, block, i| {
                let (block, element) = this.typed_array_read(body, block, source, i)?;
                this.typed_array_write(body, block, target_value, i, &element)
            },
        )?;
        body.set_terminator(
            copied,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![target_value],
                },
            },
        );

        let elements = body.add_op(
            ordinary_source,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 1,
            },
            &[source_plain],
            &[self.repr.arguments_ty()],
        );
        let no_elements = body.add_op(
            ordinary_source,
            Operator::RefIsNull,
            &[elements],
            &[Type::I32],
        );
        let copy_ordinary = body.add_block();
        body.set_terminator(
            ordinary_source,
            Terminator::CondBr {
                cond: no_elements,
                if_true: BlockTarget {
                    block: unsupported,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: copy_ordinary,
                    args: vec![],
                },
            },
        );
        let source_length =
            body.add_op(copy_ordinary, Operator::ArrayLen, &[elements], &[Type::I32]);
        let backing = self.typed_new_backing(&mut body, copy_ordinary, kind, source_length);
        let target_object =
            self.new_typed_array_object(&mut body, copy_ordinary, kind, backing, zero, source_length)?;
        let target_value = self.box_value(&mut body, copy_ordinary, &target_object)?;
        let copied = self.for_each_index(
            &mut body,
            copy_ordinary,
            source_length,
            |this, body, block, i| {
                let element = body.add_op(
                    block,
                    Operator::ArrayGet {
                        sig: this.repr.arguments,
                    },
                    &[elements, i],
                    &[this.repr.value],
                );
                this.typed_array_write(
                    body,
                    block,
                    target_value,
                    i,
                    &LowerValue::Wasm {
                        value: element,
                        kind: ValueKind::Reference,
                    },
                )
            },
        )?;
        body.set_terminator(
            copied,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![target_value],
                },
            },
        );

        body.set_terminator(unsupported, Terminator::Unreachable);
        body.set_terminator(
            join,
            Terminator::Return {
                values: vec![result],
            },
        );

        let func = self.module.funcs.push(FuncDecl::Body(
            sig,
            format!("js_fast_{tag_key}"),
            body,
        ));
        self.native_function_cache.insert(tag_key, func);
        Ok(func)
    }

    fn typed_array_instance_method(&mut self, key: &str) -> Result<Func, ConvertError> {
        match key {
            "subarray" => self.typed_array_subarray_method(),
            "set" => self.typed_array_set_method(),
            _ => Err(ConvertError::invalid(format!(
                "unknown typed-array method {key:?}"
            ))),
        }
    }

    /// The DataView accessor method names this backend implements (the exact
    /// set blitz-js emits for the linear-memory intrinsics).
    const DATAVIEW_METHODS: [&'static str; 16] = [
        "getUint8",
        "getInt8",
        "getUint16",
        "getInt16",
        "getUint32",
        "getInt32",
        "getFloat64",
        "getBigUint64",
        "setUint8",
        "setInt8",
        "setUint16",
        "setInt16",
        "setUint32",
        "setInt32",
        "setFloat64",
        "setBigUint64",
    ];

    /// The `kind` tag (object field 4) marking a DataView. Real typed arrays
    /// use `TypedArrayKind::code` (0..=8); DataView and ArrayBuffer share the
    /// object layout (backing bytes at field 3, byte offset at 5, byte length
    /// at 6) but need their own dispatch so a DataView's `.length` is never
    /// misread as an element count.
    ///
    /// The corpus's linear memory is exactly this: `$mem = new
    /// Uint8Array(0)`, `$mem_dv = new DataView($mem.buffer)`, and every load
    /// and store goes through `$mem_dv.getUintN/setUintN(offset, …, true)`.
    const DATAVIEW_KIND: i32 = 9;
    const BUFFER_KIND: i32 = 10;

    /// The backing bytes of a typed array / ArrayBuffer / DataView (object
    /// field 3), as a concrete `i8` array. All three share the same backing
    /// store, so a DataView write is visible through the Uint8Array's reads —
    /// the linear-memory aliasing the corpus relies on.
    fn backing_bytes(
        &self,
        body: &mut FunctionBody,
        block: Block,
        object: Value,
    ) -> (Value, Value, Value) {
        let plain = body.add_op(
            block,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[object],
            &[self.repr.object_ty()],
        );
        let data = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 3,
            },
            &[plain],
            &[self.repr.value],
        );
        let offset = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 5,
            },
            &[plain],
            &[Type::I32],
        );
        let length = body.add_op(
            block,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 6,
            },
            &[plain],
            &[Type::I32],
        );
        let bytes = body.add_op(
            block,
            Operator::RefCast {
                ty: self.typed_array_ty(TypedArrayKind::Int8),
            },
            &[data],
            &[self.typed_array_ty(TypedArrayKind::Int8)],
        );
        (bytes, offset, length)
    }

    /// Build an ArrayBuffer-flavoured object sharing `data` (a typed array's
    /// backing bytes): `kind = BUFFER_KIND`, full range.
    fn new_buffer_object(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        data: Value,
        byte_length: Value,
    ) -> Result<LowerValue, ConvertError> {
        let trie = self.new_trie(body, block)?;
        let root = self.anyref(body, block, trie);
        let elements = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.arguments_ty(),
            },
            &[],
            &[self.repr.arguments_ty()],
        );
        let properties = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        );
        let data = self.anyref(body, block, data);
        let kind = body.add_op(
            block,
            Operator::I32Const {
                value: Self::BUFFER_KIND as u32,
            },
            &[],
            &[Type::I32],
        );
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let object = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.object,
            },
            &[root, elements, properties, data, kind, zero, byte_length],
            &[self.repr.object_ty()],
        );
        Ok(LowerValue::Wasm {
            value: object,
            kind: ValueKind::Reference,
        })
    }

    /// `new DataView(buffer)`: wrap the buffer's backing bytes with `kind =
    /// DATAVIEW_KIND` and the buffer's full byte range. The corpus always
    /// constructs a full-range view (`new DataView($mem.buffer)`), so no
    /// offset/length arguments are handled.
    fn try_provable_dataview_constructor(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        callee: &LowerValue,
        values: &ContinuationValues,
        args: &[SValueId],
    ) -> Result<Option<(Block, LowerValue)>, ConvertError> {
        let LowerValue::ReferenceKey { key: name, .. } = callee else {
            return Ok(None);
        };
        if name.0.as_ref() != "DataView" || !self.primordial_is_provable(name) {
            return Ok(None);
        }
        let Some(buffer_arg) = args.first() else {
            return Ok(None);
        };
        let buffer = values
            .get(buffer_arg)
            .cloned()
            .ok_or_else(|| ConvertError::invalid("undefined DataView buffer"))?;
        let (buffer_value, _) = buffer.wasm()?;
        let buffer_value = self.anyref(body, block, buffer_value);
        let (bytes, _offset, length) = self.backing_bytes(body, block, buffer_value);
        let trie = self.new_trie(body, block)?;
        let root = self.anyref(body, block, trie);
        let elements = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.arguments_ty(),
            },
            &[],
            &[self.repr.arguments_ty()],
        );
        let properties = body.add_op(
            block,
            Operator::RefNull {
                ty: self.repr.value,
            },
            &[],
            &[self.repr.value],
        );
        let data = self.anyref(body, block, bytes);
        let kind = body.add_op(
            block,
            Operator::I32Const {
                value: Self::DATAVIEW_KIND as u32,
            },
            &[],
            &[Type::I32],
        );
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let object = body.add_op(
            block,
            Operator::StructNew {
                sig: self.repr.object,
            },
            &[root, elements, properties, data, kind, zero, length],
            &[self.repr.object_ty()],
        );
        Ok(Some((
            block,
            LowerValue::Wasm {
                value: object,
                kind: ValueKind::Reference,
            },
        )))
    }

    /// Map a DataView accessor name to its native adapter function.
    fn dataview_instance_method(&mut self, key: &str) -> Result<Func, ConvertError> {
        match key {
            "getUint8" => self.dataview_method(DataViewOp::GetUint8),
            "getInt8" => self.dataview_method(DataViewOp::GetInt8),
            "getUint16" => self.dataview_method(DataViewOp::GetUint16),
            "getInt16" => self.dataview_method(DataViewOp::GetInt16),
            "getUint32" => self.dataview_method(DataViewOp::GetUint32),
            "getInt32" => self.dataview_method(DataViewOp::GetInt32),
            "getFloat64" => self.dataview_method(DataViewOp::GetFloat64),
            "getBigUint64" => self.dataview_method(DataViewOp::GetBigUint64),
            "setUint8" | "setInt8" => self.dataview_method(DataViewOp::SetUint8),
            "setUint16" | "setInt16" => self.dataview_method(DataViewOp::SetUint16),
            "setUint32" | "setInt32" => self.dataview_method(DataViewOp::SetUint32),
            "setFloat64" => self.dataview_method(DataViewOp::SetFloat64),
            "setBigUint64" => self.dataview_method(DataViewOp::SetBigUint64),
            _ => Err(ConvertError::invalid(format!("unknown DataView method {key:?}"))),
        }
    }

    /// Build the native adapter for one DataView accessor. All accessors
    /// share one shape: read the byte offset argument, the backing bytes,
    /// and (for multi-byte accesses) the little-endian flag, then assemble or
    /// split the value byte by byte. Bounds violations trap (a stand-in for
    /// the spec's RangeError, which the corpus never triggers).
    fn dataview_method(&mut self, op: DataViewOp) -> Result<Func, ConvertError> {
        let label = format!("dataview_{op:?}");
        self.build_native_adapter(&label, move |this, body, entry, _context, this_val, args| {
            let bytes = op.bytes();
            let (block, offset_number) = this.read_arg_number(body, entry, args, 0)?;
            let offset = body.add_op(
                block,
                Operator::I32TruncSatF64U,
                &[offset_number],
                &[Type::I32],
            );
            // Read the DataView's backing bytes, byte offset, and byte length.
            let (backing, base, _length) = this.backing_bytes(body, block, this_val);
            // Effective byte address: view base + accessor offset.
            let address = body.add_op(block, Operator::I32Add, &[base, offset], &[Type::I32]);
            // Bounds check: address + (bytes - 1) < length. The corpus stays
            // in bounds; a violation traps.
            let last = body.add_op(
                block,
                Operator::I32Const {
                    value: (bytes - 1) as u32,
                },
                &[],
                &[Type::I32],
            );
            let end = body.add_op(block, Operator::I32Add, &[address, last], &[Type::I32]);
            let in_bounds = body.add_op(block, Operator::I32LtU, &[end, _length], &[Type::I32]);
            let valid = body.add_block();
            let invalid = body.add_block();
            body.set_terminator(
                block,
                Terminator::CondBr {
                    cond: in_bounds,
                    if_true: BlockTarget {
                        block: valid,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: invalid,
                        args: vec![],
                    },
                },
            );
            body.set_terminator(invalid, Terminator::Unreachable);

            // Multi-byte accesses take a little-endian flag (single-byte
            // accessors have none). The flag is the last argument: index 1
            // for reads, 2 for writes. The corpus always passes `true`.
            let (block, little_endian) = if bytes > 1 {
                let flag_index = if op.is_write() { 2 } else { 1 };
                let (block, flag_raw) = this.read_arg_raw(body, valid, args, flag_index);
                let truthy = this.ensure_truthy_helper()?;
                let flag = body.add_op(
                    block,
                    Operator::Call {
                        function_index: truthy,
                    },
                    &[flag_raw],
                    &[Type::I32],
                );
                (block, flag)
            } else {
                let one = body.add_op(valid, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                (valid, one)
            };

            match op {
                DataViewOp::GetUint8
                | DataViewOp::GetInt8
                | DataViewOp::GetUint16
                | DataViewOp::GetInt16
                | DataViewOp::GetUint32
                | DataViewOp::GetInt32
                | DataViewOp::GetFloat64
                | DataViewOp::GetBigUint64 => {
                    let value =
                        this.dataview_read(body, block, backing, address, bytes, little_endian, op);
                    let boxed = this.box_dataview_read(body, block, value, op)?;
                    body.set_terminator(
                        block,
                        Terminator::Return {
                            values: vec![boxed],
                        },
                    );
                }
                DataViewOp::SetUint8
                | DataViewOp::SetUint16
                | DataViewOp::SetUint32
                | DataViewOp::SetFloat64
                | DataViewOp::SetBigUint64 => {
                    let (block, value) = this.read_dataview_write_value(body, block, args, op)?;
                    this.dataview_write(body, block, backing, address, bytes, little_endian, value);
                    let undef = body.add_op(
                        block,
                        Operator::RefNull {
                            ty: this.repr.value,
                        },
                        &[],
                        &[this.repr.value],
                    );
                    body.set_terminator(
                        block,
                        Terminator::Return {
                            values: vec![undef],
                        },
                    );
                }
            }
            Ok(())
        })
    }

    /// Assemble a `bytes`-wide little-/big-endian value from `backing` at
    /// `address`, as raw bits (i32 for 8/16/32, i64 for 64, f64 reinterpreted
    /// for float64).
    fn dataview_read(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        backing: Value,
        address: Value,
        bytes: u32,
        little_endian: Value,
        op: DataViewOp,
    ) -> Value {
        // Read each byte (unsigned) into a vector of i32 values.
        let mut byte_vals = Vec::with_capacity(bytes as usize);
        for i in 0..bytes {
            let index = body.add_op(block, Operator::I32Const { value: i }, &[], &[Type::I32]);
            let at = body.add_op(block, Operator::I32Add, &[address, index], &[Type::I32]);
            let b = body.add_op(
                block,
                Operator::ArrayGetU {
                    sig: self.repr.typed_i8,
                },
                &[backing, at],
                &[Type::I32],
            );
            byte_vals.push(b);
        }
        // Assemble little- and big-endian forms, then select.
        let assemble = |body: &mut FunctionBody, block: Block, order: &[usize]| -> Value {
            match op {
                DataViewOp::GetFloat64 | DataViewOp::GetBigUint64 => {
                    let mut acc = {
                        let b = body.add_op(block, Operator::I64ExtendI32U, &[byte_vals[order[0]]], &[Type::I64]);
                        b
                    };
                    for (pos, &idx) in order.iter().enumerate().skip(1) {
                        let shift = body.add_op(
                            block,
                            Operator::I64Const {
                                value: (8 * pos) as u64,
                            },
                            &[],
                            &[Type::I64],
                        );
                        let wide = body.add_op(block, Operator::I64ExtendI32U, &[byte_vals[idx]], &[Type::I64]);
                        let shifted = body.add_op(block, Operator::I64Shl, &[wide, shift], &[Type::I64]);
                        acc = body.add_op(block, Operator::I64Or, &[acc, shifted], &[Type::I64]);
                    }
                    acc
                }
                _ => {
                    let mut acc = byte_vals[order[0]];
                    for (pos, &idx) in order.iter().enumerate().skip(1) {
                        let shift = body.add_op(
                            block,
                            Operator::I32Const {
                                value: (8 * pos) as u32,
                            },
                            &[],
                            &[Type::I32],
                        );
                        let shifted = body.add_op(block, Operator::I32Shl, &[byte_vals[idx], shift], &[Type::I32]);
                        acc = body.add_op(block, Operator::I32Or, &[acc, shifted], &[Type::I32]);
                    }
                    acc
                }
            }
        };
        let le_order: Vec<usize> = (0..bytes as usize).collect();
        let be_order: Vec<usize> = (0..bytes as usize).rev().collect();
        let le = assemble(body, block, &le_order);
        let be = assemble(body, block, &be_order);
        let ty = match op {
            DataViewOp::GetFloat64 | DataViewOp::GetBigUint64 => Type::I64,
            _ => Type::I32,
        };
        body.add_op(
            block,
            Operator::TypedSelect { ty },
            &[le, be, little_endian],
            &[ty],
        )
    }

    /// Box a freshly-assembled DataView read into its JS result value.
    fn box_dataview_read(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        value: Value,
        op: DataViewOp,
    ) -> Result<Value, ConvertError> {
        match op {
            DataViewOp::GetUint8 | DataViewOp::GetUint16 | DataViewOp::GetUint32 => {
                // Unsigned widening to f64. 32-bit needs the unsigned convert.
                let f = if op == DataViewOp::GetUint32 {
                    body.add_op(block, Operator::F64ConvertI32U, &[value], &[Type::F64])
                } else {
                    body.add_op(block, Operator::F64ConvertI32S, &[value], &[Type::F64])
                };
                self.box_value(body, block, &LowerValue::Wasm {
                    value: f,
                    kind: ValueKind::Number,
                })
            }
            DataViewOp::GetInt8 => {
                // Sign-extend the low byte.
                let shift = body.add_op(block, Operator::I32Const { value: 24 }, &[], &[Type::I32]);
                let shl = body.add_op(block, Operator::I32Shl, &[value, shift], &[Type::I32]);
                let sar = body.add_op(block, Operator::I32ShrS, &[shl, shift], &[Type::I32]);
                let f = body.add_op(block, Operator::F64ConvertI32S, &[sar], &[Type::F64]);
                self.box_value(body, block, &LowerValue::Wasm {
                    value: f,
                    kind: ValueKind::Number,
                })
            }
            DataViewOp::GetInt16 => {
                let shift = body.add_op(block, Operator::I32Const { value: 16 }, &[], &[Type::I32]);
                let shl = body.add_op(block, Operator::I32Shl, &[value, shift], &[Type::I32]);
                let sar = body.add_op(block, Operator::I32ShrS, &[shl, shift], &[Type::I32]);
                let f = body.add_op(block, Operator::F64ConvertI32S, &[sar], &[Type::F64]);
                self.box_value(body, block, &LowerValue::Wasm {
                    value: f,
                    kind: ValueKind::Number,
                })
            }
            DataViewOp::GetInt32 => {
                let f = body.add_op(block, Operator::F64ConvertI32S, &[value], &[Type::F64]);
                self.box_value(body, block, &LowerValue::Wasm {
                    value: f,
                    kind: ValueKind::Number,
                })
            }
            DataViewOp::GetFloat64 => {
                let f = body.add_op(block, Operator::F64ReinterpretI64, &[value], &[Type::F64]);
                self.box_value(body, block, &LowerValue::Wasm {
                    value: f,
                    kind: ValueKind::Number,
                })
            }
            DataViewOp::GetBigUint64 => {
                // The raw i64 is the BigInt payload.
                self.box_value(body, block, &LowerValue::Wasm {
                    value,
                    kind: ValueKind::BigInt,
                })
            }
            _ => unreachable!("not a read op"),
        }
    }

    /// Read the value argument of a DataView write, as raw bits (i32 or i64).
    fn read_dataview_write_value(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        args: Value,
        op: DataViewOp,
    ) -> Result<(Block, Value), ConvertError> {
        match op {
            DataViewOp::SetBigUint64 => {
                // The BigInt value: unbox to i64.
                let (block, raw) = self.read_arg_raw(body, block, args, 1);
                let i = self.as_i64(body, block, &LowerValue::Wasm {
                    value: raw,
                    kind: ValueKind::Reference,
                })?;
                Ok((block, i))
            }
            DataViewOp::SetFloat64 => {
                let (block, f) = self.read_arg_number(body, block, args, 1)?;
                let bits = body.add_op(block, Operator::I64ReinterpretF64, &[f], &[Type::I64]);
                Ok((block, bits))
            }
            _ => {
                let (block, f) = self.read_arg_number(body, block, args, 1)?;
                let i = body.add_op(block, Operator::I32TruncF64S, &[f], &[Type::I32]);
                Ok((block, i))
            }
        }
    }

    /// Split a raw value into `bytes` little-/big-endian bytes and store them
    /// into `backing` at `address`.
    fn dataview_write(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        backing: Value,
        address: Value,
        bytes: u32,
        little_endian: Value,
        value: Value,
    ) {
        let wide = bytes == 8;
        // Compute the byte at logical position `pos` (0 = least significant).
        let byte_at = |body: &mut FunctionBody, block: Block, pos: u32| -> Value {
            let shift_amt = 8 * pos;
            if wide {
                let shift = body.add_op(
                    block,
                    Operator::I64Const {
                        value: shift_amt as u64,
                    },
                    &[],
                    &[Type::I64],
                );
                let shifted = body.add_op(block, Operator::I64ShrU, &[value, shift], &[Type::I64]);
                body.add_op(block, Operator::I32WrapI64, &[shifted], &[Type::I32])
            } else {
                if shift_amt == 0 {
                    value
                } else {
                    let shift = body.add_op(
                        block,
                        Operator::I32Const {
                            value: shift_amt,
                        },
                        &[],
                        &[Type::I32],
                    );
                    body.add_op(block, Operator::I32ShrU, &[value, shift], &[Type::I32])
                }
            }
        };
        for i in 0..bytes {
            // Little-endian stores logical byte i at address+i; big-endian
            // stores logical byte (bytes-1-i) there.
            let le_pos = i;
            let be_pos = bytes - 1 - i;
            let le_byte = byte_at(body, block, le_pos);
            let be_byte = byte_at(body, block, be_pos);
            let byte = body.add_op(
                block,
                Operator::TypedSelect { ty: Type::I32 },
                &[le_byte, be_byte, little_endian],
                &[Type::I32],
            );
            let index = body.add_op(block, Operator::I32Const { value: i }, &[], &[Type::I32]);
            let at = body.add_op(block, Operator::I32Add, &[address, index], &[Type::I32]);
            body.add_op(
                block,
                Operator::ArraySet {
                    sig: self.repr.typed_i8,
                },
                &[backing, at, byte],
                &[],
            );
        }
    }

    fn typed_array_subarray_index(
        &self,
        body: &mut FunctionBody,
        block: Block,
        number: Value,
        length: Value,
    ) -> Value {
        let signed = body.add_op(block, Operator::I32TruncSatF64S, &[number], &[Type::I32]);
        let zero = body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let negative = body.add_op(block, Operator::I32LtS, &[signed, zero], &[Type::I32]);
        let relative = body.add_op(block, Operator::I32Add, &[length, signed], &[Type::I32]);
        let below_zero = body.add_op(block, Operator::I32LtS, &[relative, zero], &[Type::I32]);
        let negative_value = body.add_op(
            block,
            Operator::TypedSelect { ty: Type::I32 },
            &[zero, relative, below_zero],
            &[Type::I32],
        );
        let past_end = body.add_op(block, Operator::I32LtU, &[length, signed], &[Type::I32]);
        let positive_value = body.add_op(
            block,
            Operator::TypedSelect { ty: Type::I32 },
            &[length, signed, past_end],
            &[Type::I32],
        );
        body.add_op(
            block,
            Operator::TypedSelect { ty: Type::I32 },
            &[negative_value, positive_value, negative],
            &[Type::I32],
        )
    }

    fn typed_array_subarray_method(&mut self) -> Result<Func, ConvertError> {
        self.build_native_adapter(
            "typed_array_subarray",
            |this, body, entry, _context, this_val, args| {
                let (data, tag, offset, length) = this.typed_array_parts(body, entry, this_val);
                let (block, begin) = this.read_arg_number(body, entry, args, 0)?;
                let begin = this.typed_array_subarray_index(body, block, begin, length);
                let (block, end_raw) = this.read_arg_raw(body, block, args, 1);
                let absent = body.add_op(block, Operator::RefIsNull, &[end_raw], &[Type::I32]);
                let default_end = body.add_block();
                let explicit_end = body.add_block();
                let end_join = body.add_block();
                let end = body.add_blockparam(end_join, Type::I32);
                body.set_terminator(
                    block,
                    Terminator::CondBr {
                        cond: absent,
                        if_true: BlockTarget {
                            block: default_end,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: explicit_end,
                            args: vec![],
                        },
                    },
                );
                body.set_terminator(
                    default_end,
                    Terminator::Br {
                        target: BlockTarget {
                            block: end_join,
                            args: vec![length],
                        },
                    },
                );
                let number = this.as_f64(
                    body,
                    explicit_end,
                    &LowerValue::Wasm {
                        value: end_raw,
                        kind: ValueKind::Reference,
                    },
                )?;
                let end_value = this.typed_array_subarray_index(body, explicit_end, number, length);
                body.set_terminator(
                    explicit_end,
                    Terminator::Br {
                        target: BlockTarget {
                            block: end_join,
                            args: vec![end_value],
                        },
                    },
                );
                let reversed = body.add_op(end_join, Operator::I32LtU, &[end, begin], &[Type::I32]);
                let zero =
                    body.add_op(end_join, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
                let span = body.add_op(end_join, Operator::I32Sub, &[end, begin], &[Type::I32]);
                let span = body.add_op(
                    end_join,
                    Operator::TypedSelect { ty: Type::I32 },
                    &[zero, span, reversed],
                    &[Type::I32],
                );
                let offset =
                    body.add_op(end_join, Operator::I32Add, &[offset, begin], &[Type::I32]);
                let (end_join, result) = this.typed_kind_value(
                    body,
                    end_join,
                    tag,
                    data,
                    |this, body, block, kind, data| {
                        let array =
                            this.new_typed_array_object(body, block, kind, data, offset, span)?;
                        let array = this.box_value(body, block, &array)?;
                        Ok((block, array))
                    },
                )?;
                body.set_terminator(
                    end_join,
                    Terminator::Return {
                        values: vec![result],
                    },
                );
                Ok(())
            },
        )
    }

    fn typed_array_set_method(&mut self) -> Result<Func, ConvertError> {
        self.build_native_adapter(
            "typed_array_set",
            |this, body, entry, _context, this_val, args| {
                let (block, source) = this.read_arg_raw(body, entry, args, 0);
                let (block, offset_number) = this.read_arg_number(body, block, args, 1)?;
                let zero_number = body.add_op(
                    block,
                    Operator::F64Const {
                        value: 0.0f64.to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                let max_offset = body.add_op(
                    block,
                    Operator::F64Const {
                        value: 4_294_967_296.0f64.to_bits(),
                    },
                    &[],
                    &[Type::F64],
                );
                let negative_offset = body.add_op(
                    block,
                    Operator::F64Lt,
                    &[offset_number, zero_number],
                    &[Type::I32],
                );
                let huge_offset = body.add_op(
                    block,
                    Operator::F64Ge,
                    &[offset_number, max_offset],
                    &[Type::I32],
                );
                let invalid_offset = body.add_op(
                    block,
                    Operator::I32Or,
                    &[negative_offset, huge_offset],
                    &[Type::I32],
                );
                let checked_offset = body.add_block();
                let invalid = body.add_block();
                body.set_terminator(
                    block,
                    Terminator::CondBr {
                        cond: invalid_offset,
                        if_true: BlockTarget {
                            block: invalid,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: checked_offset,
                            args: vec![],
                        },
                    },
                );
                body.set_terminator(invalid, Terminator::Unreachable);
                let offset = body.add_op(
                    checked_offset,
                    Operator::I32TruncSatF64U,
                    &[offset_number],
                    &[Type::I32],
                );
                let is_object = body.add_op(
                    checked_offset,
                    Operator::RefTest {
                        ty: this.repr.object_ty(),
                    },
                    &[source],
                    &[Type::I32],
                );
                let valid = body.add_block();
                body.set_terminator(
                    checked_offset,
                    Terminator::CondBr {
                        cond: is_object,
                        if_true: BlockTarget {
                            block: valid,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: invalid,
                            args: vec![],
                        },
                    },
                );
                let source_obj = body.add_op(
                    valid,
                    Operator::RefCast {
                        ty: this.repr.object_ty(),
                    },
                    &[source],
                    &[this.repr.object_ty()],
                );
                let source_data = body.add_op(
                    valid,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 3,
                    },
                    &[source_obj],
                    &[this.repr.value],
                );
                let no_typed_source =
                    body.add_op(valid, Operator::RefIsNull, &[source_data], &[Type::I32]);
                let target_obj = body.add_op(
                    valid,
                    Operator::RefCast {
                        ty: this.repr.object_ty(),
                    },
                    &[this_val],
                    &[this.repr.object_ty()],
                );
                let target_len = body.add_op(
                    valid,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 6,
                    },
                    &[target_obj],
                    &[Type::I32],
                );
                let target_data = body.add_op(
                    valid,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 3,
                    },
                    &[target_obj],
                    &[this.repr.value],
                );
                let target_tag = body.add_op(
                    valid,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 4,
                    },
                    &[target_obj],
                    &[Type::I32],
                );
                let target_offset = body.add_op(
                    valid,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 5,
                    },
                    &[target_obj],
                    &[Type::I32],
                );
                let typed_source = body.add_block();
                let ordinary_source = body.add_block();
                body.set_terminator(
                    valid,
                    Terminator::CondBr {
                        cond: no_typed_source,
                        if_true: BlockTarget {
                            block: ordinary_source,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: typed_source,
                            args: vec![],
                        },
                    },
                );
                let source_len = body.add_op(
                    typed_source,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 6,
                    },
                    &[source_obj],
                    &[Type::I32],
                );
                let source_tag = body.add_op(
                    typed_source,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 4,
                    },
                    &[source_obj],
                    &[Type::I32],
                );
                let source_offset = body.add_op(
                    typed_source,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 5,
                    },
                    &[source_obj],
                    &[Type::I32],
                );
                let end = body.add_op(
                    typed_source,
                    Operator::I32Add,
                    &[offset, source_len],
                    &[Type::I32],
                );
                let too_long = body.add_op(
                    typed_source,
                    Operator::I32LtU,
                    &[target_len, end],
                    &[Type::I32],
                );
                let copy = body.add_block();
                body.set_terminator(
                    typed_source,
                    Terminator::CondBr {
                        cond: too_long,
                        if_true: BlockTarget {
                            block: invalid,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: copy,
                            args: vec![],
                        },
                    },
                );
                let same_kind = body.add_op(
                    copy,
                    Operator::I32Eq,
                    &[source_tag, target_tag],
                    &[Type::I32],
                );
                let direct_copy = body.add_block();
                let converted_copy = body.add_block();
                body.set_terminator(
                    copy,
                    Terminator::CondBr {
                        cond: same_kind,
                        if_true: BlockTarget {
                            block: direct_copy,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: converted_copy,
                            args: vec![],
                        },
                    },
                );
                let destination = body.add_op(
                    direct_copy,
                    Operator::I32Add,
                    &[target_offset, offset],
                    &[Type::I32],
                );
                let direct_copy = this.typed_kind_effect(
                    body,
                    direct_copy,
                    target_tag,
                    target_data,
                    |this, body, block, kind, target_data| {
                        let source_data = body.add_op(
                            block,
                            Operator::RefCast {
                                ty: this.typed_array_ty(kind),
                            },
                            &[source_data],
                            &[this.typed_array_ty(kind)],
                        );
                        let sig = this.typed_array_sig(kind);
                        body.add_op(
                            block,
                            Operator::ArrayCopy {
                                dest: sig,
                                src: sig,
                            },
                            &[
                                target_data,
                                destination,
                                source_data,
                                source_offset,
                                source_len,
                            ],
                            &[],
                        );
                        Ok(block)
                    },
                )?;
                let undef = body.add_op(
                    direct_copy,
                    Operator::RefNull {
                        ty: this.repr.value,
                    },
                    &[],
                    &[this.repr.value],
                );
                body.set_terminator(
                    direct_copy,
                    Terminator::Return {
                        values: vec![undef],
                    },
                );
                let converted_copy = this.for_each_index(
                    body,
                    converted_copy,
                    source_len,
                    |this, body, block, i| {
                        let (block, value) = this.typed_array_read(body, block, source, i)?;
                        let target_index =
                            body.add_op(block, Operator::I32Add, &[offset, i], &[Type::I32]);
                        this.typed_array_write(body, block, this_val, target_index, &value)
                    },
                )?;
                let undef = body.add_op(
                    converted_copy,
                    Operator::RefNull {
                        ty: this.repr.value,
                    },
                    &[],
                    &[this.repr.value],
                );
                body.set_terminator(
                    converted_copy,
                    Terminator::Return {
                        values: vec![undef],
                    },
                );

                let elements = body.add_op(
                    ordinary_source,
                    Operator::StructGet {
                        sig: this.repr.object,
                        idx: 1,
                    },
                    &[source_obj],
                    &[this.repr.arguments_ty()],
                );
                let no_elements = body.add_op(
                    ordinary_source,
                    Operator::RefIsNull,
                    &[elements],
                    &[Type::I32],
                );
                let copy_ordinary = body.add_block();
                body.set_terminator(
                    ordinary_source,
                    Terminator::CondBr {
                        cond: no_elements,
                        if_true: BlockTarget {
                            block: invalid,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: copy_ordinary,
                            args: vec![],
                        },
                    },
                );
                let source_len =
                    body.add_op(copy_ordinary, Operator::ArrayLen, &[elements], &[Type::I32]);
                let end = body.add_op(
                    copy_ordinary,
                    Operator::I32Add,
                    &[offset, source_len],
                    &[Type::I32],
                );
                let too_long = body.add_op(
                    copy_ordinary,
                    Operator::I32LtU,
                    &[target_len, end],
                    &[Type::I32],
                );
                let write_ordinary = body.add_block();
                body.set_terminator(
                    copy_ordinary,
                    Terminator::CondBr {
                        cond: too_long,
                        if_true: BlockTarget {
                            block: invalid,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: write_ordinary,
                            args: vec![],
                        },
                    },
                );
                let write_ordinary = this.for_each_index(
                    body,
                    write_ordinary,
                    source_len,
                    |this, body, block, i| {
                        let element = body.add_op(
                            block,
                            Operator::ArrayGet {
                                sig: this.repr.arguments,
                            },
                            &[elements, i],
                            &[this.repr.value],
                        );
                        let target_index =
                            body.add_op(block, Operator::I32Add, &[offset, i], &[Type::I32]);
                        this.typed_array_write(
                            body,
                            block,
                            this_val,
                            target_index,
                            &LowerValue::Wasm {
                                value: element,
                                kind: ValueKind::Reference,
                            },
                        )
                    },
                )?;
                let undef = body.add_op(
                    write_ordinary,
                    Operator::RefNull {
                        ty: this.repr.value,
                    },
                    &[],
                    &[this.repr.value],
                );
                body.set_terminator(
                    write_ordinary,
                    Terminator::Return {
                        values: vec![undef],
                    },
                );
                Ok(())
            },
        )
    }

    fn get_typed_array_static_property(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
    ) -> Result<Option<(Block, LowerValue)>, ConvertError> {
        let (value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return Ok(None);
        }
        let is_object = body.add_op(
            block,
            Operator::RefTest {
                ty: self.repr.object_non_null_ty(),
            },
            &[value],
            &[Type::I32],
        );
        let typed = body.add_block();
        let non_object = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: is_object,
                if_true: BlockTarget {
                    block: typed,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: non_object,
                    args: vec![],
                },
            },
        );
        let plain = body.add_op(
            typed,
            Operator::RefCast {
                ty: self.repr.object_ty(),
            },
            &[value],
            &[self.repr.object_ty()],
        );
        let data = body.add_op(
            typed,
            Operator::StructGet {
                sig: self.repr.object,
                idx: 3,
            },
            &[plain],
            &[self.repr.value],
        );
        let absent = body.add_op(typed, Operator::RefIsNull, &[data], &[Type::I32]);
        let is_typed = body.add_block();
        body.set_terminator(
            typed,
            Terminator::CondBr {
                cond: absent,
                if_true: BlockTarget {
                    block: non_object,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: is_typed,
                    args: vec![],
                },
            },
        );
        let value_result = match key {
            "length" => {
                let length = body.add_op(
                    is_typed,
                    Operator::StructGet {
                        sig: self.repr.object,
                        idx: 6,
                    },
                    &[plain],
                    &[Type::I32],
                );
                let number =
                    body.add_op(is_typed, Operator::F64ConvertI32U, &[length], &[Type::F64]);
                self.box_value(
                    body,
                    is_typed,
                    &LowerValue::Wasm {
                        value: number,
                        kind: ValueKind::Number,
                    },
                )?
            }
            "byteLength" => {
                let length = body.add_op(
                    is_typed,
                    Operator::StructGet {
                        sig: self.repr.object,
                        idx: 6,
                    },
                    &[plain],
                    &[Type::I32],
                );
                let tag = body.add_op(
                    is_typed,
                    Operator::StructGet {
                        sig: self.repr.object,
                        idx: 4,
                    },
                    &[plain],
                    &[Type::I32],
                );
                let (end, bytes) = self.typed_kind_value(
                    body,
                    is_typed,
                    tag,
                    data,
                    |this, body, block, kind, _| {
                        let size = body.add_op(
                            block,
                            Operator::I32Const {
                                value: kind.bytes_per_element() as u32,
                            },
                            &[],
                            &[Type::I32],
                        );
                        let bytes =
                            body.add_op(block, Operator::I32Mul, &[length, size], &[Type::I32]);
                        let bytes =
                            body.add_op(block, Operator::F64ConvertI32U, &[bytes], &[Type::F64]);
                        this.box_value(
                            body,
                            block,
                            &LowerValue::Wasm {
                                value: bytes,
                                kind: ValueKind::Number,
                            },
                        )
                        .map(|v| (block, v))
                    },
                )?;
                body.set_terminator(
                    end,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![bytes],
                        },
                    },
                );
                let (fallback, fallback_value) =
                    self.get_nonstring_property_value_raw(body, non_object, object, key)?;
                let fallback_value = self.box_value(body, fallback, &fallback_value)?;
                body.set_terminator(
                    fallback,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![fallback_value],
                        },
                    },
                );
                return Ok(Some((
                    join,
                    LowerValue::Wasm {
                        value: result,
                        kind: ValueKind::Reference,
                    },
                )));
            }
            "set" | "subarray" => {
                let func = self.typed_array_instance_method(key)?;
                let context = self.new_object(body, is_typed)?;
                let (context, _) = context.wasm()?;
                let method = self.native_function_value(body, is_typed, context, func)?;
                self.box_value(body, is_typed, &method)?
            }
            // `someTypedArray.buffer`: an ArrayBuffer-flavoured object sharing
            // the same backing bytes (so a DataView over it aliases the typed
            // array — the corpus's `$mem_dv = new DataView($mem.buffer)`).
            "buffer" => {
                let (bytes, _offset, length) = self.backing_bytes(body, is_typed, value);
                let buffer = self.new_buffer_object(body, is_typed, bytes, length)?;
                self.box_value(body, is_typed, &buffer)?
            }
            // DataView accessors (`getUint32`, `setBigUint64`, …). These only
            // ever apply to a DataView (a typed array has no such methods), so
            // returning the native method unconditionally here is sound.
            key if Self::DATAVIEW_METHODS.contains(&key) => {
                let func = self.dataview_instance_method(key)?;
                let context = self.new_object(body, is_typed)?;
                let (context, _) = context.wasm()?;
                let method = self.native_function_value(body, is_typed, context, func)?;
                self.box_value(body, is_typed, &method)?
            }
            _ => {
                let Some(index) = Self::static_array_index(key) else {
                    return Ok(None);
                };
                let index = body.add_op(
                    is_typed,
                    Operator::I32Const { value: index },
                    &[],
                    &[Type::I32],
                );
                let (read, value) = self.typed_array_read(body, is_typed, value, index)?;
                let value = self.box_value(body, read, &value)?;
                body.set_terminator(
                    read,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![value],
                        },
                    },
                );
                let (fallback, fallback_value) =
                    self.get_nonstring_property_value_raw(body, non_object, object, key)?;
                let fallback_value = self.box_value(body, fallback, &fallback_value)?;
                body.set_terminator(
                    fallback,
                    Terminator::Br {
                        target: BlockTarget {
                            block: join,
                            args: vec![fallback_value],
                        },
                    },
                );
                return Ok(Some((
                    join,
                    LowerValue::Wasm {
                        value: result,
                        kind: ValueKind::Reference,
                    },
                )));
            }
        };
        body.set_terminator(
            is_typed,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![value_result],
                },
            },
        );
        let (fallback, fallback_value) =
            self.get_nonstring_property_value_raw(body, non_object, object, key)?;
        let fallback_value = self.box_value(body, fallback, &fallback_value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![fallback_value],
                },
            },
        );
        Ok(Some((
            join,
            LowerValue::Wasm {
                value: result,
                kind: ValueKind::Reference,
            },
        )))
    }

    fn get_string_member_raw(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: Value,
    ) -> Result<(Block, LowerValue), ConvertError> {
        const INVALID: u32 = u32::MAX;
        let (object_value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.get_string_member_raw_base(body, block, object, key);
        }
        let (block, typed) = self.typed_object_test(body, block, object_value);
        let typed_block = body.add_block();
        let fallback = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: typed,
                if_true: BlockTarget {
                    block: typed_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
            },
        );

        let equal = self.ensure_string_equal()?;
        let parser = self.ensure_string_index()?;
        let length_name = self.new_string(body, typed_block, b"length")?;
        let is_length = body.add_op(
            typed_block,
            Operator::Call {
                function_index: equal,
            },
            &[key, length_name],
            &[Type::I32],
        );
        let length = body.add_block();
        let after_length = body.add_block();
        body.set_terminator(
            typed_block,
            Terminator::CondBr {
                cond: is_length,
                if_true: BlockTarget {
                    block: length,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: after_length,
                    args: vec![],
                },
            },
        );
        let length_value = self.typed_array_length_value(body, length, object_value)?;
        let length_value = self.box_value(body, length, &length_value)?;
        body.set_terminator(
            length,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![length_value],
                },
            },
        );

        let byte_length_name = self.new_string(body, after_length, b"byteLength")?;
        let is_byte_length = body.add_op(
            after_length,
            Operator::Call {
                function_index: equal,
            },
            &[key, byte_length_name],
            &[Type::I32],
        );
        let byte_length = body.add_block();
        let after_byte_length = body.add_block();
        body.set_terminator(
            after_length,
            Terminator::CondBr {
                cond: is_byte_length,
                if_true: BlockTarget {
                    block: byte_length,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: after_byte_length,
                    args: vec![],
                },
            },
        );
        let (byte_length, byte_length_value) =
            self.typed_array_byte_length_value(body, byte_length, object_value)?;
        let byte_length_value = self.box_value(body, byte_length, &byte_length_value)?;
        body.set_terminator(
            byte_length,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![byte_length_value],
                },
            },
        );

        let set_name = self.new_string(body, after_byte_length, b"set")?;
        let is_set = body.add_op(
            after_byte_length,
            Operator::Call {
                function_index: equal,
            },
            &[key, set_name],
            &[Type::I32],
        );
        let set = body.add_block();
        let after_set = body.add_block();
        body.set_terminator(
            after_byte_length,
            Terminator::CondBr {
                cond: is_set,
                if_true: BlockTarget {
                    block: set,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: after_set,
                    args: vec![],
                },
            },
        );
        let func = self.typed_array_instance_method("set")?;
        let context = self.new_object(body, set)?;
        let (context, _) = context.wasm()?;
        let method = self.native_function_value(body, set, context, func)?;
        let method = self.box_value(body, set, &method)?;
        body.set_terminator(
            set,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![method],
                },
            },
        );

        let subarray_name = self.new_string(body, after_set, b"subarray")?;
        let is_subarray = body.add_op(
            after_set,
            Operator::Call {
                function_index: equal,
            },
            &[key, subarray_name],
            &[Type::I32],
        );
        let subarray = body.add_block();
        let maybe_index = body.add_block();
        body.set_terminator(
            after_set,
            Terminator::CondBr {
                cond: is_subarray,
                if_true: BlockTarget {
                    block: subarray,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: maybe_index,
                    args: vec![],
                },
            },
        );
        let func = self.typed_array_instance_method("subarray")?;
        let context = self.new_object(body, subarray)?;
        let (context, _) = context.wasm()?;
        let method = self.native_function_value(body, subarray, context, func)?;
        let method = self.box_value(body, subarray, &method)?;
        body.set_terminator(
            subarray,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![method],
                },
            },
        );

        let index = body.add_op(
            maybe_index,
            Operator::Call {
                function_index: parser,
            },
            &[key],
            &[Type::I32],
        );
        let invalid = body.add_op(
            maybe_index,
            Operator::I32Const { value: INVALID },
            &[],
            &[Type::I32],
        );
        let is_invalid = body.add_op(
            maybe_index,
            Operator::I32Eq,
            &[index, invalid],
            &[Type::I32],
        );
        let indexed = body.add_block();
        body.set_terminator(
            maybe_index,
            Terminator::CondBr {
                cond: is_invalid,
                if_true: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: indexed,
                    args: vec![],
                },
            },
        );
        let (indexed, indexed_value) = self.typed_array_read(body, indexed, object_value, index)?;
        let indexed_value = self.box_value(body, indexed, &indexed_value)?;
        body.set_terminator(
            indexed,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![indexed_value],
                },
            },
        );

        let (fallback, fallback_value) =
            self.get_string_member_raw_base(body, fallback, object, key)?;
        let fallback_value = self.box_value(body, fallback, &fallback_value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![fallback_value],
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

    /// Dynamic string writes need the same indexed-property distinction as reads.
    /// The fallback preserves ordinary object behavior for names which are not
    /// canonical array indices; the read-only typed-array metadata is a no-op.
    fn set_string_member_raw(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: Value,
        value: &LowerValue,
    ) -> Result<Block, ConvertError> {
        const INVALID: u32 = u32::MAX;
        let (object_value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.set_string_member_raw_base(body, block, object, key, value);
        }
        let (block, typed) = self.typed_object_test(body, block, object_value);
        let typed_block = body.add_block();
        let fallback = body.add_block();
        let done = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: typed,
                if_true: BlockTarget {
                    block: typed_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
            },
        );

        let equal = self.ensure_string_equal()?;
        let parser = self.ensure_string_index()?;
        let length_name = self.new_string(body, typed_block, b"length")?;
        let is_length = body.add_op(
            typed_block,
            Operator::Call {
                function_index: equal,
            },
            &[key, length_name],
            &[Type::I32],
        );
        let readonly = body.add_block();
        let after_length = body.add_block();
        body.set_terminator(
            typed_block,
            Terminator::CondBr {
                cond: is_length,
                if_true: BlockTarget {
                    block: readonly,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: after_length,
                    args: vec![],
                },
            },
        );

        let byte_length_name = self.new_string(body, after_length, b"byteLength")?;
        let is_byte_length = body.add_op(
            after_length,
            Operator::Call {
                function_index: equal,
            },
            &[key, byte_length_name],
            &[Type::I32],
        );
        let maybe_index = body.add_block();
        body.set_terminator(
            after_length,
            Terminator::CondBr {
                cond: is_byte_length,
                if_true: BlockTarget {
                    block: readonly,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: maybe_index,
                    args: vec![],
                },
            },
        );
        body.set_terminator(
            readonly,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );

        let index = body.add_op(
            maybe_index,
            Operator::Call {
                function_index: parser,
            },
            &[key],
            &[Type::I32],
        );
        let invalid = body.add_op(
            maybe_index,
            Operator::I32Const { value: INVALID },
            &[],
            &[Type::I32],
        );
        let is_invalid = body.add_op(
            maybe_index,
            Operator::I32Eq,
            &[index, invalid],
            &[Type::I32],
        );
        let indexed = body.add_block();
        body.set_terminator(
            maybe_index,
            Terminator::CondBr {
                cond: is_invalid,
                if_true: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: indexed,
                    args: vec![],
                },
            },
        );
        let indexed = self.typed_array_write(body, indexed, object_value, index, value)?;
        body.set_terminator(
            indexed,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );

        let fallback = self.set_string_member_raw_base(body, fallback, object, key, value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        Ok(done)
    }

    /// Runtime numeric indexing reaches this path when the key was not a source
    /// literal. Test typed storage before the ordinary boxed-array path, then
    /// delegate unchanged for strings, ordinary arrays, and plain objects.
    fn get_nonstring_numeric_member(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        index: Value,
    ) -> Result<(Block, LowerValue), ConvertError> {
        let (value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.get_nonstring_numeric_member_base(body, block, object, index);
        }
        let (block, typed) = self.typed_object_test(body, block, value);
        let typed_block = body.add_block();
        let fallback = body.add_block();
        let join = body.add_block();
        let result = body.add_blockparam(join, self.repr.value);
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: typed,
                if_true: BlockTarget {
                    block: typed_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
            },
        );
        let (typed_block, typed_value) = self.typed_array_read(body, typed_block, value, index)?;
        let typed_value = self.box_value(body, typed_block, &typed_value)?;
        body.set_terminator(
            typed_block,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![typed_value],
                },
            },
        );
        let (fallback, fallback_value) =
            self.get_nonstring_numeric_member_base(body, fallback, object, index)?;
        let fallback_value = self.box_value(body, fallback, &fallback_value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: join,
                    args: vec![fallback_value],
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

    fn set_numeric_member_raw(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        index: Value,
        value: &LowerValue,
    ) -> Result<Block, ConvertError> {
        let (object_value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.set_numeric_member_raw_base(body, block, object, index, value);
        }
        let (block, typed) = self.typed_object_test(body, block, object_value);
        let typed_block = body.add_block();
        let fallback = body.add_block();
        let done = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: typed,
                if_true: BlockTarget {
                    block: typed_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
            },
        );
        let typed_block = self.typed_array_write(body, typed_block, object_value, index, value)?;
        body.set_terminator(
            typed_block,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        let fallback = self.set_numeric_member_raw_base(body, fallback, object, index, value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        Ok(done)
    }

    fn set_static_array_index_raw(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
        index: u32,
        value: &LowerValue,
    ) -> Result<Block, ConvertError> {
        let (object_value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.set_static_array_index_raw_base(body, block, object, key, index, value);
        }
        let (block, typed) = self.typed_object_test(body, block, object_value);
        let typed_block = body.add_block();
        let fallback = body.add_block();
        let done = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: typed,
                if_true: BlockTarget {
                    block: typed_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
            },
        );
        let static_index = index;
        let index = body.add_op(
            typed_block,
            Operator::I32Const {
                value: static_index,
            },
            &[],
            &[Type::I32],
        );
        let typed_block = self.typed_array_write(body, typed_block, object_value, index, value)?;
        body.set_terminator(
            typed_block,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        let fallback =
            self.set_static_array_index_raw_base(body, fallback, object, key, static_index, value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        Ok(done)
    }

    fn set_static_length_property_raw(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        object: &LowerValue,
        key: &str,
        value: &LowerValue,
    ) -> Result<Block, ConvertError> {
        let (object_value, kind) = object.wasm()?;
        if kind != ValueKind::Reference {
            return self.set_static_length_property_raw_base(body, block, object, key, value);
        }
        let (block, typed) = self.typed_object_test(body, block, object_value);
        let noop = body.add_block();
        let fallback = body.add_block();
        let done = body.add_block();
        body.set_terminator(
            block,
            Terminator::CondBr {
                cond: typed,
                if_true: BlockTarget {
                    block: noop,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: fallback,
                    args: vec![],
                },
            },
        );
        body.set_terminator(
            noop,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        let fallback =
            self.set_static_length_property_raw_base(body, fallback, object, key, value)?;
        body.set_terminator(
            fallback,
            Terminator::Br {
                target: BlockTarget {
                    block: done,
                    args: vec![],
                },
            },
        );
        Ok(done)
    }
}
