use std::{error::Error, fmt};

use portal_pc_waffle::{
    Module, Signature, SignatureData, StorageType, Type, WithMutablility, WithNullable,
};

use crate::repr::trie::Tries;

pub mod trie;

/// The fixed interface shared by every generated function-object adapter.
///
/// Native function bodies retain their source arity. The adapter normalizes
/// a JavaScript call's argument array before calling such a body.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Repr {
    /// JavaScript values at all heap-visible boundaries.
    pub(crate) value: Type,
    pub(crate) object: Signature,
    pub(crate) number: Signature,
    pub(crate) boolean: Signature,
    pub(crate) function: Signature,
    /// Internal accessor descriptor. Ordinary property values remain direct
    /// `anyref`s; only accessor properties use this tagged representation.
    pub(crate) descriptor: Signature,
    /// Every property location (a trie bucket or a generated shape field)
    /// stores a `slot` rather than a bare value. `slot.value` holds exactly
    /// what a bare value slot held before (a data value, or a `descriptor`
    /// ref for an accessor); `slot.flags` is a bitset of
    /// `writable | enumerable << 1 | configurable << 2`.
    pub(crate) slot: Signature,
    pub(crate) trie: Signature,
    /// WTF-8 source bytes for a JavaScript string.
    pub(crate) utf8: Signature,
    /// Lazily materialized UTF-16 code units used by JavaScript indexing.
    pub(crate) utf16: Signature,
    pub(crate) string: Signature,
    pub(crate) arguments: Signature,
    /// Packed/unboxed native storage for Number-backed typed arrays.  The
    /// JavaScript object header carries these references as `anyref`, then
    /// typed-array lowering refines to the concrete signature before a load
    /// or store.
    pub(crate) typed_i8: Signature,
    pub(crate) typed_i16: Signature,
    pub(crate) typed_i32: Signature,
    pub(crate) typed_f32: Signature,
    pub(crate) typed_f64: Signature,
    pub(crate) adapter: Signature,
    /// Tagged union a mixed-return core returns: `tag` selects which of
    /// `r` (a boxed value), `i` (a raw i32 from a boolean or an integer
    /// computation), and `f` (a raw f64) is live. Only functions whose
    /// returns provably produce more than one representation get this ABI;
    /// single-kind cores keep raw f64/i32 and everything else stays boxed.
    pub(crate) multi: Signature,
}

/// The supported Number-backed typed-array constructors.  The discriminant
/// is stored in the common object header, keeping property dispatch at one
/// seam while the concrete WasmGC array remains unboxed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TypedArrayKind {
    Int8,
    Uint8,
    Uint8Clamped,
    Int16,
    Uint16,
    Int32,
    Uint32,
    Float32,
    Float64,
}

impl TypedArrayKind {
    pub(crate) const ALL: [Self; 9] = [
        Self::Int8,
        Self::Uint8,
        Self::Uint8Clamped,
        Self::Int16,
        Self::Uint16,
        Self::Int32,
        Self::Uint32,
        Self::Float32,
        Self::Float64,
    ];

    pub(crate) const fn code(self) -> i32 {
        match self {
            Self::Int8 => 0,
            Self::Uint8 => 1,
            Self::Uint8Clamped => 2,
            Self::Int16 => 3,
            Self::Uint16 => 4,
            Self::Int32 => 5,
            Self::Uint32 => 6,
            Self::Float32 => 7,
            Self::Float64 => 8,
        }
    }

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Int8 => "Int8Array",
            Self::Uint8 => "Uint8Array",
            Self::Uint8Clamped => "Uint8ClampedArray",
            Self::Int16 => "Int16Array",
            Self::Uint16 => "Uint16Array",
            Self::Int32 => "Int32Array",
            Self::Uint32 => "Uint32Array",
            Self::Float32 => "Float32Array",
            Self::Float64 => "Float64Array",
        }
    }

    /// Look up a kind by constructor name. Used by the provable fast path
    /// for `new Uint8Array(...)`-style call sites.
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        match name {
            "Int8Array" => Some(Self::Int8),
            "Uint8Array" => Some(Self::Uint8),
            "Uint8ClampedArray" => Some(Self::Uint8Clamped),
            "Int16Array" => Some(Self::Int16),
            "Uint16Array" => Some(Self::Uint16),
            "Int32Array" => Some(Self::Int32),
            "Uint32Array" => Some(Self::Uint32),
            "Float32Array" => Some(Self::Float32),
            "Float64Array" => Some(Self::Float64),
            _ => None,
        }
    }

    pub(crate) const fn bytes_per_element(self) -> i32 {
        match self {
            Self::Int8 | Self::Uint8 | Self::Uint8Clamped => 1,
            Self::Int16 | Self::Uint16 => 2,
            Self::Int32 | Self::Uint32 | Self::Float32 => 4,
            Self::Float64 => 8,
        }
    }
}

impl Repr {
    pub(crate) fn new(module: &mut Module) -> Self {
        let value = Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Any,
            nullable: true,
        });

        // Forward-declare `slot` so the trie's bucket type and the object
        // header below can both reference it before its fields are filled
        // in, mirroring the `object`/`function`/`descriptor` pattern.
        let slot = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        let slot_ty = ref_sig(slot);

        // Generic property tries use nullable `anyref` child links so nodes
        // can dispatch to a generated shape or continue through the trie
        // without requiring a recursive Wasm type group. The terminal slot
        // (field 0) holds a `slot`, not a bare value, so
        // writable/enumerable/configurable survive alongside the stored
        // value.
        let mut tries = Tries::default();
        let trie = tries.get(module, slot_ty, value);
        // GC type references outside a recursive group must point backward.
        // Allocate the element component before the object header that owns
        // it.
        let arguments = module.signatures.push(SignatureData::Array {
            ty: field(value),
            shared: false,
        });
        let utf8 = module.signatures.push(SignatureData::Array {
            ty: WithMutablility {
                value: StorageType::I8,
                mutable: true,
            },
            shared: false,
        });
        let utf16 = module.signatures.push(SignatureData::Array {
            ty: WithMutablility {
                // Keep cached code units in ordinary i32 storage.  WasmGC
                // exposes packed i16 array writes through a distinct stack
                // representation, whereas JavaScript's UTF-16 operations
                // use widened code units throughout.
                value: StorageType::Val(Type::I32),
                mutable: true,
            },
            shared: false,
        });
        // Typed arrays use actual WasmGC array element storage rather than
        // the boxed `arguments` array used by ordinary JavaScript arrays.
        // i8/i16 deliberately remain packed: signed/unsigned reads select
        // the appropriate WasmGC load instruction at the typed-array seam.
        let typed_i8 = module.signatures.push(SignatureData::Array {
            ty: WithMutablility {
                value: StorageType::I8,
                mutable: true,
            },
            shared: false,
        });
        let typed_i16 = module.signatures.push(SignatureData::Array {
            ty: WithMutablility {
                value: StorageType::I16,
                mutable: true,
            },
            shared: false,
        });
        let typed_i32 = module.signatures.push(SignatureData::Array {
            ty: field(Type::I32),
            shared: false,
        });
        let typed_f32 = module.signatures.push(SignatureData::Array {
            ty: field(Type::F32),
            shared: false,
        });
        let typed_f64 = module.signatures.push(SignatureData::Array {
            ty: field(Type::F64),
            shared: false,
        });
        let string = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        let object = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        let number = module.signatures.push(SignatureData::Struct {
            fields: vec![field(Type::F64)],
            shared: false,
        });
        let boolean = module.signatures.push(SignatureData::Struct {
            fields: vec![field(Type::I32)],
            shared: false,
        });
        let multi = module.signatures.push(SignatureData::Struct {
            fields: vec![
                field(Type::I32),
                field(value),
                field(Type::I32),
                field(Type::F64),
            ],
            shared: false,
        });
        let adapter = module.signatures.push(SignatureData::Func {
            params: vec![ref_sig(object), value, ref_sig(arguments)],
            returns: vec![value],
            shared: false,
        });
        let function = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        let descriptor = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });

        module.signatures[object] = SignatureData::Struct {
            // Every ordinary object has the same header.  `elements` being
            // non-null is the sole distinction between an ordinary array and
            // an object. Typed arrays keep `elements` null and use the final
            // four fields, so `Array.isArray` remains false for them.
            // The property root is either a generic trie or a generated
            // shape instance. Both are carried as `anyref` so a lookup can
            // refine with `ref.test` before continuing through a trie.
            fields: vec![
                field(value),
                field(ref_sig(arguments)),
                field(value),
                field(value),
                field(Type::I32),
                field(Type::I32),
                field(Type::I32),
            ],
            shared: false,
        };
        module.signatures[string] = SignatureData::Struct {
            fields: vec![field(ref_sig(utf8)), field(ref_sig(utf16))],
            shared: false,
        };
        module.signatures[function] = SignatureData::Struct {
            fields: vec![
                field(value),
                field(ref_sig(arguments)),
                field(value),
                field(ref_sig(adapter)),
                field(ref_sig(object)),
                field(value),
                field(Type::I32),
                // Primordial tag: 0 for user functions, a positive [`crate::
                // conv`] tag constant for native primordials with a fast
                // core. `ref.eq` cannot compare function references (they are
                // outside the `eq` hierarchy), so the guarded fast path
                // identifies a primordial callee by this tag instead.
                field(Type::I32),
            ],
            shared: false,
        };
        module.signatures[descriptor] = SignatureData::Struct {
            // Getter and setter entries are nullable function objects. A
            // data property has no descriptor at all, so `ref.test` is the
            // single runtime tag check that separates the two cases.
            fields: vec![field(value), field(value)],
            shared: false,
        };
        module.signatures[slot] = SignatureData::Struct {
            fields: vec![field(value), field(Type::I32)],
            shared: false,
        };

        Self {
            value,
            object,
            number,
            boolean,
            function,
            descriptor,
            slot,
            trie,
            utf8,
            utf16,
            string,
            arguments,
            typed_i8,
            typed_i16,
            typed_i32,
            typed_f32,
            typed_f64,
            adapter,
            multi,
        }
    }

    pub(crate) fn multi_ty(self) -> Type {
        ref_sig(self.multi)
    }

    pub(crate) fn object_ty(self) -> Type {
        ref_sig(self.object)
    }

    /// For `RefTest`: `ref.test` against a *nullable* type matches `null`
    /// too, so a plain `RefTest{ty: object_ty()}` cannot distinguish "is an
    /// object" from "is null" — combine both checks in one test with this
    /// non-null variant instead (same pattern as `descriptor_non_null_ty`).
    pub(crate) fn object_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.object,
            },
            nullable: false,
        })
    }

    pub(crate) fn number_ty(self) -> Type {
        ref_sig(self.number)
    }

    pub(crate) fn number_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.number,
            },
            nullable: false,
        })
    }

    pub(crate) fn boolean_ty(self) -> Type {
        ref_sig(self.boolean)
    }

    pub(crate) fn boolean_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.boolean,
            },
            nullable: false,
        })
    }

    pub(crate) fn string_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.string,
            },
            nullable: false,
        })
    }

    /// The abstract `eqref` heap type. `ref.eq` requires both operands to be
    /// in the `eq` hierarchy — a plain `anyref` operand does not validate — so
    /// every reference comparison casts through this type first. Every value
    /// this backend produces is a struct, an array, or an i31, all of which
    /// are `eqref` subtypes, so the cast cannot trap in practice.
    pub(crate) fn eq_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Eq,
            nullable: true,
        })
    }

    /// The non-nullable `i31ref` heap type. JS `null` is represented as an
    /// i31 sentinel (see [`JS_NULL_SENTINEL`]), so `ref.test` against this
    /// type is the single runtime check that distinguishes JS `null` from
    /// `undefined` (the null `anyref`).
    pub(crate) fn i31_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::I31,
            nullable: false,
        })
    }

    pub(crate) fn function_ty(self) -> Type {
        ref_sig(self.function)
    }

    pub(crate) fn function_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.function,
            },
            nullable: false,
        })
    }

    pub(crate) fn descriptor_ty(self) -> Type {
        ref_sig(self.descriptor)
    }

    pub(crate) fn descriptor_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.descriptor,
            },
            nullable: false,
        })
    }

    pub(crate) fn trie_ty(self) -> Type {
        ref_sig(self.trie)
    }

    pub(crate) fn slot_ty(self) -> Type {
        ref_sig(self.slot)
    }

    pub(crate) fn slot_non_null_ty(self) -> Type {
        Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig {
                sig_index: self.slot,
            },
            nullable: false,
        })
    }

    pub(crate) fn utf8_ty(self) -> Type {
        ref_sig(self.utf8)
    }

    pub(crate) fn utf16_ty(self) -> Type {
        ref_sig(self.utf16)
    }

    pub(crate) fn string_ty(self) -> Type {
        ref_sig(self.string)
    }

    pub(crate) fn arguments_ty(self) -> Type {
        ref_sig(self.arguments)
    }
}

/// Bit layout of a `slot`'s `flags` field. Ordinary object-literal and
/// `obj.x = y` writes use [`SLOT_FLAGS_DEFAULT`]; `Object.defineProperty`
/// and friends compose the individual bits from a user-supplied descriptor.
pub(crate) const SLOT_WRITABLE: i32 = 1 << 0;
pub(crate) const SLOT_ENUMERABLE: i32 = 1 << 1;
pub(crate) const SLOT_CONFIGURABLE: i32 = 1 << 2;
pub(crate) const SLOT_FLAGS_DEFAULT: i32 =
    SLOT_WRITABLE | SLOT_ENUMERABLE | SLOT_CONFIGURABLE;

/// Index of the primordial-tag field within the `function` struct (see the
/// field's declaration in [`Repr::new`]).
pub(crate) const FUNCTION_FIELD_TAG: usize = 7;

/// The i31 payload representing JS `null`. The null `anyref` is reserved for
/// `undefined`, so `null` needs a distinct representation that is cheap,
/// identity-comparable via `ref.eq`, and detectable with `ref.test` against
/// [`Repr::i31_non_null_ty`]. An i31 sentinel provides all three: i31
/// equality is by value, and this backend never materializes any other i31
/// (numbers are boxed structs), so every `ref.test (ref i31)` hit is a JS
/// `null`. Nothing else may ever emit `RefI31` without breaking this
/// invariant — pick a value well away from any future small-int range.
pub(crate) const JS_NULL_SENTINEL: i32 = 0x4000_0000;

pub(crate) fn field(value: Type) -> WithMutablility<StorageType> {
    WithMutablility {
        value: StorageType::Val(value),
        mutable: true,
    }
}

pub(crate) fn ref_sig(sig_index: Signature) -> Type {
    Type::Heap(WithNullable {
        value: portal_pc_waffle::HeapType::Sig { sig_index },
        nullable: true,
    })
}

/// An actionable lowering error. `convert` never uses `todo!` for user IR:
/// the source span is preserved in the diagnostic text whenever it exists.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConvertError {
    /// A stable, user-facing description of the unsupported construct.
    pub message: String,
    /// Debug-formatted source span when the lowering was handed one.
    pub location: Option<String>,
}

impl ConvertError {
    pub(crate) fn unsupported(feature: impl fmt::Display, span: impl fmt::Debug) -> Self {
        let location = format!("{span:?}");
        Self {
            message: format!("unsupported in portal-jsc-waffle: {feature} at {location}"),
            location: Some(location),
        }
    }

    pub(crate) fn invalid(detail: impl fmt::Display) -> Self {
        Self {
            message: format!("invalid portal-jsc-waffle input: {detail}"),
            location: None,
        }
    }
}

impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl Error for ConvertError {}
