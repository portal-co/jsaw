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
    pub(crate) trie: Signature,
    /// WTF-8 source bytes for a JavaScript string.
    pub(crate) utf8: Signature,
    /// Lazily materialized UTF-16 code units used by JavaScript indexing.
    pub(crate) utf16: Signature,
    pub(crate) string: Signature,
    pub(crate) arguments: Signature,
    pub(crate) adapter: Signature,
}

impl Repr {
    pub(crate) fn new(module: &mut Module) -> Self {
        let value = Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Any,
            nullable: true,
        });

        // Generic property tries use nullable `anyref` child links so nodes
        // can dispatch to a generated shape or continue through the trie
        // without requiring a recursive Wasm type group.
        let mut tries = Tries::default();
        let trie = tries.get(module, value);
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
        let adapter = module.signatures.push(SignatureData::Func {
            params: vec![ref_sig(object), value, ref_sig(arguments)],
            returns: vec![value],
            shared: false,
        });
        let function = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });

        module.signatures[object] = SignatureData::Struct {
            // Every ordinary object has the same header.  `elements` being
            // non-null is the sole distinction between an object and an
            // array, which is exactly what a future `Array.isArray` needs.
            // The property root is either a generic trie or a generated
            // shape instance. Both are carried as `anyref` so a lookup can
            // refine with `ref.test` before continuing through a trie.
            fields: vec![field(value), field(ref_sig(arguments)), field(value)],
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
            ],
            shared: false,
        };

        Self {
            value,
            object,
            number,
            boolean,
            function,
            trie,
            utf8,
            utf16,
            string,
            arguments,
            adapter,
        }
    }

    pub(crate) fn object_ty(self) -> Type {
        ref_sig(self.object)
    }

    pub(crate) fn number_ty(self) -> Type {
        ref_sig(self.number)
    }

    pub(crate) fn boolean_ty(self) -> Type {
        ref_sig(self.boolean)
    }

    pub(crate) fn function_ty(self) -> Type {
        ref_sig(self.function)
    }

    pub(crate) fn trie_ty(self) -> Type {
        ref_sig(self.trie)
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
