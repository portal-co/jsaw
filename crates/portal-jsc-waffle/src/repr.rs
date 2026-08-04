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
    pub(crate) arguments: Signature,
    pub(crate) adapter: Signature,
}

impl Repr {
    pub(crate) fn new(module: &mut Module) -> Self {
        let value = Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Any,
            nullable: true,
        });

        // Child trie links are stored as `anyref` and cast at the use site.
        // This keeps the representation valid even for Waffle backends that
        // do not place recursive struct definitions in a rec group.
        let mut tries = Tries::default();
        let trie = tries.get(module, value);
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
        let arguments = module.signatures.push(SignatureData::Array {
            ty: field(value),
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
            fields: vec![field(ref_sig(trie))],
            shared: false,
        };
        module.signatures[function] = SignatureData::Struct {
            fields: vec![
                field(ref_sig(trie)),
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
