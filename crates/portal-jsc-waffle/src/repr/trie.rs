use std::collections::BTreeMap;

use portal_pc_waffle::{Module, Signature, SignatureData, StorageType, Type, WithMutablility};

/// Allocate the byte-addressed property trie used by objects and function
/// objects. Edges are nullable `anyref`: consumers cast a non-null edge back
/// to this signature before following it, avoiding a recursive Wasm type
/// group while retaining a concrete terminal representation.
fn new_trie(module: &mut Module, value: Type) -> Signature {
    let signature = module.signatures.push(SignatureData::Struct {
        fields: vec![],
        shared: false,
    });
    module.signatures[signature] = SignatureData::Struct {
        fields: std::iter::once(WithMutablility {
            value: StorageType::Val(value),
            mutable: true,
        })
        .chain((0..=u8::MAX).map(|_| WithMutablility {
            value: StorageType::Val(value),
            mutable: true,
        }))
        .collect(),
        shared: false,
    };
    signature
}

#[derive(Default)]
pub(crate) struct Tries {
    map: BTreeMap<Type, Signature>,
}

impl Tries {
    pub(crate) fn get(&mut self, module: &mut Module, value: Type) -> Signature {
        if let Some(signature) = self.map.get(&value) {
            return *signature;
        }
        let signature = new_trie(module, value);
        self.map.insert(value, signature);
        signature
    }
}
