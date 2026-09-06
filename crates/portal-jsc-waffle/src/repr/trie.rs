use std::collections::BTreeMap;

use portal_pc_waffle::{Module, Signature, SignatureData, StorageType, Type, WithMutablility};

/// Allocate the byte-addressed property trie used by objects and function
/// objects. Edges (fields `1..=256`) are nullable `anyref`: consumers cast a
/// non-null edge back to this signature before following it, avoiding a
/// recursive Wasm type group while retaining a concrete terminal
/// representation. Field `0` is the terminal slot for a key whose bytes are
/// fully consumed at this node, so it carries `slot_ty` (value + attribute
/// flags) rather than the generic edge type.
fn new_trie(module: &mut Module, slot_ty: Type, child_ty: Type) -> Signature {
    let signature = module.signatures.push(SignatureData::Struct {
        fields: vec![],
        shared: false,
    });
    module.signatures[signature] = SignatureData::Struct {
        fields: std::iter::once(WithMutablility {
            value: StorageType::Val(slot_ty),
            mutable: true,
        })
        .chain((0..=u8::MAX).map(|_| WithMutablility {
            value: StorageType::Val(child_ty),
            mutable: true,
        }))
        .collect(),
        shared: false,
    };
    signature
}

#[derive(Default)]
pub(crate) struct Tries {
    map: BTreeMap<(Type, Type), Signature>,
}

impl Tries {
    pub(crate) fn get(&mut self, module: &mut Module, slot_ty: Type, child_ty: Type) -> Signature {
        if let Some(signature) = self.map.get(&(slot_ty, child_ty)) {
            return *signature;
        }
        let signature = new_trie(module, slot_ty, child_ty);
        self.map.insert((slot_ty, child_ty), signature);
        signature
    }
}
