use portal_pc_waffle::Terminator;

use super::*;

fn new_trie(module: &mut Module, val: Type) -> Signature {
    let sig = module.signatures.push(SignatureData::None);
    module.signatures[sig] = SignatureData::Struct {
        fields: [WithMutablility {
            mutable: true,
            value: StorageType::Val(val),
        }]
        .into_iter()
        .chain((0..=0xffu8).map(|a| WithMutablility {
            value: StorageType::Val(Type::Heap(WithNullable {
                nullable: true,
                value: portal_pc_waffle::HeapType::Sig { sig_index: sig },
            })),
            mutable: true,
        }))
        .collect(),
        shared: false,
    };
    return sig;
}
fn new_trie_getter(module: &mut Module, trie: Signature) -> Func {
    let sig = module.signatures.push(SignatureData::Func {
        params: [
            Type::Heap(WithNullable {
                value: portal_pc_waffle::HeapType::Sig { sig_index: trie },
                nullable: true,
            }),
            Type::I32,
        ]
        .into_iter()
        .collect(),
        returns: [Type::Heap(WithNullable {
            value: portal_pc_waffle::HeapType::Sig { sig_index: trie },
            nullable: true,
        })]
        .into_iter()
        .collect(),
        shared: false,
    });
    let mut func = FunctionBody::new(module, sig);
    let obj = func.blocks[func.entry].params[0].1;
    let idx = func.blocks[func.entry].params[1].1;
    let k = func.add_block();
    let f = func.add_block();
    func.set_terminator(
        f,
        Terminator::Return {
            values: [obj].into_iter().collect(),
        },
    );
    let c = func.add_op(func.entry, Operator::RefIsNull, &[obj], &[Type::I32]);
    func.set_terminator(
        func.entry,
        Terminator::CondBr {
            cond: c,
            if_true: BlockTarget {
                block: f,
                args: Default::default(),
            },
            if_false: BlockTarget {
                block: k,
                args: Default::default(),
            },
        },
    );
    let targets = (0..=0xffu8)
        .map(|a| {
            let k = func.add_block();
            let val = func.add_op(
                k,
                Operator::StructGet {
                    sig: trie,
                    idx: (a as usize) + 1,
                },
                &[obj],
                &[Type::Heap(WithNullable {
                    value: portal_pc_waffle::HeapType::Sig { sig_index: trie },
                    nullable: true,
                })],
            );
            func.set_terminator(
                k,
                portal_pc_waffle::Terminator::Return {
                    values: [val].into_iter().collect(),
                },
            );
            BlockTarget {
                block: k,
                args: Default::default(),
            }
        })
        .collect();
    let t = portal_pc_waffle::Terminator::Select {
        value: idx,
        targets,
        default: BlockTarget {
            block: func.entry,
            args: [obj, idx].into_iter().collect(),
        },
    };
    func.set_terminator(k, t);
    return module.funcs.push(portal_pc_waffle::FuncDecl::Body(
        sig,
        format!("get_trie"),
        func,
    ));
}
fn new_trie_setter(module: &mut Module, trie: Signature) -> Func {
    let sig = module.signatures.push(SignatureData::Func {
        params: [
            Type::Heap(WithNullable {
                value: portal_pc_waffle::HeapType::Sig { sig_index: trie },
                nullable: true,
            }),
            Type::I32,
            Type::Heap(WithNullable {
                value: portal_pc_waffle::HeapType::Sig { sig_index: trie },
                nullable: true,
            }),
        ]
        .into_iter()
        .collect(),
        returns: [].into_iter().collect(),
        shared: false,
    });
    let mut func = FunctionBody::new(module, sig);
    let obj = func.blocks[func.entry].params[0].1;
    let idx = func.blocks[func.entry].params[1].1;
    let val = func.blocks[func.entry].params[2].1;
    let targets = (0..=0xffu8)
        .map(|a| {
            let k = func.add_block();
            let val = func.add_op(
                k,
                Operator::StructSet {
                    sig: trie,
                    idx: (a as usize) + 1,
                },
                &[obj, val],
                &[],
            );
            func.set_terminator(
                k,
                portal_pc_waffle::Terminator::Return {
                    values: [].into_iter().collect(),
                },
            );
            BlockTarget {
                block: k,
                args: Default::default(),
            }
        })
        .collect();
    let t = portal_pc_waffle::Terminator::Select {
        value: idx,
        targets,
        default: BlockTarget {
            block: func.entry,
            args: [obj, idx, val].into_iter().collect(),
        },
    };
    func.set_terminator(func.entry, t);
    return module.funcs.push(portal_pc_waffle::FuncDecl::Body(
        sig,
        format!("set_trie"),
        func,
    ));
}
#[derive(Default)]
pub(crate) struct Tries {
    map: BTreeMap<Type, Signature>,
    getters: BTreeMap<Type, Func>,
    setters: BTreeMap<Type, Func>,
}
impl Tries {
    pub(crate) fn get(&mut self, module: &mut Module, val: Type) -> Signature {
        if let Some(sig) = self.map.get(&val) {
            return *sig;
        }
        let sig = new_trie(module, val);
        self.map.insert(val, sig);
        return sig;
    }
    pub(crate) fn get_getter(&mut self, module: &mut Module, val: Type) -> Func {
        if let Some(func) = self.getters.get(&val) {
            return *func;
        }
        let sig = self.get(module, val);
        let func = new_trie_getter(module, sig);
        self.getters.insert(val, func);
        return func;
    }
    pub(crate) fn get_setter(&mut self, module: &mut Module, val: Type) -> Func {
        if let Some(func) = self.setters.get(&val) {
            return *func;
        }
        let sig = self.get(module, val);
        let func = new_trie_setter(module, sig);
        self.setters.insert(val, func);
        return func;
    }
}
