use std::collections::{BTreeMap, HashMap, VecDeque};

use portal_jsc_swc_ssa::SFunc;
use portal_pc_waffle::{Module, Type, WithNullable};

pub fn convert<'a>(root: &'a SFunc, module: &mut Module) {
    let object = module
        .signatures
        .push(portal_pc_waffle::SignatureData::Struct { fields: vec![], shared: false });
    let mut workqueue = VecDeque::new();
    workqueue.push_back((root, root.entry));
    let mut fcache = BTreeMap::new();
    while let Some((sfunc, block)) = workqueue.pop_front() {
        //HACK: pointers to SFunc are stable, so we can use them as keys in the cache
        let (func,cache) = fcache.entry(sfunc as *const SFunc as usize).or_insert_with(|| {
            let sig = module
                .signatures
                .push(portal_pc_waffle::SignatureData::Func {
                    params: sfunc.cfg.blocks[sfunc.entry]
                        .params
                        .iter()
                        .map(|_| {
                            Type::Heap(WithNullable {
                                nullable: true,
                                value: portal_pc_waffle::HeapType::Sig { sig_index: object },
                            })
                        })
                        .collect(),
                    returns: vec![Type::Heap(WithNullable {
                        nullable: true,
                        value: portal_pc_waffle::HeapType::Sig { sig_index: object },
                    })],
                    shared: false,
                });
            let func = portal_pc_waffle::FunctionBody::new(module, sig);
            let mut map = BTreeMap::new();
            map.insert(sfunc.entry, func.entry);
            let func = module.funcs.push(portal_pc_waffle::FuncDecl::Body(
                sig,
                format!("func_{}", module.funcs.len()),
                func,
            ));
            (func,map)
        });
        let func = *func;
        
    }
}
