use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    mem::take,
};

use portal_jsc_swc_ssa::SFunc;
use portal_pc_waffle::{Module, Type, WithNullable};

pub fn convert<'a>(root: &'a SFunc, module: &mut Module) {
    let object = module
        .signatures
        .push(portal_pc_waffle::SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
    let mut workqueue = VecDeque::new();
    workqueue.push_back((root, root.entry));
    let mut fcache: BTreeMap<
        usize,
        (
            portal_pc_waffle::Func,
            BTreeMap<portal_jsc_swc_ssa::SBlockId, portal_pc_waffle::Block>,
        ),
    > = BTreeMap::new();
    macro_rules! fcache {
        ($sfunc:expr) => {
            match $sfunc {
                //HACK: pointers to SFunc are stable, so we can use them as keys in the cache
                sfunc => fcache
                    .entry(sfunc as *const SFunc as usize)
                    .or_insert_with(|| {
                        let sig = module
                            .signatures
                            .push(portal_pc_waffle::SignatureData::Func {
                                params: sfunc.cfg.blocks[sfunc.entry]
                                    .params
                                    .iter()
                                    .map(|_| {
                                        Type::Heap(WithNullable {
                                            nullable: true,
                                            value: portal_pc_waffle::HeapType::Sig {
                                                sig_index: object,
                                            },
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
                        (func, map)
                    }),
            }
        };
    }
    while let Some((sfunc, sblock)) = workqueue.pop_front() {
        let (func, cache) = fcache!(sfunc);
        let func = *func;
        let mut block = *cache.entry(sblock).or_insert_with(|| {
            let func = module
                .funcs
                .get_mut(func)
                .and_then(|a| a.body_mut())
                .unwrap();
            let b = func.add_block();
            for p in &sfunc.cfg.blocks[sblock].params {
                func.add_blockparam(
                    b,
                    Type::Heap(WithNullable {
                        nullable: true,
                        value: portal_pc_waffle::HeapType::Sig { sig_index: object },
                    }),
                );
            }
            b
        });
        let mut vals = sfunc.cfg.blocks[sblock]
            .params
            .iter()
            .map(|a| a.0)
            .zip(
                module.funcs[func].body().unwrap().blocks[block]
                    .params
                    .iter()
                    .map(|a| a.1),
            )
            .collect::<BTreeMap<_, _>>();
        let mut blkset = [(block, vals)].into_iter().collect::<BTreeMap<_, _>>();
        for stmt in sfunc.cfg.blocks[sblock].stmts.iter().cloned() {
            for (mut block, mut vals) in take(&mut blkset) {
                let val: portal_pc_waffle::Value = match &sfunc.cfg.values[stmt].value {
                    _ => todo!("unsupported statement: {:?}", sfunc.cfg.values[stmt].value),
                };
                vals.insert(stmt, val);
                blkset.insert(block, vals);
            }
        }
        for (block,vals) in blkset{
            let terminator = match &sfunc.cfg.blocks[sblock].postcedent.term{
                _ => todo!("unsupported terminator")
            };
            module.funcs[func].body_mut().unwrap().set_terminator(block, terminator);
        }
    }
}
