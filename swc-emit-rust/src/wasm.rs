use std::{
    collections::{BTreeMap, BTreeSet},
    env::args,
    iter::once,
};

use anyhow::Context;
use id_arena::Id;
use swc_ecma_ast::{BinaryOp, Id as Ident, UnaryOp};
use swc_ssa::{SBlock, SFunc, STarget};
use waffle::{
    util::new_sig, Block, BlockTarget, ExportKind, Func, FuncDecl, FunctionBody, Memory, MemoryArg,
    Module, Operator, Type, Value,
};
pub fn get_export_f(module: &Module, n: &str) -> anyhow::Result<Func> {
    Ok(module
        .exports
        .iter()
        .find_map(|a| {
            if a.name == n {
                match &a.kind {
                    ExportKind::Func(m) => Some(*m),
                    _ => None,
                }
            } else {
                None
            }
        })
        .context("in getting the func")?)
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Tag {
    Number,
    U32 { smallness: usize },
    U64 { smallness: usize },
}
impl Tag {
    // pub fn parent(&self) -> Option<Tag> {
    //     match self {
    //         Tag::Number => None,
    //         Tag::U32{s} => Some(Tag::Number),
    //         Tag::U64 => None,
    //     }
    // }
    pub fn deopt(
        &self,
        fb: &mut FunctionBody,
        k: Block,
        v: Value,
        module: &Module,
    ) -> anyhow::Result<(Option<Tag>, Value)> {
        Ok(match self {
            Tag::Number => {
                let f = get_export_f(module, "jrl_object_new_float")?;
                (
                    None,
                    fb.add_op(k, Operator::Call { function_index: f }, &[v], &[Type::F64]),
                )
            }
            Tag::U32 { smallness } => {
                if *smallness == 0 {
                    (
                        Some(Tag::Number),
                        fb.add_op(k, Operator::F64ConvertI32S, &[v], &[Type::F64]),
                    )
                } else {
                    (
                        Some(Tag::U32 {
                            smallness: *smallness - 1,
                        }),
                        v,
                    )
                }
            }
            Tag::U64 { smallness } => {
                if *smallness == 0 {
                    let f = get_export_f(module, "jrl_object_new_u64")?;
                    (
                        None,
                        fb.add_op(k, Operator::Call { function_index: f }, &[v], &[Type::F64]),
                    )
                } else {
                    (
                        Some(Tag::U64 {
                            smallness: *smallness - 1,
                        }),
                        v,
                    )
                }
            }
        })
    }
}
pub struct BodyInstance {
    pub all: BTreeMap<Id<SBlock>, BTreeMap<Vec<Option<Tag>>, Block>>,
    pub undef: Value,
}
fn full_deopt(
    fb: &mut FunctionBody,
    k2: Block,
    mut a: Value,
    tag: Option<Tag>,
    module: &Module,
) -> anyhow::Result<Value> {
    if tag == None {
        let f = get_export_f(module, "jrl_object_clone")?;
        return Ok(fb.add_op(k2, Operator::Call { function_index: f }, &[a], &[Type::F64]));
    }
    let mut b = tag;
    while let Some(c) = b {
        (b, a) = c.deopt(fb, k2, a, &module)?;
    }
    return Ok(a);
}
fn merge_deopt(
    fb: &mut FunctionBody,
    k2: Block,
    mut a: Value,
    mut tag: Option<Tag>,
    mut b: Value,
    mut tag2: Option<Tag>,
    module: &Module,
) -> anyhow::Result<(Value, Value, Option<Tag>)> {
    let mut b2 = false;
    while tag != tag2 {
        if {
            b2 = !b2;
            b2
        } {
            if let Some(t) = tag {
                (tag, a) = t.deopt(fb, k2, a, module)?;
            }
        } else {
            if let Some(t) = tag2 {
                (tag2, b) = t.deopt(fb, k2, b, module)?;
            }
        }
    }
    return Ok((a, b, tag));
}
fn do_rsc(
    fb: &mut FunctionBody,
    mut k2: Block,
    mut a: Value,
    c: Option<&BlockTarget>,
    drops: &BTreeSet<Value>,
    module: &Module,
) -> anyhow::Result<(Block, Value)> {
    let f = get_export_f(module, "jrl_result_ok")?;
    let b = fb.add_op(k2, Operator::Call { function_index: f }, &[a], &[Type::I32]);
    let o = a;
    let f = get_export_f(module, "jrl_result_value")?;
    a = fb.add_op(k2, Operator::Call { function_index: f }, &[a], &[Type::F64]);
    let k3 = fb.add_block();
    let x = fb.add_block();
    fb.set_terminator(
        k2,
        waffle::Terminator::CondBr {
            cond: b,
            if_true: BlockTarget {
                block: k3,
                args: vec![],
            },
            if_false: BlockTarget {
                block: x,
                args: vec![],
            },
        },
    );
    k2 = k3;
    match c {
        Some(t) => {
            let t = BlockTarget {
                block: t.block,
                args: once(a).chain(t.args.iter().cloned()).collect(),
            };
            let f = get_export_f(module, "jrl_object_drop")?;
            for d in drops.iter().cloned().filter(|a| !t.args.contains(a)) {
                fb.add_op(x, Operator::Call { function_index: f }, &[d], &[]);
            }
            fb.set_terminator(x, waffle::Terminator::Br { target: t });
        }
        None => {
            fb.set_terminator(x, waffle::Terminator::Return { values: vec![o] });
        }
    };
    return Ok((k2, a));
}
impl BodyInstance {
    pub fn go(
        &mut self,
        module: &mut Module,
        o: &impl Fn(&Module, u64, bool) -> Operator,
        f: &SFunc,
        fb: &mut FunctionBody,
        k: Id<SBlock>,
        tags: Vec<Option<Tag>>,
        vars: &BTreeMap<Ident, Value>,
        memory: Memory,
    ) -> anyhow::Result<Block> {
        loop {
            if let Some(a) = self.all.get(&k).and_then(|x| x.get(&tags)) {
                return Ok(*a);
            }
            let mut k2 = fb.add_block();
            self.all.entry(k).or_default().insert(tags.clone(), k2);
            let uf = get_export_f(module, "jrl_undef")?;
            let t = if module.memories[memory].memory64 {
                Type::I64
            } else {
                Type::I32
            };
            let mut state = f.cfg.blocks[k]
                .params
                .iter()
                .map(|a| a.0)
                .zip(tags.iter().cloned())
                .map(|(a, b)| {
                    (
                        a,
                        (
                            match b {
                                Some(Tag::U32 { .. }) => fb.add_blockparam(k2, Type::I32),
                                Some(Tag::U64 { .. }) => fb.add_blockparam(k2, Type::I64),
                                _ => fb.add_blockparam(k2, Type::F64),
                            },
                            b,
                        ),
                    )
                })
                .collect::<BTreeMap<_, _>>();
            let catchk = match &f.cfg.blocks[k].postcedent.catch {
                swc_ssa::SCatch::Throw => None,
                swc_ssa::SCatch::Just { target } => {
                    let mut tags2 = vec![None];
                    let args = target
                        .args
                        .iter()
                        .filter_map(|v| state.get(v))
                        .map(|(a, b)| {
                            tags2.push(b.clone());
                            *a
                        })
                        .collect();
                    Some(BlockTarget {
                        block: self.go(module, o, f, fb, target.block, tags2, vars, memory)?,
                        args,
                    })
                }
            };
            let mut drops = BTreeSet::new();
            for i in f.cfg.blocks[k].stmts.iter().cloned() {
                let j = match &f.cfg.values[i].0 {
                    swc_ssa::SValue::Param { block, idx, ty } => todo!(),
                    swc_ssa::SValue::Item(item) => match item {
                        swc_tac::Item::Just { id } => {
                            let (mut a, b) =
                                state.get(id).cloned().context("in getting the value")?;
                            if b == None {
                                let f = get_export_f(module, "jrl_object_clone")?;
                                a = fb.add_op(
                                    k2,
                                    Operator::Call { function_index: f },
                                    &[a],
                                    &[Type::F64],
                                );
                            }
                            Some((a, b))
                        }
                        swc_tac::Item::Bin { left, right, op } => {
                            let (left, ltag) =
                                state.get(left).cloned().context("in getting the value")?;
                            let (right, rtag) =
                                state.get(right).cloned().context("in getting the value")?;
                            fn muls(a: usize, b: usize, n: usize) -> Option<usize> {
                                n.checked_sub((n - a) + (n - b))
                            }
                            macro_rules! tag_bulk {
                                    ($ltag:ident, $rtag:ident, $op:ident ($({$name:ident |$smallness:ident, $smallness2:ident, $n:ident| => $rem:expr}),*) => $default:expr) => {
                                        paste::paste!{
                                            match ($ltag, $rtag, $op) {
                                                $(
                                                (Some(Tag::U32 { smallness: $smallness }),Some(Tag::U32{smallness: $smallness2}),BinaryOp::$name) if {
                                                    let $n = 32;
                                                    $rem
                                                }.is_some() => {
                                                    let v = fb.add_op(k2, Operator::[<I32 $name>], &[left,right], &[Type::I32]);
                                                    Some((v,Some(Tag::U32 { smallness: {
                                                        let $n = 32;
                                                        $rem
                                                    }.unwrap() })))
                                                }
                                                (Some(Tag::U64 { smallness: $smallness }),Some(Tag::U64{smallness: $smallness2}),BinaryOp::$name) if {
                                                    let $n = 64;
                                                    $rem
                                                }.is_some() => {
                                                    let v = fb.add_op(k2, Operator::[<I64 $name>], &[left,right], &[Type::I64]);
                                                    Some((v,Some(Tag::U64 { smallness: {
                                                        let $n = 64;
                                                        $rem
                                                    }.unwrap() })))
                                                }
                                                )*
                                                _ => $default,
                                            }
                                        }
                                    };
                                };
                            macro_rules! mtag_bulk {
                                    ($mtag:ident, $op:ident ($({$name:ident |$smallness:ident| => $rem:expr}),*) => $default:expr) => {
                                        paste::paste!{
                                            match ($mtag, $op) {
                                                $(
                                                (Some(Tag::U32 { smallness: $smallness }),BinaryOp::$name) if $rem.is_some() => {
                                                    let v = fb.add_op(k2, Operator::[<I32 $name>], &[left,right], &[Type::I32]);
                                                    Some((v,Some(Tag::U32 { smallness: $rem.unwrap() })))
                                                }
                                                (Some(Tag::U64 { smallness: $smallness }),BinaryOp::$name) if $rem.is_some() => {
                                                    let v = fb.add_op(k2, Operator::[<I64 $name>], &[left,right], &[Type::I64]);
                                                    Some((v,Some(Tag::U64 { smallness: $rem.unwrap() })))
                                                }
                                                )*
                                                _ => $default,
                                            }
                                        }
                                    };
                                };
                            tag_bulk!(ltag, rtag, op (
                                {Mul |s1,s2,n| => muls(s1,s2,n)}
                            ) => {
                                let (left, right, mtag) =
                                    merge_deopt(fb, k2, left, ltag, right, rtag, module)?;
                                mtag_bulk!(mtag, op (
                                    {Add |smallness| => smallness.checked_sub(1)},
                                    {Sub |smallness| => smallness.checked_sub(1)}
                                ) => {
                                    let left = full_deopt(fb, k2, left, mtag, module)?;
                                    let right = full_deopt(fb, k2, right, mtag, module)?;
                                    let f = get_export_f(
                                        module,
                                        &format!("jrl_object_BinaryOp_{op:?}"),
                                    )?;
                                    let mut a = fb.add_op(
                                        k2,
                                        Operator::Call { function_index: f },
                                        &[left, right],
                                        &[Type::F64],
                                    );
                                    (k2, a) =
                                        do_rsc(fb, k2, a, catchk.as_ref(), &drops, module)?;
                                    match op {
                                        _ => Some((a, None)),
                                    }
                                })
                            })
                        }
                        swc_tac::Item::Un { arg, op } => {
                            let (a, b) = state.get(arg).cloned().context("in getting the value")?;
                            match (b, op) {
                                (Some(Tag::Number | Tag::U32 { .. }), UnaryOp::Plus) => {
                                    Some((a, b))
                                }
                                _ => {
                                    let a = full_deopt(fb, k2, a, b, module)?;
                                    let f = get_export_f(
                                        module,
                                        &format!("jrl_object_UnaryOp_{op:?}"),
                                    )?;
                                    let mut a = fb.add_op(
                                        k2,
                                        Operator::Call { function_index: f },
                                        &[a],
                                        &[Type::F64],
                                    );
                                    (k2, a) = do_rsc(fb, k2, a, catchk.as_ref(), &drops, module)?;
                                    match op {
                                        UnaryOp::Plus => {
                                            let f =
                                                get_export_f(module, &format!("jrl_object_float"))?;
                                            let a = fb.add_op(
                                                k2,
                                                Operator::Call { function_index: f },
                                                &[a],
                                                &[Type::F64],
                                            );
                                            Some((a, Some(Tag::Number)))
                                        }
                                        _ => Some((a, None)),
                                    }
                                }
                            }
                        }
                        swc_tac::Item::Mem { obj, mem } => {
                            let (mut a, mut b) =
                                state.get(obj).cloned().context("in getting the value")?;
                            let obj = full_deopt(fb, k2, a, b, module)?;
                            let (mut a, mut b) =
                                state.get(mem).cloned().context("in getting the value")?;
                            let mem = full_deopt(fb, k2, a, b, module)?;
                            let fg = get_export_f(module, "jrl_member_get")?;
                            let v = fb.add_op(
                                k2,
                                Operator::Call { function_index: fg },
                                &[obj, mem],
                                &[Type::F64],
                            );

                            Some((v, None))
                        }
                        swc_tac::Item::Func { func } => todo!(),
                        swc_tac::Item::Lit { lit } => todo!(),
                        swc_tac::Item::Call { r#fn, member, args } => todo!(),
                        swc_tac::Item::Obj { members } => todo!(),
                        swc_tac::Item::Arr { members } => todo!(),
                        swc_tac::Item::Yield { value, delegate } => todo!(),
                        swc_tac::Item::Await { value } => todo!(),
                        swc_tac::Item::Undef => Some((self.undef, None)),
                    },
                    swc_ssa::SValue::Assign { target, val } => {
                        let (mut a, mut b) =
                            state.get(val).cloned().context("in getting the value")?;
                        let val = full_deopt(fb, k2, a, b, module)?;
                        match target {
                            swc_tac::LId::Id { id } => todo!(),
                            swc_tac::LId::Member { obj, mem } => {
                                let (mut a, mut b) =
                                    state.get(obj).cloned().context("in getting the value")?;
                                let obj = full_deopt(fb, k2, a, b, module)?;
                                let (mut a, mut b) =
                                    state.get(mem).cloned().context("in getting the value")?;

                                let mem = full_deopt(fb, k2, a, b, module)?;
                                let fg = get_export_f(module, "jrl_member_set")?;
                                fb.add_op(
                                    k2,
                                    Operator::Call { function_index: fg },
                                    &[obj, mem, val],
                                    &[],
                                );
                                None
                            }
                        }
                    }
                    swc_ssa::SValue::LoadId(i) => {
                        let lo = o(module, 0, false);
                        Some((
                            fb.add_op(
                                k2,
                                lo,
                                &[vars.get(i).cloned().context("in getting the var")?],
                                &[Type::F64],
                            ),
                            None,
                        ))
                    }
                    swc_ssa::SValue::StoreId { target, val } => {
                        let so = o(module, 0, true);
                        let (mut a, mut b) =
                            state.get(val).cloned().context("in getting the value")?;
                        let a = full_deopt(fb, k2, a, b, module)?;
                        fb.add_op(
                            k2,
                            so,
                            &[vars.get(target).cloned().context("in getting the var")?, a],
                            &[],
                        );
                        None
                    }
                };
                if let Some(j) = j {
                    if j.1 == None {
                        drops.insert(j.0);
                    }
                    state.insert(i, j);
                }
            }
            let target =
                |target: &STarget, module: &mut Module, this: &mut Self, fb: &mut FunctionBody| {
                    let mut tags2 = vec![None];
                    let args = target
                        .args
                        .iter()
                        .filter_map(|v| state.get(v))
                        .map(|(a, b)| {
                            tags2.push(b.clone());
                            *a
                        })
                        .collect();
                    let t = BlockTarget {
                        block: this.go(module, o, f, fb, target.block, tags2, vars, memory)?,
                        args,
                    };
                    let k2 = fb.add_block();
                    let f = get_export_f(module, "jrl_object_drop")?;
                    for d in drops.iter().cloned().filter(|a| !t.args.contains(a)) {
                        fb.add_op(k2, Operator::Call { function_index: f }, &[d], &[]);
                    }
                    fb.set_terminator(k2, waffle::Terminator::Br { target: t });
                    anyhow::Ok(BlockTarget {
                        block: k2,
                        args: vec![],
                    })
                };
            let term = match &f.cfg.blocks[k].postcedent.term {
                swc_ssa::STerm::Throw(id) => {
                    let (a, b) = state.get(id).cloned().context("in getting the val")?;
                    let a = full_deopt(fb, k2, a, b, module)?;
                    match catchk.as_ref() {
                        Some(t) => {
                            let t = BlockTarget {
                                block: t.block,
                                args: once(a).chain(t.args.iter().cloned()).collect(),
                            };
                            let f = get_export_f(module, "jrl_object_drop")?;
                            for d in drops.iter().cloned().filter(|a| !t.args.contains(a)) {
                                fb.add_op(k2, Operator::Call { function_index: f }, &[d], &[]);
                            }
                            waffle::Terminator::Br { target: t }
                        }
                        None => {
                            let f = get_export_f(module, "jrl_object_drop")?;
                            for d in drops.iter().cloned().filter(|b| *b != a) {
                                fb.add_op(k2, Operator::Call { function_index: f }, &[d], &[]);
                            }
                            let f = get_export_f(module, "jrl_result_new")?;
                            let o =
                                fb.add_op(k2, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
                            let o =
                                fb.add_op(k2, Operator::Call { function_index: f }, &[a, o], &[t]);
                            waffle::Terminator::Return { values: vec![o] }
                        }
                    }
                }
                swc_ssa::STerm::Return(id) => {
                    let id = id.as_ref().context("in getting the explicit retval")?;
                    let (a, b) = state.get(id).cloned().context("in getting the val")?;
                    let a = full_deopt(fb, k2, a, b, module)?;
                    let f = get_export_f(module, "jrl_object_drop")?;
                    for d in drops.iter().cloned().filter(|b| *b != a) {
                        fb.add_op(k2, Operator::Call { function_index: f }, &[d], &[]);
                    }
                    let f = get_export_f(module, "jrl_result_new")?;
                    let o = fb.add_op(k2, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                    let o = fb.add_op(k2, Operator::Call { function_index: f }, &[a, o], &[t]);
                    waffle::Terminator::Return { values: vec![o] }
                }
                swc_ssa::STerm::Jmp(starget) => waffle::Terminator::Br {
                    target: target(starget, module, self, fb)?,
                },
                swc_ssa::STerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => todo!(),
                swc_ssa::STerm::Switch { x, blocks, default } => todo!(),
                swc_ssa::STerm::Default => waffle::Terminator::Unreachable,
            };
            fb.set_terminator(k2, term);
        }
    }
}
pub fn emit_body(
    module: &mut Module,
    memory: Memory,
    f: &SFunc,
    fb: &mut FunctionBody,
    vars: &[Ident],
) -> anyhow::Result<()> {
    let e = fb.entry;
    let t = if module.memories[memory].memory64 {
        Type::I64
    } else {
        Type::I32
    };
    let o = |module: &Module, i: u64, s: bool| {
        let l: u64 = if module.memories[memory].memory64 {
            8
        } else {
            4
        };
        let arg = MemoryArg {
            align: l.ilog2(),
            offset: (i * l),
            memory,
        };
        if s {
            if module.memories[memory].memory64 {
                Operator::I64Store { memory: arg }
            } else {
                Operator::I32Store { memory: arg }
            }
        } else {
            if module.memories[memory].memory64 {
                Operator::I64Load { memory: arg }
            } else {
                Operator::I32Load { memory: arg }
            }
        }
    };
    let lo = o(module, 0, false);
    let so = o(module, 0, true);
    let uf = get_export_f(module, "jrl_undef")?;
    let xf = get_export_f(module, "jrl_cell_new")?;
    let undef = fb.add_op(e, Operator::Call { function_index: uf }, &[], &[t]);
    let var_map = vars
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, x)| {
            let v = fb.blocks[e].params[0].1;
            (x, fb.add_op(e, o(module, i as u64, false), &[v], &[t]))
        })
        .collect::<BTreeMap<_, _>>()
        .into_iter()
        .chain(f.cfg.decls.iter().cloned().map(|a| {
            (a, {
                fb.add_op(e, Operator::Call { function_index: xf }, &[undef], &[t])
            })
        }))
        .collect::<BTreeMap<_, _>>();
    let mut i = BodyInstance {
        all: BTreeMap::new(),
        undef,
    };

    Ok(())
}
pub fn emit_ssa(module: &mut Module, f: &SFunc, vars: &[Ident]) -> anyhow::Result<(Func, u64)> {
    let f = swc_ssa::ch::ch(f)?;
    let memory = module
        .exports
        .iter()
        .find_map(|a| {
            if a.name == "memory" {
                match &a.kind {
                    ExportKind::Memory(m) => Some(*m),
                    _ => None,
                }
            } else {
                None
            }
        })
        .context("in getting the memory")?;
    let table = module
        .exports
        .iter()
        .find_map(|a| {
            if a.name == "__indirect_function_table" {
                match &a.kind {
                    ExportKind::Table(m) => Some(*m),
                    _ => None,
                }
            } else {
                None
            }
        })
        .context("in getting the memory")?;
    let t = if module.memories[memory].memory64 {
        Type::I64
    } else {
        Type::I32
    };
    let sig = new_sig(
        module,
        waffle::SignatureData::Func {
            params: vec![t, t, t, t, t],
            returns: vec![t],
        },
    );
    let mut fb = FunctionBody::new(module, sig);
    let fi = module.funcs.push(waffle::FuncDecl::None);
    let ti = {
        let mut a = module.tables[table]
            .func_elements
            .as_mut()
            .context("in getting the elements")?;
        let ti = a.len() as u64;
        a.push(fi);
        ti
    };
    emit_body(module, memory, &f, &mut fb, vars)?;
    module.funcs[fi] = FuncDecl::Body(sig, format!("$"), fb);
    return Ok((fi, ti));
}
