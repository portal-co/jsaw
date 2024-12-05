use std::collections::BTreeMap;

use anyhow::Context;
use swc_opt_ssa::OptType;
use waffle::{
    util::new_sig, BlockTarget, ExportKind, Func, FuncDecl, FunctionBody, HeapType, Memory, Module,
    Operator, SignatureData, Table, TableData, Type, WithMutablility, WithNullable,
};
use waffle_ast::tutils::{talloc, tfree};
pub struct Gopts {
    pub use_gc: bool,
}
fn memory(module: &Module) -> anyhow::Result<Memory> {
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
    Ok(memory)
}
fn ift(module: &Module) -> anyhow::Result<Table> {
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
    Ok(table)
}
pub fn to_type(
    module: &mut Module,
    o: Option<&OptType>,
    handle: bool,
    gopts: &Gopts,
) -> anyhow::Result<Type> {
    let Some(o) = o else {
        return Ok(Type::F64);
    };
    Ok(match o {
        OptType::Number => Type::F64,
        OptType::U32 { bits_usable } => Type::I32,
        OptType::BigInt => Type::F64,
        OptType::U64 { bits_usable } => Type::I64,
        OptType::Bool => Type::I32,
        OptType::Array { elem_ty } => {
            if gopts.use_gc {
                if handle {
                    Type::I32
                } else {
                    let sig = waffle::SignatureData::Array {
                        ty: WithMutablility {
                            mutable: true,
                            value: waffle::StorageType::Val(to_type(
                                module,
                                elem_ty.as_ref().as_ref(),
                                true,
                                gopts,
                            )?),
                        },
                    };
                    let sig = new_sig(module, sig);
                    Type::Heap(WithNullable {
                        nullable: true,
                        value: waffle::HeapType::Sig { sig_index: sig },
                    })
                }
            } else {
                Type::F64
            }
        }
    })
}
#[derive(Clone, Copy)]
pub struct TableI {
    pub table: Table,
    pub talloc: Func,
    pub tfree: Func,
}
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
pub struct Main {
    pub array_tables: BTreeMap<Option<OptType>, TableI>,
    pub drops: BTreeMap<Option<OptType>, Func>,
    pub collects: BTreeMap<Option<OptType>, Func>,
}
impl Main {
    pub fn array_table(
        &mut self,
        gopts: &Gopts,
        module: &mut Module,
        x: Option<OptType>,
    ) -> anyhow::Result<TableI> {
        loop {
            if let Some(a) = self.array_tables.get(&x) {
                return Ok(*a);
            }
            let rty = to_type(
                module,
                Some(&OptType::Array {
                    elem_ty: Box::new(x.clone()),
                }),
                false,
                gopts,
            )?;
            match rty {
                Type::Heap(_) => {
                    let t = module.tables.push(TableData {
                        ty: rty,
                        initial: 0,
                        max: None,
                        table64: false,
                        func_elements: None,
                    });
                    let talloc = talloc(module, t, &[])?;
                    let tfree = tfree(module, t, &[])?;
                    self.array_tables.insert(
                        x.clone(),
                        TableI {
                            table: t,
                            talloc,
                            tfree,
                        },
                    );
                }
                _ => anyhow::bail!("usage of array tables on non-gc platforms"),
            }
        }
    }
    pub fn dropper(
        &mut self,
        gopts: &Gopts,
        module: &mut Module,
        x: Option<OptType>,
    ) -> anyhow::Result<Func> {
        loop {
            if let Some(f) = self.drops.get(&x) {
                return Ok(*f);
            }
            let f = module.funcs.push(waffle::FuncDecl::None);
            self.drops.insert(x.clone(), f);
            let sig = SignatureData::Func {
                params: vec![to_type(module, x.as_ref(), true, gopts)?],
                returns: vec![],
            };
            let sig = new_sig(module, sig);
            let mut body = FunctionBody::new(module, sig);
            let exp_dropper = get_export_f(module, "jrl_object_drop")?;
            let param = body.blocks[body.entry].params[0].1;
            let e = body.entry;
            match x.as_ref() {
                Some(a) => match a {
                    OptType::Number => {
                        body.set_terminator(e, waffle::Terminator::Return { values: vec![] });
                    }
                    OptType::U32 { bits_usable } => {
                        body.set_terminator(e, waffle::Terminator::Return { values: vec![] });
                    }
                    OptType::BigInt => {
                        body.set_terminator(
                            e,
                            waffle::Terminator::ReturnCall {
                                func: exp_dropper,
                                args: vec![param],
                            },
                        );
                    }
                    OptType::U64 { bits_usable } => {
                        body.set_terminator(e, waffle::Terminator::Return { values: vec![] });
                    }
                    OptType::Bool => {
                        body.set_terminator(e, waffle::Terminator::Return { values: vec![] });
                    }
                    OptType::Array { elem_ty } => {
                        if gopts.use_gc {
                            let t = self.array_table(gopts, module, elem_ty.as_ref().clone())?;
                            let nest = self.dropper(gopts, module, elem_ty.as_ref().clone())?;
                            let nty = to_type(module, elem_ty.as_ref().as_ref(), true, gopts)?;
                            let ty = to_type(module, x.as_ref(), false, gopts)?;
                            let Type::Heap(WithNullable {
                                value: HeapType::Sig { sig_index },
                                nullable,
                            }) = ty
                            else {
                                anyhow::bail!("type not expected");
                            };
                            let f = body.add_op(
                                e,
                                Operator::Call {
                                    function_index: t.tfree,
                                },
                                &[param],
                                &[ty],
                            );
                            let arg = body.add_op(e, Operator::ArrayLen, &[f], &[Type::I32]);
                            let lop = body.add_block();
                            let p = body.add_blockparam(lop, Type::I32);
                            body.set_terminator(
                                e,
                                waffle::Terminator::Br {
                                    target: BlockTarget {
                                        block: lop,
                                        args: vec![arg],
                                    },
                                },
                            );
                            let i = body.add_op(lop, Operator::I32Eqz, &[p], &[Type::I32]);
                            let brk = body.add_block();
                            body.set_terminator(brk, waffle::Terminator::Return { values: vec![] });
                            let c = body.add_block();
                            body.set_terminator(
                                lop,
                                waffle::Terminator::CondBr {
                                    cond: i,
                                    if_true: BlockTarget {
                                        block: brk,
                                        args: vec![],
                                    },
                                    if_false: BlockTarget {
                                        block: c,
                                        args: vec![],
                                    },
                                },
                            );
                            let v = body.add_op(
                                c,
                                Operator::ArrayGet { sig: sig_index },
                                &[f, p],
                                &[nty],
                            );
                            body.add_op(
                                c,
                                Operator::Call {
                                    function_index: nest,
                                },
                                &[v],
                                &[],
                            );
                            let m =
                                body.add_op(c, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                            let m = body.add_op(c, Operator::I32Sub, &[p, m], &[Type::I32]);
                            body.set_terminator(
                                c,
                                waffle::Terminator::Br {
                                    target: BlockTarget {
                                        block: lop,
                                        args: vec![m],
                                    },
                                },
                            );
                        } else {
                            body.set_terminator(
                                e,
                                waffle::Terminator::ReturnCall {
                                    func: exp_dropper,
                                    args: vec![param],
                                },
                            );
                        }
                    }
                },
                None => {
                    body.set_terminator(
                        e,
                        waffle::Terminator::ReturnCall {
                            func: exp_dropper,
                            args: vec![param],
                        },
                    );
                }
            };
            module.funcs[f] = FuncDecl::Body(sig, format!("~drop"), body);
        }
    }
    pub fn collector(
        &mut self,
        gopts: &Gopts,
        module: &mut Module,
        x: Option<OptType>,
    ) -> anyhow::Result<Func> {
        loop {
            if let Some(f) = self.collects.get(&x) {
                return Ok(*f);
            }
            let f = module.funcs.push(waffle::FuncDecl::None);
            self.collects.insert(x.clone(), f);
            let sty = if module.tables[ift(module)?].table64{
                Type::I64
            }else{
                Type::I32
            };
            let sig = SignatureData::Func {
                params: vec![to_type(module, x.as_ref(), true, gopts)?, sty, Type::I32],
                returns: vec![Type::I32],
            };
            let sig = new_sig(module, sig);
            let tsig = new_sig(
                module,
                SignatureData::Func {
                    params: vec![Type::F64, Type::I32],
                    returns: vec![sty],
                },
            );
            let mut body = FunctionBody::new(module, sig);
            let exp_dropper = get_export_f(module, "jrl_object_drop")?;
            let param = body.blocks[body.entry].params[0].1;
            let func_idx = body.blocks[body.entry].params[1].1;
            let state = body.blocks[body.entry].params[2].1;
            let e = body.entry;
            match x.as_ref() {
                Some(a) => match a {
                    OptType::Number => {
                        body.set_terminator(
                            e,
                            waffle::Terminator::Return {
                                values: vec![state],
                            },
                        );
                    }
                    OptType::U32 { bits_usable } => {
                        body.set_terminator(
                            e,
                            waffle::Terminator::Return {
                                values: vec![state],
                            },
                        );
                    }
                    OptType::BigInt => {
                        body.set_terminator(
                            e,
                            waffle::Terminator::ReturnCallIndirect {
                                table: ift(module)?,
                                args: vec![param, state, func_idx],
                                sig: tsig,
                            },
                        );
                    }
                    OptType::U64 { bits_usable } => {
                        body.set_terminator(
                            e,
                            waffle::Terminator::Return {
                                values: vec![state],
                            },
                        );
                    }
                    OptType::Bool => {
                        body.set_terminator(
                            e,
                            waffle::Terminator::Return {
                                values: vec![state],
                            },
                        );
                    }
                    OptType::Array { elem_ty } => {
                        if gopts.use_gc {
                            let t = self.array_table(gopts, module, elem_ty.as_ref().clone())?;
                            let nest = self.collector(gopts, module, elem_ty.as_ref().clone())?;
                            let nty = to_type(module, elem_ty.as_ref().as_ref(), true, gopts)?;
                            let ty = to_type(module, x.as_ref(), false, gopts)?;
                            let Type::Heap(WithNullable {
                                value: HeapType::Sig { sig_index },
                                nullable,
                            }) = ty
                            else {
                                anyhow::bail!("type not expected");
                            };
                            let f = body.add_op(
                                e,
                                Operator::TableGet {
                                    table_index: t.table,
                                },
                                &[param],
                                &[ty],
                            );
                            let arg = body.add_op(e, Operator::ArrayLen, &[f], &[Type::I32]);
                            let lop = body.add_block();
                            let p = body.add_blockparam(lop, Type::I32);
                            let s = body.add_blockparam(lop, sty);
                            body.set_terminator(
                                e,
                                waffle::Terminator::Br {
                                    target: BlockTarget {
                                        block: lop,
                                        args: vec![arg, state],
                                    },
                                },
                            );
                            let i = body.add_op(lop, Operator::I32Eqz, &[p], &[Type::I32]);
                            let brk = body.add_block();
                            body.set_terminator(
                                brk,
                                waffle::Terminator::Return { values: vec![s] },
                            );
                            let c = body.add_block();
                            body.set_terminator(
                                lop,
                                waffle::Terminator::CondBr {
                                    cond: i,
                                    if_true: BlockTarget {
                                        block: brk,
                                        args: vec![],
                                    },
                                    if_false: BlockTarget {
                                        block: c,
                                        args: vec![],
                                    },
                                },
                            );
                            let v = body.add_op(
                                c,
                                Operator::ArrayGet { sig: sig_index },
                                &[f, p],
                                &[nty],
                            );
                            let s = body.add_op(
                                c,
                                Operator::Call {
                                    function_index: nest,
                                },
                                &[v, s, func_idx],
                                &[],
                            );
                            let m =
                                body.add_op(c, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                            let m = body.add_op(c, Operator::I32Sub, &[p, m], &[Type::I32]);
                            body.set_terminator(
                                c,
                                waffle::Terminator::Br {
                                    target: BlockTarget {
                                        block: lop,
                                        args: vec![m, s],
                                    },
                                },
                            );
                        } else {
                            body.set_terminator(
                                e,
                                waffle::Terminator::ReturnCallIndirect {
                                    table: ift(module)?,
                                    args: vec![param, state, func_idx],
                                    sig: tsig,
                                },
                            );
                        }
                    }
                },
                None => {
                    body.set_terminator(
                        e,
                        waffle::Terminator::ReturnCallIndirect {
                            table: ift(module)?,
                            args: vec![param, state, func_idx],
                            sig: tsig,
                        },
                    );
                }
            };
            module.funcs[f] = FuncDecl::Body(sig, format!("~drop"), body);
        }
    }
}
pub struct Internal<'a> {
    pub main: &'a Main,
}
