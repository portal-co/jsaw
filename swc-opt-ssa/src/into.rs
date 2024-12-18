use std::{collections::BTreeMap, mem::swap};

use anyhow::Context;
use id_arena::Id;
use swc_ecma_ast::{BinaryOp, Lit, UnaryOp};
use swc_ssa::{SBlock, SCatch, SFunc, STarget, STerm, SValue, SwcFunc};
use swc_tac::Item;

use crate::{OptBlock, OptCfg, OptFunc, OptType, OptValue, OptValueW};

pub struct Convert {
    pub all: BTreeMap<Id<SBlock>, BTreeMap<Vec<Option<OptType>>, Id<OptBlock>>>,
}
fn deopt(
    out: &mut OptCfg,
    k: Id<OptBlock>,
    mut v: Id<OptValueW>,
    mut ty: Option<OptType>,
) -> anyhow::Result<Id<OptValueW>> {
    while let Some(t) = ty {
        let w = out.values.alloc(OptValueW(OptValue::Deopt(v)));
        out.blocks[k].insts.push(w);
        v = w;
        ty = t.parent();
    }
    return Ok(v);
}
fn bi_id_deopt(
    out: &mut OptCfg,
    k: Id<OptBlock>,
    mut v1: Id<OptValueW>,
    mut ty1: Option<OptType>,
    mut v2: Id<OptValueW>,
    mut ty2: Option<OptType>,
) -> anyhow::Result<(Id<OptValueW>, Id<OptValueW>, Option<OptType>)> {
    let mut s = false;
    while ty1 != ty2 {
        if let Some(t) = ty1 {
            let w = out.values.alloc(OptValueW(OptValue::Deopt(v1)));
            out.blocks[k].insts.push(w);
            v1 = w;
            ty1 = t.parent();
        };
        swap(&mut ty1, &mut ty2);
        swap(&mut v1, &mut v2);
        s = !s;
    }
    if s {
        swap(&mut ty1, &mut ty2);
        swap(&mut v1, &mut v2);
    }
    return Ok((v1, v2, ty1));
}
impl Convert {
    pub fn transform(
        &mut self,
        inp: &SwcFunc,
        out: &mut OptCfg,
        i: Id<SBlock>,
        tys: Vec<Option<OptType>>,
    ) -> anyhow::Result<Id<OptBlock>> {
        loop {
            if let Some(k) = self.all.get(&i).and_then(|v| v.get(&tys)) {
                return Ok(*k);
            }
            let k = out.blocks.alloc(Default::default());
            self.all.entry(i).or_default().insert(tys.clone(), k);
            let mut state = inp.blocks[i]
                .params
                .iter()
                .map(|a| a.0)
                .zip(
                    tys.iter()
                        .cloned()
                        .map(|a| (out.add_blockparam(k, a.clone()), a)),
                )
                .collect::<BTreeMap<_, _>>();
            let catch = match &inp.blocks[i].postcedent.catch {
                swc_ssa::SCatch::Throw => SCatch::Throw,
                swc_ssa::SCatch::Just { target } => {
                    let mut xs = vec![None];
                    let args = target
                        .args
                        .iter()
                        .filter_map(|x| state.get(x))
                        .cloned()
                        .map(|(a, x)| {
                            xs.push(x);
                            return a;
                        })
                        .collect();
                    SCatch::Just {
                        target: STarget {
                            args,
                            block: self.transform(inp, out, target.block, xs)?,
                        },
                    }
                }
            };
            out.blocks[k].postcedent.catch = catch;
            for i in inp.blocks[i].stmts.iter().cloned() {
                let (j, tag) = match &inp.values[i].0 {
                    SValue::Param { block, idx, ty } => todo!(),
                    SValue::Item(item) => match item {
                        swc_tac::Item::Just { id } => {
                            let (a, b) = state.get(id).cloned().context("in getting the value")?;

                            (
                                OptValue::Emit {
                                    val: SValue::Item(Item::Just { id: a }),
                                    ty: b.clone(),
                                },
                                b,
                            )
                        }
                        swc_tac::Item::Bin { left, right, op } => {
                            let (left, lty) =
                                state.get(left).cloned().context("in getting the value")?;
                            let (right, rty) =
                                state.get(right).cloned().context("in getting the value")?;
                            match (lty.clone(), rty.clone(), op) {
                                (
                                    Some(OptType::U32 { bits_usable }),
                                    Some(OptType::U32 { bits_usable: b2 }),
                                    BinaryOp::Mul,
                                ) if (32 - bits_usable) + (32 - b2) <= 32 => {
                                    let result = SValue::Item(Item::Bin {
                                        left,
                                        right,
                                        op: op.clone(),
                                    });
                                    let ty = Some(OptType::U32 {
                                        bits_usable: (32 - bits_usable) + (32 - b2) - 32,
                                    });
                                    match op {
                                        op => (
                                            OptValue::Emit {
                                                val: result,
                                                ty: ty.clone(),
                                            },
                                            ty,
                                        ),
                                    }
                                }
                                (
                                    Some(OptType::U64 { bits_usable }),
                                    Some(OptType::U64 { bits_usable: b2 }),
                                    BinaryOp::Mul,
                                ) if (64 - bits_usable) + (64 - b2) <= 64 => {
                                    let result = SValue::Item(Item::Bin {
                                        left,
                                        right,
                                        op: op.clone(),
                                    });
                                    let ty = Some(OptType::U64 {
                                        bits_usable: (64 - bits_usable) + (64 - b2) - 64,
                                    });
                                    match op {
                                        op => (
                                            OptValue::Emit {
                                                val: result,
                                                ty: ty.clone(),
                                            },
                                            ty,
                                        ),
                                    }
                                }
                                (lty, rty, op) => {
                                    let (left, right, ty) =
                                        bi_id_deopt(out, k, left, lty, right, rty)?;
                                    match (ty.clone(), op) {
                                        (Some(OptType::Number | OptType::BigInt), op) => {
                                            let result = SValue::Item(Item::Bin {
                                                left,
                                                right,
                                                op: op.clone(),
                                            });
                                            let ty = if op.precedence() == 6 || op.precedence() == 7
                                            {
                                                Some(OptType::Bool)
                                            } else {
                                                ty
                                            };
                                            match op {
                                                op => (
                                                    OptValue::Emit {
                                                        val: result,
                                                        ty: ty.clone(),
                                                    },
                                                    ty,
                                                ),
                                            }
                                        }
                                        (
                                            Some(
                                                OptType::U32 { bits_usable }
                                                | OptType::U64 { bits_usable },
                                            ),
                                            BinaryOp::Add | BinaryOp::Sub,
                                        ) if bits_usable != 0 => {
                                            let result = SValue::Item(Item::Bin {
                                                left,
                                                right,
                                                op: op.clone(),
                                            });
                                            let ty = if op.precedence() == 6 || op.precedence() == 7
                                            {
                                                Some(OptType::Bool)
                                            } else {
                                                ty.unwrap().parent()
                                            };
                                            match op {
                                                op => (
                                                    OptValue::Emit {
                                                        val: result,
                                                        ty: ty.clone(),
                                                    },
                                                    ty,
                                                ),
                                            }
                                        }
                                        (ty, op) => {
                                            let left = deopt(out, k, left, ty.clone())?;
                                            let right = deopt(out, k, right, ty)?;
                                            let result = SValue::Item(Item::Bin {
                                                left,
                                                right,
                                                op: op.clone(),
                                            });
                                            match op {
                                                op => (
                                                    OptValue::Emit {
                                                        val: result,
                                                        ty: None,
                                                    },
                                                    None,
                                                ),
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        swc_tac::Item::Un { arg, op } => {
                            let (arg, tag) =
                                state.get(arg).cloned().context("in getting the value")?;
                            match (tag.clone(), op) {
                                (Some(OptType::Number | OptType::U32 { .. }), UnaryOp::Plus) => (
                                    OptValue::Emit {
                                        val: SValue::Item(Item::Just { id: arg }),
                                        ty: tag.clone(),
                                    },
                                    tag,
                                ),
                                (
                                    Some(
                                        OptType::Number
                                        | OptType::BigInt
                                        | OptType::U32 { .. }
                                        | OptType::U64 { .. },
                                    ),
                                    UnaryOp::Minus,
                                ) => {
                                    let result = SValue::Item(Item::Un {
                                        arg: arg,
                                        op: op.clone(),
                                    });
                                    match op {
                                        op => (
                                            OptValue::Emit {
                                                val: result,
                                                ty: tag.clone(),
                                            },
                                            tag,
                                        ),
                                    }
                                }
                                (tag, op) => {
                                    let arg = deopt(out, k, arg, tag)?;
                                    let result = SValue::Item(Item::Un {
                                        arg: arg,
                                        op: op.clone(),
                                    });
                                    match op {
                                        UnaryOp::Plus => {
                                            let val = OptValueW(OptValue::Emit {
                                                val: result,
                                                ty: None,
                                            });
                                            let val = out.values.alloc(val);
                                            out.blocks[k].insts.push(val);
                                            (
                                                OptValue::Assert {
                                                    val: val,
                                                    ty: Some(OptType::Number),
                                                },
                                                Some(OptType::Number),
                                            )
                                        }
                                        op => (
                                            OptValue::Emit {
                                                val: result,
                                                ty: None,
                                            },
                                            None,
                                        ),
                                    }
                                }
                            }
                        }
                        Item::Lit { lit } => match lit.clone() {
                            Lit::Bool(_) => (
                                OptValue::Emit {
                                    val: SValue::Item(Item::Lit { lit: lit.clone() }),
                                    ty: Some(OptType::Bool),
                                },
                                Some(OptType::Bool),
                            ),
                            Lit::BigInt(_) => (
                                OptValue::Emit {
                                    val: SValue::Item(Item::Lit { lit: lit.clone() }),
                                    ty: Some(OptType::BigInt),
                                },
                                Some(OptType::BigInt),
                            ),
                            Lit::Num(_) => (
                                OptValue::Emit {
                                    val: SValue::Item(Item::Lit { lit: lit.clone() }),
                                    ty: Some(OptType::Number),
                                },
                                Some(OptType::Number),
                            ),
                            lit => (
                                OptValue::Emit {
                                    val: SValue::Item(Item::Lit { lit }),
                                    ty: None,
                                },
                                None,
                            ),
                        },
                        Item::Arr { members } if members.len() > 0 => {
                            let (x, ty) = state
                                .get(&members[0])
                                .cloned()
                                .context("in getting the var")?;
                            let mut elem_tys = vec![];
                            let mut members = members[1..]
                                .iter()
                                .map(|a| {
                                    let (mut a, mut at) =
                                        state.get(a).cloned().context("in getting the val")?;
                                    // (a, x, at) = bi_id_deopt(out, k, a, at, x, ty.clone())?;
                                    // ty = at.clone();
                                    elem_tys.push(at.clone());
                                    Ok((a))
                                })
                                .collect::<anyhow::Result<Vec<_>>>()?;
                            let ty = Some(OptType::Object {
                                nest: crate::ObjType::Array,
                                extensible: false,
                                elem_tys,
                            });
                            (
                                OptValue::Emit {
                                    val: SValue::Item(Item::Arr { members: members }),
                                    ty: ty.clone(),
                                },
                                ty,
                            )
                        }
                        Item::Mem { obj, mem } => {
                            let (mut obj, mut oty) =
                                state.get(obj).cloned().context("in getting the val")?;
                            let (mem, mty) =
                                state.get(mem).cloned().context("in getting the val")?;
                            while let Some(OptType::Object {
                                nest,
                                extensible,
                                elem_tys,
                            }) = &oty
                            {
                                let w = out.values.alloc(OptValueW(OptValue::Deopt(obj)));
                                out.blocks[k].insts.push(w);
                                obj = w;
                                oty = oty.unwrap().parent();
                            }
                            match (oty.clone(), mty.clone()) {
                                (
                                    Some(OptType::Array { elem_ty }),
                                    Some(
                                        OptType::Number
                                        | OptType::BigInt
                                        | OptType::U32 { .. }
                                        | OptType::U64 { .. },
                                    ),
                                ) => (
                                    OptValue::Emit {
                                        val: SValue::Item(Item::Mem { obj, mem }),
                                        ty: elem_ty.as_ref().clone(),
                                    },
                                    elem_ty.as_ref().clone(),
                                ),
                                (oty, mty) => {
                                    let obj = deopt(out, k, obj, oty)?;
                                    let mem = deopt(out, k, mem, mty)?;
                                    (
                                        OptValue::Emit {
                                            val: SValue::Item(Item::Mem { obj, mem }),
                                            ty: None,
                                        },
                                        None,
                                    )
                                }
                            }
                        }
                        a => {
                            let a = a.clone().map(&mut |x| {
                                let (val, tag) =
                                    state.get(&x).cloned().context("in getting the val")?;
                                deopt(out, k, val, tag)
                            })?;
                            (
                                OptValue::Emit {
                                    val: SValue::Item(a),
                                    ty: None,
                                },
                                None,
                            )
                        }
                    },
                    SValue::Assign { target, val } => {
                        let target = target.clone().map(&mut |x| {
                            let (val, tag) =
                                state.get(&x).cloned().context("in getting the val")?;
                            deopt(out, k, val, tag)
                        })?;
                        let (val, tag) = state.get(val).cloned().context("in getting the val")?;
                        let val = deopt(out, k, val, tag)?;
                        (
                            OptValue::Emit {
                                val: SValue::Assign { target, val },
                                ty: None,
                            },
                            None,
                        )
                    }
                    SValue::LoadId(i) => (
                        OptValue::Emit {
                            val: SValue::LoadId(i.clone()),
                            ty: None,
                        },
                        None,
                    ),
                    SValue::StoreId { target, val } => {
                        let (val, tag) = state.get(val).cloned().context("in getting the val")?;
                        let val = deopt(out, k, val, tag)?;
                        (
                            OptValue::Emit {
                                val: SValue::StoreId {
                                    target: target.clone(),
                                    val,
                                },
                                ty: None,
                            },
                            None,
                        )
                    }
                };
                let j = crate::OptValueW(j);
                let j = out.values.alloc(j);
                out.blocks[k].insts.push(j);
                state.insert(i, (j, tag));
            }
            let tgt = |this: &mut Self, out: &mut OptCfg, target: &STarget| {
                let mut xs = vec![];
                let args = target
                    .args
                    .iter()
                    .filter_map(|x| state.get(x))
                    .cloned()
                    .map(|(a, x)| {
                        xs.push(x);
                        return a;
                    })
                    .collect();
                anyhow::Ok(STarget {
                    args,
                    block: this.transform(inp, out, target.block, xs)?,
                })
            };
            let term = match &inp.blocks[i].postcedent.term {
                swc_ssa::STerm::Throw(v) => {
                    let (v, ty) = state.get(v).cloned().context("in getting the val")?;
                    let v = deopt(out, k, v, ty)?;
                    STerm::Throw(v)
                }
                swc_ssa::STerm::Return(v) => STerm::Return(match v.as_ref() {
                    None => None,
                    Some(v) => {
                        let (v, ty) = state.get(v).cloned().context("in getting the val")?;
                        let v = deopt(out, k, v, ty)?;
                        Some(v)
                    }
                }),
                swc_ssa::STerm::Jmp(starget) => STerm::Jmp(tgt(self, out, starget)?),
                swc_ssa::STerm::CondJmp {
                    cond,
                    if_true,
                    if_false,
                } => {
                    let (cond, ty) = state.get(cond).cloned().context("in getting the val")?;
                    let cond = if ty == Some(OptType::Bool) {
                        cond
                    } else {
                        let cond = deopt(out, k, cond, ty)?;
                        let cond = OptValue::Assert {
                            val: cond,
                            ty: Some(OptType::Bool),
                        };
                        let cond = out.values.alloc(OptValueW(cond));
                        out.blocks[k].insts.push(cond);
                        cond
                    };
                    STerm::CondJmp {
                        cond,
                        if_true: tgt(self, out, if_true)?,
                        if_false: tgt(self, out, if_false)?,
                    }
                }
                swc_ssa::STerm::Switch { x, blocks, default } => {
                    let (mut x, mut ty) = state.get(x).cloned().context("in getting the val")?;
                    let default = tgt(self, out, default)?;
                    let mut blocks = blocks
                        .iter()
                        .map(|(a, b)| {
                            let (mut a, mut at) =
                                state.get(a).cloned().context("in getting the val")?;
                            (a, x, at) = bi_id_deopt(out, k, a, at, x, ty.clone())?;
                            ty = at.clone();
                            Ok((a, tgt(self, out, b)?))
                        })
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    for (m, _) in blocks.iter_mut() {
                        while out.values[*m].ty(&out) != ty {
                            let n = out.values.alloc(OptValueW(OptValue::Deopt(*m)));
                            out.blocks[k].insts.push(n);
                            *m = n;
                        }
                    }
                    STerm::Switch { x, blocks, default }
                }
                swc_ssa::STerm::Default => STerm::Default,
            };
            out.blocks[k].postcedent.term = term;
        }
    }
}
impl TryFrom<SFunc> for OptFunc {
    type Error = anyhow::Error;

    fn try_from(mut value: SFunc) -> Result<Self, Self::Error> {
        ssa_impls::maxssa::maxssa(&mut value);
        let mut cfg = OptCfg::default();
        cfg.decls.extend(value.cfg.decls.iter().cloned());
        let mut t = Convert {
            all: BTreeMap::new(),
        };
        let tys = value.cfg.blocks[value.entry]
            .params
            .iter()
            .map(|_| None)
            .collect();
        let x = t.transform(&value.cfg, &mut cfg, value.entry, tys)?;
        Ok(OptFunc {
            cfg,
            entry: x,
            is_generator: value.is_generator,
            is_async: value.is_async,
        })
    }
}
