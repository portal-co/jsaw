//! The nano representation (DreamComp #36): arena-offset references.
//!
//! References are one word holding a byte offset into the lane's arena; the
//! zero word is null (offset 0 is never allocated) and odd words are i31
//! payloads, so real offsets are always 8-aligned and even. Every allocation
//! carries the 8-byte header `{ type_id: i32, len: i32 }`; struct fields and
//! array elements follow at naturally-aligned offsets computed from the
//! plan's signatures. This mirrors `nano_mvp`'s arena ABI, widened to 8-byte
//! words.
//!
//! The arena itself is target-runtime state: growth goes through the
//! `js_nano_arena_ensure` runtime entry point, which must zero-fill any
//! fresh region (the bump allocator relies on zero-fill for
//! `ArrayNewDefault` payloads and header padding), return 0 on success, and
//! return nonzero on exhaustion (the caller then traps deterministically).

use super::{
    InstCx, LeBinOp, LeCallee, LeDiag, LeExpr, LeFuncId, LeLocal, LeModule, LeStmt, LeType,
    LeUnOp, Representation, RuntimeSink, heap_test_mode, zero_of,
};
use crate::backend_plan::{
    BackendPlan, PlanFuncId, PlanHeapType, PlanSigId, PlanSignature, PlanStorage, TypeId,
};

/// Byte offset of the header's `len` field (the type id is at 0).
const LEN_OFFSET: u32 = 4;
/// Byte offset where the payload starts.
const PAYLOAD_OFFSET: u32 = 8;
/// Initial bump pointer (offset 0 is the null sentinel; low bytes reserved).
const ARENA_BASE: u64 = 16;

/// The nano representation: arena-offset references with no collector.
pub struct NanoRepresentation;

/// Physical layout of one aggregate signature in the arena.
struct AggLayout {
    type_id: u32,
    /// Struct: byte offset of each field from the allocation base.
    field_offsets: Vec<u32>,
    /// Struct field types (lowered).
    field_tys: Vec<LeType>,
    /// Total allocation size (arrays: the fixed prefix).
    fixed_size: u32,
    /// Alignment as log2.
    align_log2: u32,
    /// Array: element size as log2, and its lowered storage type.
    elem: Option<(u32, LeType)>,
}

// -- small expression constructors ---------------------------------------------

fn i64(v: u64) -> LeExpr {
    LeExpr::ConstI64(v)
}

fn i32c(v: u32) -> LeExpr {
    LeExpr::ConstI32(v)
}

fn word(value: LeExpr) -> LeExpr {
    // Widen an i32 to a word (zero-extended).
    LeExpr::Unary(LeUnOp::I64ExtendI32U, Box::new(value))
}

fn add(a: LeExpr, b: LeExpr) -> LeExpr {
    LeExpr::Binary(LeBinOp::I64Add, Box::new(a), Box::new(b))
}

fn and(a: LeExpr, b: LeExpr) -> LeExpr {
    LeExpr::Binary(LeBinOp::I64And, Box::new(a), Box::new(b))
}

fn is_null(v: LeExpr) -> LeExpr {
    LeExpr::Binary(LeBinOp::I64Eq, Box::new(v), Box::new(i64(0)))
}

fn is_i31(v: LeExpr) -> LeExpr {
    LeExpr::Binary(
        LeBinOp::I64Ne,
        Box::new(and(v, i64(1))),
        Box::new(i64(0)),
    )
}

/// `base + offset` arena address arithmetic (offsets are words).
fn addr_of(base: LeExpr, offset: u32) -> LeExpr {
    if offset == 0 {
        base
    } else {
        add(base, i64(offset as u64))
    }
}

impl NanoRepresentation {
    fn bump_global(&self, plan: &BackendPlan) -> u32 {
        // The bump pointer is the first representation global, immediately
        // after the plan globals (see `declare`).
        plan.globals.len() as u32
    }

    fn lower_storage(&self, plan: &BackendPlan, storage: &PlanStorage) -> Result<LeType, LeDiag> {
        match storage {
            PlanStorage::I8 => Ok(LeType::I8),
            PlanStorage::I16 => Ok(LeType::I16),
            PlanStorage::Val(ty) => self.lower_type(plan, *ty),
        }
    }

    fn layout<R: Representation>(
        &self,
        cx: &InstCx<R>,
        sig: PlanSigId,
    ) -> Result<AggLayout, LeDiag> {
        let plan_sig = &cx.t.plan.signatures[sig.index()];
        let type_id = cx.t.type_ids.of(sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        match plan_sig {
            PlanSignature::Struct { fields, .. } => {
                let mut field_offsets = Vec::new();
                let mut field_tys = Vec::new();
                let mut offset = PAYLOAD_OFFSET;
                let mut align_log2 = 3u32;
                for field in fields {
                    let ty = self.lower_storage(cx.t.plan, &field.storage)?;
                    let size = ty.byte_size();
                    let field_align = size.min(8);
                    offset = offset.div_ceil(field_align) * field_align;
                    field_offsets.push(offset);
                    field_tys.push(ty);
                    offset += size;
                    align_log2 = align_log2.max(size.min(8).trailing_zeros());
                }
                Ok(AggLayout {
                    type_id,
                    field_offsets,
                    field_tys,
                    fixed_size: offset.div_ceil(8).max(1) * 8,
                    align_log2,
                    elem: None,
                })
            }
            PlanSignature::Array { element, .. } => {
                let ty = self.lower_storage(cx.t.plan, &element.storage)?;
                let size = ty.byte_size();
                Ok(AggLayout {
                    type_id,
                    field_offsets: Vec::new(),
                    field_tys: Vec::new(),
                    fixed_size: PAYLOAD_OFFSET,
                    align_log2: 3,
                    elem: Some((size.trailing_zeros(), ty)),
                })
            }
            _ => Err(LeDiag {
                function: cx.at.0,
                block: cx.at.1,
                inst: cx.at.2,
                message: format!("signature {sig} is not an aggregate"),
            }),
        }
    }

    /// The bump allocator helper: `js_nano_alloc(size: i32, align_log2: i32)
    /// -> ref`. Traps deterministically on exhaustion.
    fn alloc(&self, cx: &mut InstCx<Self>, size: LeExpr, align_log2: u32, out: LeLocal) {
        let bump = self.bump_global(cx.t.plan);
        let helper = cx.helper(
            "js_nano_alloc",
            vec![LeType::I32, LeType::I32],
            vec![LeType::Ref],
            move |b, params| {
                let size = LeExpr::Local(params[0]);
                let align = LeExpr::Local(params[1]);
                let bump_v = b.assign(LeType::I64, LeExpr::GlobalGet(bump));
                let align64 = b.assign(LeType::I64, word(align));
                let mask = b.assign(
                    LeType::I64,
                    LeExpr::Binary(
                        LeBinOp::I64Sub,
                        Box::new(LeExpr::Binary(
                            LeBinOp::I64Shl,
                            Box::new(i64(1)),
                            Box::new(LeExpr::Local(align64)),
                        )),
                        Box::new(i64(1)),
                    ),
                );
                let not_mask = b.assign(
                    LeType::I64,
                    LeExpr::Binary(
                        LeBinOp::I64Xor,
                        Box::new(LeExpr::Local(mask)),
                        Box::new(i64(u64::MAX)),
                    ),
                );
                let bumped = b.assign(
                    LeType::I64,
                    add(LeExpr::Local(bump_v), LeExpr::Local(mask)),
                );
                let aligned = b.assign(
                    LeType::I64,
                    and(LeExpr::Local(bumped), LeExpr::Local(not_mask)),
                );
                let end = b.assign(
                    LeType::I64,
                    add(LeExpr::Local(aligned), word(size)),
                );
                let ok = b.temp(LeType::I32);
                b.emit(LeStmt::Call {
                    callee: LeCallee::Runtime("js_nano_arena_ensure".into()),
                    args: vec![LeExpr::Local(end)],
                    results: vec![ok],
                });
                b.emit(LeStmt::If {
                    cond: LeExpr::Unary(LeUnOp::I32Eqz, Box::new(LeExpr::Local(ok))),
                    then: vec![],
                    else_: vec![LeStmt::Trap],
                });
                b.emit(LeStmt::GlobalSet(bump, LeExpr::Local(end)));
                b.emit(LeStmt::Return(vec![LeExpr::Local(aligned)]));
            },
        );
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![size, i32c(align_log2)],
            results: vec![out],
        });
    }

    /// The null guard: `js_nano_deref(ref) -> ref`, trapping on null.
    fn deref(&self, cx: &mut InstCx<Self>, recv: LeExpr) -> LeLocal {
        let tmp = cx.assign_temp(LeType::Ref, recv);
        let helper = cx.helper(
            "js_nano_deref",
            vec![LeType::Ref],
            vec![LeType::Ref],
            |b, params| {
                let r = LeExpr::Local(params[0]);
                b.emit(LeStmt::If {
                    cond: is_null(r.clone()),
                    then: vec![LeStmt::Trap],
                    else_: vec![],
                });
                b.emit(LeStmt::Return(vec![r]));
            },
        );
        let out = cx.temp(LeType::Ref);
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![LeExpr::Local(tmp)],
            results: vec![out],
        });
        out
    }

    /// The bounds guard: `js_nano_bounds(idx: i32, len: i32) -> i32`.
    fn bounds(&self, cx: &mut InstCx<Self>, index: LeExpr, len: LeExpr) -> LeLocal {
        let idx = cx.assign_temp(LeType::I32, index);
        let len = cx.assign_temp(LeType::I32, len);
        let helper = cx.helper(
            "js_nano_bounds",
            vec![LeType::I32, LeType::I32],
            vec![LeType::I32],
            |b, params| {
                let i = LeExpr::Local(params[0]);
                let l = LeExpr::Local(params[1]);
                b.emit(LeStmt::If {
                    cond: LeExpr::Binary(LeBinOp::I32GeU, Box::new(i.clone()), Box::new(l)),
                    then: vec![LeStmt::Trap],
                    else_: vec![],
                });
                b.emit(LeStmt::Return(vec![i]));
            },
        );
        let out = cx.temp(LeType::I32);
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![LeExpr::Local(idx), LeExpr::Local(len)],
            results: vec![out],
        });
        out
    }

    /// The header's `len` field of a (non-null-checked) reference.
    fn len_of(&self, recv: LeExpr) -> LeExpr {
        LeExpr::ArenaLoad {
            ty: LeType::I32,
            base: Box::new(recv),
            offset: LEN_OFFSET,
        }
    }

    /// Element address of array element `index` (word index) with element
    /// size log2 `elem_log2`.
    fn elem_addr(&self, arr: LeExpr, index: LeExpr, elem_log2: u32) -> LeExpr {
        let idx = word(index);
        let scaled = if elem_log2 == 0 {
            idx
        } else {
            LeExpr::Binary(LeBinOp::I64Shl, Box::new(idx), Box::new(i64(elem_log2 as u64)))
        };
        add(arr, add(i64(PAYLOAD_OFFSET as u64), scaled))
    }

    /// The cast/test helpers: `js_nano_cast(ref, expected, mode, nullable)
    /// -> ref` (traps) and `js_nano_test(ref, expected, mode) -> i32`.
    /// Modes are documented at `heap_test_mode`.
    fn cast_helper(&self, cx: &mut InstCx<Self>, test_only: bool) -> LeFuncId {
        let name = if test_only {
            "js_nano_test"
        } else {
            "js_nano_cast"
        };
        let results = if test_only {
            vec![LeType::I32]
        } else {
            vec![LeType::Ref]
        };
        let mut params = vec![LeType::Ref, LeType::I32, LeType::I32];
        if !test_only {
            params.push(LeType::I32); // nullable
        }
        cx.helper(name, params, results, move |b, params| {
            let r = LeExpr::Local(params[0]);
            let expected = LeExpr::Local(params[1]);
            let mode = LeExpr::Local(params[2]);
            let nullable = if test_only {
                i32c(1) // tests admit null (result 0)
            } else {
                LeExpr::Local(params[3])
            };

            // Result helper: in test mode produce 0/1, in cast mode return
            // the reference or trap.
            let fail: Vec<LeStmt> = if test_only {
                vec![LeStmt::Return(vec![i32c(0)])]
            } else {
                vec![LeStmt::Trap]
            };
            let pass: Vec<LeStmt> = if test_only {
                vec![LeStmt::Return(vec![i32c(1)])]
            } else {
                vec![LeStmt::Return(vec![r.clone()])]
            };

            // Null.
            b.emit(LeStmt::If {
                cond: is_null(r.clone()),
                then: vec![LeStmt::If {
                    cond: nullable,
                    then: if test_only {
                        vec![LeStmt::Return(vec![i32c(0)])]
                    } else {
                        vec![LeStmt::Return(vec![r.clone()])]
                    },
                    else_: fail.clone(),
                }],
                else_: vec![],
            });
            // i31 (odd word): passes modes 3 (any/eq) and 4 (i31).
            let mode_ge_3 = LeExpr::Binary(
                LeBinOp::I32GeU,
                Box::new(mode.clone()),
                Box::new(i32c(3)),
            );
            b.emit(LeStmt::If {
                cond: is_i31(r.clone()),
                then: vec![LeStmt::If {
                    cond: mode_ge_3,
                    then: pass.clone(),
                    else_: fail.clone(),
                }],
                else_: vec![],
            });
            // Header'd reference: read the type id and dispatch on mode.
            let id = b.assign(
                LeType::I32,
                LeExpr::ArenaLoad {
                    ty: LeType::I32,
                    base: Box::new(r.clone()),
                    offset: 0,
                },
            );
            let is_mode = |m: u32| {
                LeExpr::Binary(LeBinOp::I32Eq, Box::new(mode.clone()), Box::new(i32c(m)))
            };
            // mode 0: id == expected
            b.emit(LeStmt::If {
                cond: is_mode(0),
                then: vec![LeStmt::If {
                    cond: LeExpr::Binary(
                        LeBinOp::I32Eq,
                        Box::new(LeExpr::Local(id)),
                        Box::new(expected.clone()),
                    ),
                    then: pass.clone(),
                    else_: fail.clone(),
                }],
                else_: vec![],
            });
            // mode 1: 1 <= id <= expected (any struct)
            let ge1 = LeExpr::Binary(
                LeBinOp::I32GeU,
                Box::new(LeExpr::Local(id)),
                Box::new(i32c(1)),
            );
            let le_exp = LeExpr::Binary(
                LeBinOp::I32LeU,
                Box::new(LeExpr::Local(id)),
                Box::new(expected.clone()),
            );
            b.emit(LeStmt::If {
                cond: is_mode(1),
                then: vec![LeStmt::If {
                    cond: LeExpr::Binary(LeBinOp::I32And, Box::new(ge1), Box::new(le_exp)),
                    then: pass.clone(),
                    else_: fail.clone(),
                }],
                else_: vec![],
            });
            // mode 2: id > expected (any array; expected == struct_count)
            b.emit(LeStmt::If {
                cond: is_mode(2),
                then: vec![LeStmt::If {
                    cond: LeExpr::Binary(
                        LeBinOp::I32GtU,
                        Box::new(LeExpr::Local(id)),
                        Box::new(expected.clone()),
                    ),
                    then: pass.clone(),
                    else_: fail.clone(),
                }],
                else_: vec![],
            });
            // modes 3/4: any non-null header'd reference passes mode 3;
            // mode 4 (i31-only) fails.
            b.emit(LeStmt::If {
                cond: LeExpr::Binary(
                    LeBinOp::I32Eq,
                    Box::new(mode.clone()),
                    Box::new(i32c(3)),
                ),
                then: pass,
                else_: fail,
            });
        })
    }
}

impl Representation for NanoRepresentation {
    fn name(&self) -> &'static str {
        "nano"
    }

    fn collects(&self) -> bool {
        false
    }

    fn lower_type(&self, plan: &BackendPlan, ty: TypeId) -> Result<LeType, LeDiag> {
        match plan.types.get(ty.index()) {
            Some(crate::backend_plan::PlanType::I32) => Ok(LeType::I32),
            Some(crate::backend_plan::PlanType::I64) => Ok(LeType::I64),
            Some(crate::backend_plan::PlanType::F32) => Ok(LeType::F32),
            Some(crate::backend_plan::PlanType::F64) => Ok(LeType::F64),
            Some(crate::backend_plan::PlanType::Ref { .. }) => Ok(LeType::Ref),
            Some(other) => Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("plan type {other:?} is outside the nano surface"),
            }),
            None => Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("plan type {ty} is out of range"),
            }),
        }
    }

    fn declare(&self, _plan: &BackendPlan, module: &mut LeModule) -> Result<(), LeDiag> {
        // The bump pointer global follows the plan globals (see
        // `bump_global`).
        module.globals.push((LeType::I64, Some(ARENA_BASE)));
        Ok(())
    }

    fn emit_runtime(&self, _plan: &BackendPlan, sink: &mut RuntimeSink) -> Result<(), LeDiag> {
        sink.runtime_fn(
            "js_nano_arena_ensure",
            vec![LeType::I64],
            vec![LeType::I32],
        );
        Ok(())
    }

    // -- aggregates -------------------------------------------------------------

    fn struct_new(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        fields: Option<&[LeExpr]>,
        out: LeLocal,
    ) -> Result<(), LeDiag> {
        let layout = self.layout(cx, sig)?;
        self.alloc(
            cx,
            i32c(layout.fixed_size),
            layout.align_log2,
            out,
        );
        cx.emit(LeStmt::ArenaStore {
            ty: LeType::I32,
            base: LeExpr::Local(out),
            offset: 0,
            value: i32c(layout.type_id),
        });
        cx.emit(LeStmt::ArenaStore {
            ty: LeType::I32,
            base: LeExpr::Local(out),
            offset: LEN_OFFSET,
            value: i32c(0),
        });
        for (i, offset) in layout.field_offsets.iter().enumerate() {
            let ty = layout.field_tys[i];
            let value = match fields {
                Some(fields) => fields[i].clone(),
                None => zero_of(ty),
            };
            cx.emit(LeStmt::ArenaStore {
                ty,
                base: LeExpr::Local(out),
                offset: *offset,
                value,
            });
        }
        Ok(())
    }

    fn struct_get(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        field: u32,
        recv: LeExpr,
        out: LeLocal,
    ) -> Result<(), LeDiag> {
        let layout = self.layout(cx, sig)?;
        let Some(&offset) = layout.field_offsets.get(field as usize) else {
            return Err(cx_diag(cx, format!("field {field} out of range")));
        };
        let ty = layout.field_tys[field as usize];
        let base = self.deref(cx, recv);
        cx.emit(LeStmt::Assign(
            out,
            LeExpr::ArenaLoad {
                ty,
                base: Box::new(LeExpr::Local(base)),
                offset,
            },
        ));
        Ok(())
    }

    fn struct_set(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        field: u32,
        recv: LeExpr,
        value: LeExpr,
    ) -> Result<(), LeDiag> {
        let layout = self.layout(cx, sig)?;
        let Some(&offset) = layout.field_offsets.get(field as usize) else {
            return Err(cx_diag(cx, format!("field {field} out of range")));
        };
        let ty = layout.field_tys[field as usize];
        let base = self.deref(cx, recv);
        cx.emit(LeStmt::ArenaStore {
            ty,
            base: LeExpr::Local(base),
            offset,
            value,
        });
        Ok(())
    }

    fn array_new(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        len: LeExpr,
        items: Option<&[LeExpr]>,
        out: LeLocal,
    ) -> Result<(), LeDiag> {
        let layout = self.layout(cx, sig)?;
        let Some((elem_log2, elem_ty)) = layout.elem else {
            return Err(cx_diag(cx, format!("signature {sig} is not an array")));
        };
        // size = 8 + (len << elem_log2), computed in word width.
        let len_tmp = cx.assign_temp(LeType::I32, len);
        let scaled = if elem_log2 == 0 {
            word(LeExpr::Local(len_tmp))
        } else {
            LeExpr::Binary(
                LeBinOp::I64Shl,
                Box::new(word(LeExpr::Local(len_tmp))),
                Box::new(i64(elem_log2 as u64)),
            )
        };
        let size = cx.assign_temp(
            LeType::I64,
            add(i64(PAYLOAD_OFFSET as u64), scaled),
        );
        let size32 = cx.assign_temp(
            LeType::I32,
            LeExpr::Unary(LeUnOp::I32WrapI64, Box::new(LeExpr::Local(size))),
        );
        self.alloc(cx, LeExpr::Local(size32), layout.align_log2, out);
        cx.emit(LeStmt::ArenaStore {
            ty: LeType::I32,
            base: LeExpr::Local(out),
            offset: 0,
            value: i32c(layout.type_id),
        });
        cx.emit(LeStmt::ArenaStore {
            ty: LeType::I32,
            base: LeExpr::Local(out),
            offset: LEN_OFFSET,
            value: LeExpr::Local(len_tmp),
        });
        if let Some(items) = items {
            for (i, item) in items.iter().enumerate() {
                let addr = addr_of(
                    LeExpr::Local(out),
                    PAYLOAD_OFFSET + (i as u32) << elem_log2,
                );
                cx.emit(LeStmt::ArenaStore {
                    ty: elem_ty,
                    base: addr,
                    offset: 0,
                    value: item.clone(),
                });
            }
        }
        // ArrayNewDefault relies on the arena zero-fill contract (module
        // header).
        Ok(())
    }

    fn array_get(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        arr: LeExpr,
        index: LeExpr,
        packed: Option<bool>,
        out: LeLocal,
    ) -> Result<(), LeDiag> {
        let layout = self.layout(cx, sig)?;
        let Some((elem_log2, elem_ty)) = layout.elem else {
            return Err(cx_diag(cx, format!("signature {sig} is not an array")));
        };
        let base = self.deref(cx, arr);
        let len = self.len_of(LeExpr::Local(base));
        let idx = self.bounds(cx, index, len);
        let addr = self.elem_addr(LeExpr::Local(base), LeExpr::Local(idx), elem_log2);
        let raw = cx.assign_temp(
            LeType::I32,
            LeExpr::ArenaLoad {
                ty: elem_ty,
                base: Box::new(addr),
                offset: 0,
            },
        );
        let value = match (packed, elem_ty) {
            (Some(true), LeType::I8) => {
                LeExpr::Unary(LeUnOp::I32Extend8S, Box::new(LeExpr::Local(raw)))
            }
            (Some(false), LeType::I8) => LeExpr::Local(raw),
            (Some(true), LeType::I16) => {
                LeExpr::Unary(LeUnOp::I32Extend16S, Box::new(LeExpr::Local(raw)))
            }
            (Some(false), LeType::I16) => LeExpr::Local(raw),
            _ => LeExpr::Local(raw),
        };
        cx.emit(LeStmt::Assign(out, value));
        Ok(())
    }

    fn array_set(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        arr: LeExpr,
        index: LeExpr,
        value: LeExpr,
    ) -> Result<(), LeDiag> {
        let layout = self.layout(cx, sig)?;
        let Some((elem_log2, elem_ty)) = layout.elem else {
            return Err(cx_diag(cx, format!("signature {sig} is not an array")));
        };
        let base = self.deref(cx, arr);
        let len = self.len_of(LeExpr::Local(base));
        let idx = self.bounds(cx, index, len);
        let addr = self.elem_addr(LeExpr::Local(base), LeExpr::Local(idx), elem_log2);
        cx.emit(LeStmt::ArenaStore {
            ty: elem_ty,
            base: addr,
            offset: 0,
            value,
        });
        Ok(())
    }

    fn array_len(&self, cx: &mut InstCx<Self>, arr: LeExpr, out: LeLocal) -> Result<(), LeDiag> {
        let base = self.deref(cx, arr);
        cx.emit(LeStmt::Assign(out, self.len_of(LeExpr::Local(base))));
        Ok(())
    }

    fn array_copy(
        &self,
        cx: &mut InstCx<Self>,
        dst: LeExpr,
        dst_index: LeExpr,
        src: LeExpr,
        src_index: LeExpr,
        len: LeExpr,
    ) -> Result<(), LeDiag> {
        // Byte-wise memmove helper; element size is irrelevant to the copy
        // loop because the caller scales indices and length to bytes first.
        // We keep the wasm array.copy signature (element units) and scale
        // here.
        let helper = cx.helper(
            "js_nano_array_copy",
            vec![LeType::Ref, LeType::I32, LeType::Ref, LeType::I32, LeType::I32],
            vec![],
            |b, params| {
                let dst = LeExpr::Local(params[0]);
                let di = LeExpr::Local(params[1]);
                let src = LeExpr::Local(params[2]);
                let si = LeExpr::Local(params[3]);
                let len = LeExpr::Local(params[4]);
                // Bounds: di+len <= dst.len, si+len <= src.len (word math to
                // avoid overflow).
                let dlen = b.assign(
                    LeType::I32,
                    LeExpr::ArenaLoad {
                        ty: LeType::I32,
                        base: Box::new(dst.clone()),
                        offset: LEN_OFFSET,
                    },
                );
                let slen = b.assign(
                    LeType::I32,
                    LeExpr::ArenaLoad {
                        ty: LeType::I32,
                        base: Box::new(src.clone()),
                        offset: LEN_OFFSET,
                    },
                );
                let dend = b.assign(
                    LeType::I64,
                    add(word(di.clone()), word(len.clone())),
                );
                let send = b.assign(
                    LeType::I64,
                    add(word(si.clone()), word(len.clone())),
                );
                b.emit(LeStmt::If {
                    cond: LeExpr::Binary(
                        LeBinOp::I64GtU,
                        Box::new(LeExpr::Local(dend)),
                        Box::new(word(LeExpr::Local(dlen))),
                    ),
                    then: vec![LeStmt::Trap],
                    else_: vec![],
                });
                b.emit(LeStmt::If {
                    cond: LeExpr::Binary(
                        LeBinOp::I64GtU,
                        Box::new(LeExpr::Local(send)),
                        Box::new(word(LeExpr::Local(slen))),
                    ),
                    then: vec![LeStmt::Trap],
                    else_: vec![],
                });
                // Byte addresses.
                let daddr = b.assign(
                    LeType::I64,
                    add(
                        dst.clone(),
                        add(i64(PAYLOAD_OFFSET as u64), word(di.clone())),
                    ),
                );
                let saddr = b.assign(
                    LeType::I64,
                    add(
                        src.clone(),
                        add(i64(PAYLOAD_OFFSET as u64), word(si.clone())),
                    ),
                );
                // Forward when daddr < saddr, else backward (memmove).
                let i = b.temp(LeType::I32);
                let one_i32 = i32c(1);
                let forward = vec![
                    LeStmt::Loop(vec![
                        LeStmt::If {
                            cond: LeExpr::Binary(
                                LeBinOp::I32GeU,
                                Box::new(LeExpr::Local(i)),
                                Box::new(len.clone()),
                            ),
                            then: vec![LeStmt::Break],
                            else_: vec![],
                        },
                        LeStmt::ArenaStore {
                            ty: LeType::I8,
                            base: add(LeExpr::Local(daddr), word(LeExpr::Local(i))),
                            offset: 0,
                            value: LeExpr::ArenaLoad {
                                ty: LeType::I8,
                                base: Box::new(add(LeExpr::Local(saddr), word(LeExpr::Local(i)))),
                                offset: 0,
                            },
                        },
                        LeStmt::Assign(
                            i,
                            LeExpr::Binary(
                                LeBinOp::I32Add,
                                Box::new(LeExpr::Local(i)),
                                Box::new(one_i32.clone()),
                            ),
                        ),
                    ]),
                ];
                // i starts at 0 (forward) or len (backward).
                b.emit(LeStmt::Assign(i, i32c(0)));
                let backward_init = vec![LeStmt::Assign(i, len.clone())];
                let backward = vec![
                    LeStmt::Loop(vec![
                        LeStmt::If {
                            cond: LeExpr::Unary(
                                LeUnOp::I32Eqz,
                                Box::new(LeExpr::Local(i)),
                            ),
                            then: vec![LeStmt::Break],
                            else_: vec![],
                        },
                        LeStmt::Assign(
                            i,
                            LeExpr::Binary(
                                LeBinOp::I32Sub,
                                Box::new(LeExpr::Local(i)),
                                Box::new(i32c(1)),
                            ),
                        ),
                        LeStmt::ArenaStore {
                            ty: LeType::I8,
                            base: add(LeExpr::Local(daddr), word(LeExpr::Local(i))),
                            offset: 0,
                            value: LeExpr::ArenaLoad {
                                ty: LeType::I8,
                                base: Box::new(add(LeExpr::Local(saddr), word(LeExpr::Local(i)))),
                                offset: 0,
                            },
                        },
                    ]),
                ];
                let backward_full: Vec<LeStmt> =
                    backward_init.into_iter().chain(backward).collect();
                b.emit(LeStmt::If {
                    cond: LeExpr::Binary(
                        LeBinOp::I64LtU,
                        Box::new(LeExpr::Local(daddr)),
                        Box::new(LeExpr::Local(saddr)),
                    ),
                    then: forward,
                    else_: backward_full,
                });
                b.emit(LeStmt::Return(vec![]));
            },
        );
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![dst, dst_index, src, src_index, len],
            results: vec![],
        });
        Ok(())
    }

    // -- reference tests and casts ----------------------------------------------

    fn ref_test(
        &self,
        cx: &mut InstCx<Self>,
        heap: &PlanHeapType,
        _nullable: bool,
        recv: LeExpr,
        out: LeLocal,
    ) -> Result<(), LeDiag> {
        let (mode, expected) = heap_test_mode(cx.t.plan, cx.t.type_ids, heap).map_err(|m| cx_diag(cx, m))?;
        let helper = self.cast_helper(cx, true);
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![recv, i32c(expected), i32c(mode)],
            results: vec![out],
        });
        Ok(())
    }

    fn ref_cast(
        &self,
        cx: &mut InstCx<Self>,
        heap: &PlanHeapType,
        nullable: bool,
        recv: LeExpr,
        out: LeLocal,
    ) -> Result<(), LeDiag> {
        let (mode, expected) = heap_test_mode(cx.t.plan, cx.t.type_ids, heap).map_err(|m| cx_diag(cx, m))?;
        let helper = self.cast_helper(cx, false);
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![
                recv,
                i32c(expected),
                i32c(mode),
                i32c(nullable as u32),
            ],
            results: vec![out],
        });
        Ok(())
    }

    // -- function references -------------------------------------------------------

    fn funcref_value(&self, _target: PlanFuncId, slot: u32) -> LeExpr {
        i64(slot as u64)
    }

    fn callable(&self, value: LeExpr, _sig: PlanSigId) -> LeCallee {
        LeCallee::Ptr(Box::new(LeExpr::TableElem {
            table: 0,
            index: Box::new(value),
        }))
    }

    // -- trampoline frames ----------------------------------------------------------

    fn frame_alloc(&self, cx: &mut InstCx<Self>, slots: u32, out: LeLocal) -> Result<(), LeDiag> {
        self.alloc(cx, i32c(8 * slots), 3, out);
        // Zero the callee slot so a partially-filled frame never dispatches.
        cx.emit(LeStmt::ArenaStore {
            ty: LeType::I64,
            base: LeExpr::Local(out),
            offset: 0,
            value: i64(0),
        });
        Ok(())
    }

    fn frame_store(&self, cx: &mut InstCx<Self>, frame: &LeLocal, slot: u32, value: LeExpr) {
        cx.emit(LeStmt::ArenaStore {
            ty: LeType::I64,
            base: LeExpr::Local(*frame),
            offset: slot * 8,
            value,
        });
    }

    fn frame_load(&self, _cx: &mut InstCx<Self>, frame: &LeLocal, slot: u32) -> LeExpr {
        LeExpr::ArenaLoad {
            ty: LeType::I64,
            base: Box::new(LeExpr::Local(*frame)),
            offset: slot * 8,
        }
    }
}

fn cx_diag<R: Representation>(cx: &InstCx<R>, message: String) -> LeDiag {
    LeDiag {
        function: cx.at.0,
        block: cx.at.1,
        inst: cx.at.2,
        message,
    }
}
