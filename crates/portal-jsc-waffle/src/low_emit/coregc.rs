//! The CoreGC representation (DreamComp #36): thin header'd references with
//! a precise collector.
//!
//! References are one word: a native pointer to an allocation whose header
//! `{ type_id: u32, flags: u32, len: u32, pad: u32 }` (layout 0) precedes the
//! payload. The zero word is null and odd words are i31 payloads (module
//! header of `low_emit`). Layouts are declared per aggregate signature so
//! targets render native structs/arrays; the skeleton's root-discipline
//! statements (`RootFrameEnter`/`RootStore`/`RootFramePop`) give the
//! collector an exact root set at every allocation and call.
//!
//! ## Runtime contract (implemented per target, e.g. the C runtime in #38)
//!
//! - `js_coregc_alloc(type_id: i32, len: i32) -> ref`: allocate a zero-filled
//!   object of `type_id` (`len` is the element count for array layouts,
//!   ignored for structs), running the collector as needed; traps
//!   deterministically on exhaustion. Zero-fill is required (the emitter
//!   relies on it for `StructNewDefault`/`ArrayNewDefault` payloads).
//! - `js_coregc_root_enter(slots: i32)` / `js_coregc_root_store(slot: i32,
//!   value: ref)` / `js_coregc_root_pop()`: the shadow root stack backing
//!   the skeleton's root-discipline statements. Root frames nest with calls;
//!   trap exits may leak frames — the runtime resets its shadow stack at the
//!   artifact entry boundary.
//! - `js_coregc_array_copy(dst: ref, di: i32, src: ref, si: i32, len: i32,
//!   elem_log2: i32)`: bounds-checked memmove (wasm `array.copy` semantics).
//!
//! The collector finds reference fields through the descriptor static
//! `js_coregc_descriptors`: a u32 row count followed by one row per
//! non-header layout `{ type_id, is_array, elem_is_ref, n_ref_fields,
//! ref_field_indices... }` (all u32, little-endian; `ref_field_indices` is
//! empty for arrays).
//!
//! Tail-call frames are GC objects of the reserved frame layout (an i64
//! array; descriptor says no reference elements): erased slot words that
//! hold references are never scanned — between step calls no allocation can
//! run (the wrapper/driver loops contain none), and during a step the
//! values are rooted via the step's own locals.

use super::{
    InstCx, LeBinOp, LeCallee, LeDiag, LeExpr, LeFuncId, LeLayout, LeLocal, LeModule, LeStatic,
    LeStmt, LeType, LeUnOp, Representation, RuntimeSink, heap_test_mode, zero_of,
};
use crate::backend_plan::{
    BackendPlan, PlanFuncId, PlanHeapType, PlanSigId, PlanSignature, PlanStorage, TypeId,
};

/// Header layout index (always first).
pub const HEADER_LAYOUT: u32 = 0;
/// Header field indices.
const HEADER_TYPE_FIELD: u32 = 0;
const HEADER_LEN_FIELD: u32 = 2;

/// The CoreGC representation: thin header'd pointers with precise
/// collection.
pub struct CoreGcRepresentation;

fn i32c(v: u32) -> LeExpr {
    LeExpr::ConstI32(v)
}

fn i64(v: u64) -> LeExpr {
    LeExpr::ConstI64(v)
}

fn is_null(v: LeExpr) -> LeExpr {
    LeExpr::Binary(LeBinOp::I64Eq, Box::new(v), Box::new(i64(0)))
}

fn is_i31(v: LeExpr) -> LeExpr {
    LeExpr::Binary(
        LeBinOp::I64Ne,
        Box::new(LeExpr::Binary(LeBinOp::I64And, Box::new(v), Box::new(i64(1)))),
        Box::new(i64(0)),
    )
}

impl CoreGcRepresentation {
    fn lower_storage(&self, plan: &BackendPlan, storage: &PlanStorage) -> Result<LeType, LeDiag> {
        match storage {
            PlanStorage::I8 => Ok(LeType::I8),
            PlanStorage::I16 => Ok(LeType::I16),
            PlanStorage::Val(ty) => self.lower_type(plan, *ty),
        }
    }

    /// The layout index of an aggregate signature: 1 + its position among
    /// the plan's aggregate signatures (layout 0 is the header).
    fn layout_index(&self, plan: &BackendPlan, sig: PlanSigId) -> Option<u32> {
        let mut n = 1u32;
        for (i, s) in plan.signatures.iter().enumerate() {
            match s {
                PlanSignature::Struct { .. } | PlanSignature::Array { .. } => {
                    if i == sig.index() {
                        return Some(n);
                    }
                    n += 1;
                }
                _ => {}
            }
        }
        None
    }

    /// The reserved type id and layout index of tail-call frames (after all
    /// plan aggregates).
    fn frame_type(&self, cx: &InstCx<Self>) -> (u32, u32) {
        let count = cx.t.type_ids.struct_count + cx.t.type_ids.array_count;
        (count + 1, 1 + count)
    }

    /// `js_coregc_deref(ref) -> ref`: trap on null.
    fn deref(&self, cx: &mut InstCx<Self>, recv: LeExpr) -> LeLocal {
        let tmp = cx.assign_temp(LeType::Ref, recv);
        let helper = cx.helper(
            "js_coregc_deref",
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

    /// `js_coregc_bounds(idx: i32, len: i32) -> i32`.
    fn bounds(&self, cx: &mut InstCx<Self>, index: LeExpr, len: LeExpr) -> LeLocal {
        let idx = cx.assign_temp(LeType::I32, index);
        let len = cx.assign_temp(LeType::I32, len);
        let helper = cx.helper(
            "js_coregc_bounds",
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

    fn header_field(&self, recv: LeExpr, field: u32) -> LeExpr {
        LeExpr::Field {
            layout: HEADER_LAYOUT,
            field,
            base: Box::new(recv),
        }
    }

    /// `js_coregc_test(ref, expected, mode) -> i32` /
    /// `js_coregc_cast(ref, expected, mode, nullable) -> ref`.
    fn cast_helper(&self, cx: &mut InstCx<Self>, test_only: bool) -> super::LeFuncId {
        let name = if test_only {
            "js_coregc_test"
        } else {
            "js_coregc_cast"
        };
        let results = if test_only {
            vec![LeType::I32]
        } else {
            vec![LeType::Ref]
        };
        let mut params = vec![LeType::Ref, LeType::I32, LeType::I32];
        if !test_only {
            params.push(LeType::I32);
        }
        cx.helper(name, params, results, move |b, params| {
            let r = LeExpr::Local(params[0]);
            let expected = LeExpr::Local(params[1]);
            let mode = LeExpr::Local(params[2]);
            let nullable = if test_only {
                i32c(1)
            } else {
                LeExpr::Local(params[3])
            };
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
            let mode_34 = LeExpr::Binary(
                LeBinOp::I32Or,
                Box::new(LeExpr::Binary(
                    LeBinOp::I32Eq,
                    Box::new(mode.clone()),
                    Box::new(i32c(3)),
                )),
                Box::new(LeExpr::Binary(
                    LeBinOp::I32Eq,
                    Box::new(mode.clone()),
                    Box::new(i32c(4)),
                )),
            );
            b.emit(LeStmt::If {
                cond: is_i31(r.clone()),
                then: vec![LeStmt::If {
                    cond: mode_34,
                    then: pass.clone(),
                    else_: fail.clone(),
                }],
                else_: vec![],
            });
            let id = b.assign(
                LeType::I32,
                LeExpr::Field {
                    layout: HEADER_LAYOUT,
                    field: HEADER_TYPE_FIELD,
                    base: Box::new(r.clone()),
                },
            );
            let is_mode = |m: u32| {
                LeExpr::Binary(LeBinOp::I32Eq, Box::new(mode.clone()), Box::new(i32c(m)))
            };
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
            b.emit(LeStmt::If {
                cond: is_mode(3),
                then: pass,
                else_: fail,
            });
        })
    }

    /// The allocation call.
    fn alloc(&self, cx: &mut InstCx<Self>, type_id: u32, len: LeExpr, out: LeLocal) {
        cx.emit(LeStmt::Call {
            callee: LeCallee::Runtime("js_coregc_alloc".into()),
            args: vec![i32c(type_id), len],
            results: vec![out],
        });
    }
}

impl Representation for CoreGcRepresentation {
    fn name(&self) -> &'static str {
        "coregc"
    }

    fn collects(&self) -> bool {
        true
    }

    fn lower_type(&self, plan: &BackendPlan, ty: TypeId) -> Result<LeType, LeDiag> {
        match plan.types.get(ty.index()) {
            Some(crate::backend_plan::PlanType::I32) => Ok(LeType::I32),
            Some(crate::backend_plan::PlanType::I64) => Ok(LeType::I64),
            Some(crate::backend_plan::PlanType::F32) => Ok(LeType::F32),
            Some(crate::backend_plan::PlanType::F64) => Ok(LeType::F64),
            // Funcref words (bare or typed `(ref $funcsig)`) are table-slot
            // indices, not heap references: they must never be rooted,
            // scanned, or field-tracked.
            Some(crate::backend_plan::PlanType::Ref { heap, .. })
                if super::plan_heap_is_func_word(plan, heap) =>
            {
                Ok(LeType::I64)
            }
            Some(crate::backend_plan::PlanType::Ref { .. }) => Ok(LeType::Ref),
            Some(other) => Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("plan type {other:?} is outside the coregc surface"),
            }),
            None => Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("plan type {ty} is out of range"),
            }),
        }
    }

    fn declare(&self, plan: &BackendPlan, module: &mut LeModule) -> Result<(), LeDiag> {
        let type_ids = super::TypeIdTable::build(plan);
        // Ref-typed module globals would be invisible to the collector;
        // fail closed (the corpus has none).
        for (i, g) in plan.globals.iter().enumerate() {
            if let Ok(LeType::Ref) = self.lower_type(plan, g.ty) {
                return Err(LeDiag {
                    function: None,
                    block: None,
                    inst: None,
                    message: format!(
                        "coregc does not yet support ref-typed module globals (global {i})"
                    ),
                });
            }
        }

        // Layout 0: the allocation header.
        module.layouts.push(LeLayout {
            type_id: 0,
            fields: vec![LeType::I32, LeType::I32, LeType::I32, LeType::I32],
            array_element: None,
        });

        // One layout per aggregate signature, in plan signature order.
        let mut descriptor: Vec<u8> = Vec::new();
        let mut rows: Vec<Vec<u8>> = Vec::new();
        for (i, sig) in plan.signatures.iter().enumerate() {
            match sig {
                PlanSignature::Struct { fields, .. } => {
                    let id = type_ids.ids[&(i as u32)];
                    let mut lowered = Vec::new();
                    let mut ref_fields: Vec<u32> = Vec::new();
                    for (fi, field) in fields.iter().enumerate() {
                        let ty = self.lower_storage(plan, &field.storage)?;
                        if ty.is_ref() {
                            ref_fields.push(fi as u32);
                        }
                        lowered.push(ty);
                    }
                    module.layouts.push(LeLayout {
                        type_id: id,
                        fields: lowered,
                        array_element: None,
                    });
                    let mut row = Vec::new();
                    row.extend_from_slice(&id.to_le_bytes());
                    row.extend_from_slice(&0u32.to_le_bytes());
                    row.extend_from_slice(&0u32.to_le_bytes());
                    row.extend_from_slice(&(ref_fields.len() as u32).to_le_bytes());
                    for fi in ref_fields {
                        row.extend_from_slice(&fi.to_le_bytes());
                    }
                    rows.push(row);
                }
                PlanSignature::Array { element, .. } => {
                    let id = type_ids.ids[&(i as u32)];
                    let ty = self.lower_storage(plan, &element.storage)?;
                    module.layouts.push(LeLayout {
                        type_id: id,
                        fields: Vec::new(),
                        array_element: Some(ty),
                    });
                    let mut row = Vec::new();
                    row.extend_from_slice(&id.to_le_bytes());
                    row.extend_from_slice(&1u32.to_le_bytes());
                    row.extend_from_slice(&(ty.is_ref() as u32).to_le_bytes());
                    row.extend_from_slice(&0u32.to_le_bytes());
                    rows.push(row);
                }
                _ => {}
            }
        }

        // The frame layout (i64 slot array, no reference elements).
        let frame_id = type_ids.struct_count + type_ids.array_count + 1;
        module.layouts.push(LeLayout {
            type_id: frame_id,
            fields: Vec::new(),
            array_element: Some(LeType::I64),
        });
        let mut row = Vec::new();
        row.extend_from_slice(&frame_id.to_le_bytes());
        row.extend_from_slice(&1u32.to_le_bytes());
        row.extend_from_slice(&0u32.to_le_bytes());
        row.extend_from_slice(&0u32.to_le_bytes());
        rows.push(row);

        descriptor.extend_from_slice(&(rows.len() as u32).to_le_bytes());
        for row in rows {
            descriptor.extend_from_slice(&row);
        }
        module.statics.push(LeStatic {
            name: "js_coregc_descriptors".into(),
            bytes: descriptor,
        });
        Ok(())
    }

    fn emit_runtime(&self, _plan: &BackendPlan, sink: &mut RuntimeSink) -> Result<(), LeDiag> {
        sink.runtime_fn(
            "js_coregc_alloc",
            vec![LeType::I32, LeType::I32],
            vec![LeType::Ref],
        );
        sink.runtime_fn("js_coregc_root_enter", vec![LeType::I32], vec![]);
        sink.runtime_fn("js_coregc_root_store", vec![LeType::I32, LeType::Ref], vec![]);
        sink.runtime_fn("js_coregc_root_pop", vec![], vec![]);
        sink.runtime_fn(
            "js_coregc_array_copy",
            vec![
                LeType::Ref,
                LeType::I32,
                LeType::Ref,
                LeType::I32,
                LeType::I32,
                LeType::I32,
            ],
            vec![],
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
        let layout = self.layout_index(cx.t.plan, sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        let type_id = cx.t.type_ids.of(sig).expect("aggregate has a type id");
        self.alloc(cx, type_id, i32c(0), out);
        let field_tys = match &cx.t.plan.signatures[sig.index()] {
            PlanSignature::Struct { fields, .. } => fields
                .iter()
                .map(|f| self.lower_storage(cx.t.plan, &f.storage))
                .collect::<Result<Vec<_>, _>>()?,
            _ => {
                return Err(LeDiag {
                    function: cx.at.0,
                    block: cx.at.1,
                    inst: cx.at.2,
                    message: format!("signature {sig} is not a struct"),
                });
            }
        };
        for (i, ty) in field_tys.iter().enumerate() {
            let value = match fields {
                Some(fields) => fields[i].clone(),
                None => zero_of(*ty),
            };
            cx.emit(LeStmt::SetField {
                layout,
                field: i as u32,
                base: LeExpr::Local(out),
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
        let layout = self.layout_index(cx.t.plan, sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        let base = self.deref(cx, recv);
        cx.emit(LeStmt::Assign(
            out,
            LeExpr::Field {
                layout,
                field,
                base: Box::new(LeExpr::Local(base)),
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
        let layout = self.layout_index(cx.t.plan, sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        let base = self.deref(cx, recv);
        cx.emit(LeStmt::SetField {
            layout,
            field,
            base: LeExpr::Local(base),
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
        let layout = self.layout_index(cx.t.plan, sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        let type_id = cx.t.type_ids.of(sig).expect("aggregate has a type id");
        let len_tmp = cx.assign_temp(LeType::I32, len);
        self.alloc(cx, type_id, LeExpr::Local(len_tmp), out);
        if let Some(items) = items {
            for (i, item) in items.iter().enumerate() {
                cx.emit(LeStmt::SetIndex {
                    layout,
                    base: LeExpr::Local(out),
                    index: i32c(i as u32),
                    value: item.clone(),
                });
            }
        }
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
        let layout = self.layout_index(cx.t.plan, sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        let base = self.deref(cx, arr);
        let len = self.header_field(LeExpr::Local(base), HEADER_LEN_FIELD);
        let idx = self.bounds(cx, index, len);
        let elem_ty = match &cx.t.plan.signatures[sig.index()] {
            PlanSignature::Array { element, .. } => {
                self.lower_storage(cx.t.plan, &element.storage)?
            }
            _ => {
                return Err(LeDiag {
                    function: cx.at.0,
                    block: cx.at.1,
                    inst: cx.at.2,
                    message: format!("signature {sig} is not an array"),
                });
            }
        };
        let raw_ty = match (packed, elem_ty) {
            (Some(_), LeType::I8 | LeType::I16) => LeType::I32,
            _ => elem_ty,
        };
        let raw = cx.assign_temp(
            raw_ty,
            LeExpr::Index {
                layout,
                base: Box::new(LeExpr::Local(base)),
                index: Box::new(LeExpr::Local(idx)),
            },
        );
        let value = match (packed, elem_ty) {
            (Some(true), LeType::I8) => {
                LeExpr::Unary(LeUnOp::I32Extend8S, Box::new(LeExpr::Local(raw)))
            }
            (Some(true), LeType::I16) => {
                LeExpr::Unary(LeUnOp::I32Extend16S, Box::new(LeExpr::Local(raw)))
            }
            (Some(false), LeType::I8) => {
                LeExpr::Unary(LeUnOp::I32Extend8U, Box::new(LeExpr::Local(raw)))
            }
            (Some(false), LeType::I16) => {
                LeExpr::Unary(LeUnOp::I32Extend16U, Box::new(LeExpr::Local(raw)))
            }
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
        let layout = self.layout_index(cx.t.plan, sig).ok_or_else(|| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: format!("signature {sig} is not an aggregate"),
        })?;
        let base = self.deref(cx, arr);
        let len = self.header_field(LeExpr::Local(base), HEADER_LEN_FIELD);
        let idx = self.bounds(cx, index, len);
        cx.emit(LeStmt::SetIndex {
            layout,
            base: LeExpr::Local(base),
            index: LeExpr::Local(idx),
            value,
        });
        Ok(())
    }

    fn array_len(&self, cx: &mut InstCx<Self>, arr: LeExpr, out: LeLocal) -> Result<(), LeDiag> {
        let base = self.deref(cx, arr);
        cx.emit(LeStmt::Assign(
            out,
            self.header_field(LeExpr::Local(base), HEADER_LEN_FIELD),
        ));
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
        // The runtime's copy is element-size aware through `elem_log2`;
        // jsaw's own arrays are word-sized, and the corpus only copies those
        // — the runtime enforces the bound checks either way.
        cx.emit(LeStmt::Call {
            callee: LeCallee::Runtime("js_coregc_array_copy".into()),
            args: vec![dst, dst_index, src, src_index, len, i32c(3)],
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
        let (mode, expected) = heap_test_mode(cx.t.plan, cx.t.type_ids, heap).map_err(|m| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: m,
        })?;
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
        let (mode, expected) = heap_test_mode(cx.t.plan, cx.t.type_ids, heap).map_err(|m| LeDiag {
            function: cx.at.0,
            block: cx.at.1,
            inst: cx.at.2,
            message: m,
        })?;
        let helper = self.cast_helper(cx, false);
        cx.emit(LeStmt::Call {
            callee: LeCallee::Direct(helper),
            args: vec![recv, i32c(expected), i32c(mode), i32c(nullable as u32)],
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
        let (frame_type, _) = self.frame_type(cx);
        self.alloc(cx, frame_type, i32c(slots), out);
        // Zero the callee slot so a partially-filled frame never dispatches.
        let (_, frame_layout) = self.frame_type(cx);
        cx.emit(LeStmt::SetIndex {
            layout: frame_layout,
            base: LeExpr::Local(out),
            index: i32c(0),
            value: i64(0),
        });
        Ok(())
    }

    fn frame_store(&self, cx: &mut InstCx<Self>, frame: &LeLocal, slot: u32, value: LeExpr) {
        let (_, frame_layout) = self.frame_type(cx);
        cx.emit(LeStmt::SetIndex {
            layout: frame_layout,
            base: LeExpr::Local(*frame),
            index: i32c(slot),
            value,
        });
    }

    fn frame_load(&self, cx: &mut InstCx<Self>, frame: &LeLocal, slot: u32) -> LeExpr {
        let (_, frame_layout) = self.frame_type(cx);
        LeExpr::Index {
            layout: frame_layout,
            base: Box::new(LeExpr::Local(*frame)),
            index: Box::new(i32c(slot)),
        }
    }
}
