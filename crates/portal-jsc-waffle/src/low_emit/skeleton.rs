//! The shared low-level emitter skeleton (DreamComp #36).
//!
//! Walks a [`BackendPlan`] and produces a target-neutral [`LeModule`],
//! owning scalar lowering, the flat-CFG translation with parallel-move
//! edges, direct/indirect/ref call plumbing, the step-protocol tail-call
//! trampoline, boundary adaptation, and CoreGC root discipline. All
//! representation decisions go through the [`Representation`] trait — see
//! the [`crate::low_emit`] module header for the design contract.

use super::{
    FnState, InstCx, LeBinOp, LeBlock, LeBlockId, LeBody, LeCallee, LeDiag, LeExport, LeExpr,
    LeFunc, LeFuncId, LeImport, LeLocal, LeModule, LeStmt, LeTable, LeType, LeUnOp,
    LOW_EMIT_SCHEMA, Representation, RuntimeSink, Tables, TypeIdTable, scalar_binop, scalar_unop,
    trapping_unop,
};
use crate::backend_plan::{
    BackendPlan, BoundaryDirection, BoundaryTarget, PlanCall, PlanCallKind, PlanConst,
    PlanFuncId, PlanFunction, PlanFunctionKind, PlanInst, PlanSignature, PlanTarget,
    PlanTerminator, TypeId,
};
use std::collections::{BTreeMap, BTreeSet};

/// Emit a plan into a target-neutral module under the given representation.
pub fn emit_low<R: Representation>(plan: &BackendPlan, rep: &R) -> Result<LeModule, Vec<LeDiag>> {
    Emitter::new(plan, rep).run()
}

/// The shared low-level emitter. See [`emit_low`].
pub struct Emitter<'p, R: Representation> {
    plan: &'p BackendPlan,
    rep: &'p R,
    type_ids: TypeIdTable,
    /// Tail-target member → funcref table slot (slot 0 is the trap stub).
    table_slots: BTreeMap<PlanFuncId, u32>,
    /// Plan function index → emitted wrapper/plain function id.
    func_ids: BTreeMap<u32, LeFuncId>,
    /// Plan function index → step function id (tail-target set only).
    step_ids: BTreeMap<u32, LeFuncId>,
    /// The tail-target set (plan function indices).
    tail_targets: BTreeSet<u32>,
    /// Frame size in slots: 1 (callee) + max(params, results) over the
    /// tail-target set.
    frame_slots: u32,
    helper_base: u32,
}

impl<'p, R: Representation> Emitter<'p, R> {
    pub fn new(plan: &'p BackendPlan, rep: &'p R) -> Self {
        Emitter {
            plan,
            rep,
            type_ids: TypeIdTable::build(plan),
            table_slots: BTreeMap::new(),
            func_ids: BTreeMap::new(),
            step_ids: BTreeMap::new(),
            tail_targets: BTreeSet::new(),
            frame_slots: 1,
            helper_base: 0,
        }
    }

    fn func_sig(&self, func: &PlanFunction) -> Result<(Vec<TypeId>, Vec<TypeId>), LeDiag> {
        match &self.plan.signatures[func.signature.index()] {
            PlanSignature::Func { params, returns, .. } => Ok((params.clone(), returns.clone())),
            _ => Err(LeDiag {
                function: None,
                block: None,
                inst: None,
                message: format!("function {} has a non-function signature", func.name),
            }),
        }
    }

    fn lower_tys(&self, tys: &[TypeId]) -> Result<Vec<LeType>, LeDiag> {
        tys.iter()
            .map(|t| self.rep.lower_type(self.plan, *t))
            .collect()
    }

    /// Compute the tail-target set and assign funcref table slots: every
    /// `ref.func` target plus every direct defined tail-call target.
    fn compute_tail_targets(&mut self) {
        for func in &self.plan.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Some(target) = inst.ref_func {
                        self.tail_targets.insert(target.0);
                    }
                }
                if let PlanTerminator::TailCall { call, .. } = &block.terminator
                    && let PlanCallKind::Direct { callee } = call
                    && matches!(
                        &self.plan.functions[callee.0 as usize].kind,
                        PlanFunctionKind::Defined
                    )
                {
                    self.tail_targets.insert(callee.0);
                }
            }
        }
        for (slot, f) in (1u32..).zip(self.tail_targets.iter().copied().collect::<Vec<_>>()) {
            self.table_slots.insert(PlanFuncId(f), slot);
        }
        let mut width = 0usize;
        for &f in &self.tail_targets {
            if let Ok((params, returns)) = self.func_sig(&self.plan.functions[f as usize]) {
                width = width.max(params.len()).max(returns.len());
            }
        }
        self.frame_slots = 1 + width as u32;
    }

    fn run(mut self) -> Result<LeModule, Vec<LeDiag>> {
        // ---- ids: plan functions (in plan order, imports included), then
        // step functions, then a trap stub, then representation helpers and
        // runtime entry points (allocated lazily from `helper_base`).
        let mut next: u32 = 0;
        let mut imports = Vec::new();
        let mut diags: Vec<LeDiag> = Vec::new();
        for (index, func) in self.plan.functions.iter().enumerate() {
            let id = LeFuncId(next);
            next += 1;
            self.func_ids.insert(index as u32, id);
            if let PlanFunctionKind::Import { module, name } = &func.kind {
                match self.func_sig(func) {
                    Ok((params, returns)) => {
                        match (self.lower_tys(&params), self.lower_tys(&returns)) {
                            (Ok(params), Ok(results)) => imports.push(LeImport {
                                module: module.clone(),
                                name: name.clone(),
                                func: id,
                                params,
                                results,
                            }),
                            (Err(d), _) | (_, Err(d)) => diags.push(d),
                        }
                    }
                    Err(d) => diags.push(d),
                }
            }
        }
        self.compute_tail_targets();
        for &f in self.tail_targets.clone().iter() {
            self.step_ids.insert(f, LeFuncId(next));
            next += 1;
        }
        let stub_id = LeFuncId(next);
        next += 1;
        self.helper_base = next;

        let mut module = LeModule {
            schema: LOW_EMIT_SCHEMA.to_string(),
            lane: self.rep.name().to_string(),
            imports,
            ..LeModule::default()
        };
        for global in &self.plan.globals {
            match self.rep.lower_type(self.plan, global.ty) {
                Ok(ty) => module.globals.push((ty, global.init)),
                Err(mut d) => {
                    d.message = format!("global: {}", d.message);
                    diags.push(d);
                }
            }
        }
        if let Err(d) = self.rep.declare(self.plan, &mut module) {
            diags.push(d);
        }
        module.start = self.plan.start.and_then(|f| self.func_ids.get(&f.0).copied());

        let tables = Tables {
            plan: self.plan,
            rep: self.rep,
            type_ids: &self.type_ids,
            table_slots: &self.table_slots,
            func_ids: &self.func_ids,
            step_ids: &self.step_ids,
            frame_slots: self.frame_slots,
            helper_base: self.helper_base,
            collects: self.rep.collects(),
        };

        // ---- function bodies.
        let mut bodies: BTreeMap<u32, LeFunc> = BTreeMap::new();
        let mut helpers: Vec<LeFunc> = Vec::new();
        let mut helper_names: BTreeMap<String, LeFuncId> = BTreeMap::new();
        for (index, func) in self.plan.functions.iter().enumerate() {
            if matches!(func.kind, PlanFunctionKind::Import { .. }) {
                continue;
            }
            let tail_target = self.tail_targets.contains(&(index as u32));
            emit_one_function(
                tables,
                func,
                PlanFuncId(index as u32),
                tail_target,
                &mut bodies,
                &mut helpers,
                &mut helper_names,
                &mut diags,
            );
        }

        // ---- import placeholders: imports occupy their `LeFuncId` with a
        // body-less entry so `module.funcs` stays dense-as-documented
        // (targets emit extern prototypes for them and never a body).
        for imp in &module.imports {
            let locals: Vec<(LeLocal, LeType)> = imp
                .params
                .iter()
                .enumerate()
                .map(|(i, t)| (LeLocal(i as u32), *t))
                .collect();
            bodies.insert(
                imp.func.0,
                LeFunc {
                    name: format!("import:{}/{}", imp.module, imp.name),
                    params: imp
                        .params
                        .iter()
                        .enumerate()
                        .map(|(i, t)| (LeLocal(i as u32), *t))
                        .collect(),
                    results: imp.results.clone(),
                    locals,
                    body: LeBody::Runtime,
                },
            );
        }

        // ---- representation runtime (helpers + runtime entry points).
        // Runtime fns are id-dense right after the lazily-created helpers.
        let runtime_base = self.helper_base + helpers.len() as u32;
        let mut runtime_fns: Vec<LeFunc> = Vec::new();
        let mut next_runtime = runtime_base;
        {
            let mut sink = RuntimeSink::new(&mut runtime_fns, &mut next_runtime);
            if let Err(d) = self.rep.emit_runtime(self.plan, &mut sink) {
                diags.push(d);
            }
        }

        // ---- tables: 0 = funcref values (slot → wrapper/plain fn), 1 =
        // step dispatch (slot → step fn). Slot 0 and gaps hit the stub.
        if !self.table_slots.is_empty() {
            let max_slot = self.table_slots.values().copied().max().unwrap_or(0);
            let mut value_elems = vec![stub_id; max_slot as usize + 1];
            let mut step_elems = vec![stub_id; max_slot as usize + 1];
            for (&target, &slot) in &self.table_slots {
                if let Some(&wrapper) = self.func_ids.get(&target.0) {
                    value_elems[slot as usize] = wrapper;
                }
                if let Some(&step) = self.step_ids.get(&target.0) {
                    step_elems[slot as usize] = step;
                }
            }
            module.tables.push(LeTable {
                name: "funcref".to_string(),
                elems: value_elems,
            });
            module.tables.push(LeTable {
                name: "step".into(),
                elems: step_elems,
            });
            bodies.insert(
                stub_id.0,
                LeFunc {
                    name: "js_trap_stub".into(),
                    params: vec![],
                    results: vec![],
                    locals: vec![],
                    body: LeBody::Structured(vec![LeStmt::Trap]),
                },
            );
        }

        for (offset, helper) in helpers.into_iter().enumerate() {
            bodies.insert(self.helper_base + offset as u32, helper);
        }
        for (offset, func) in runtime_fns.into_iter().enumerate() {
            bodies.insert(runtime_base + offset as u32, func);
        }
        module.funcs = bodies.into_values().collect();

        // ---- exports.
        for boundary in &self.plan.boundaries {
            if boundary.direction != BoundaryDirection::Export {
                continue;
            }
            if let BoundaryTarget::Func(func) = boundary.target {
                if let Some(&id) = self.func_ids.get(&func.0) {
                    module.exports.push(LeExport {
                        name: boundary.name.clone(),
                        func: id,
                    });
                } else {
                    diags.push(LeDiag {
                        function: None,
                        block: None,
                        inst: None,
                        message: format!("export {:?} names a non-plan function", boundary.name),
                    });
                }
            }
        }

        if diags.is_empty() {
            Ok(module)
        } else {
            Err(diags)
        }
    }
}



// ---------------------------------------------------------------------------
// Per-function emission
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn emit_one_function<R: Representation>(
    t: Tables<'_, R>,
    func: &PlanFunction,
    plan_func: PlanFuncId,
    tail_target: bool,
    bodies: &mut BTreeMap<u32, LeFunc>,
    helpers: &mut Vec<LeFunc>,
    helper_names: &mut BTreeMap<String, LeFuncId>,
    diags: &mut Vec<LeDiag>,
) {
    let (param_tys, result_tys) = match func_sig_of(t.plan, func) {
        Ok((p, r)) => (p, r),
        Err(d) => {
            diags.push(d);
            return;
        }
    };
    let param_tys = match lower_all(t, &param_tys) {
        Ok(p) => p,
        Err(d) => {
            diags.push(d);
            return;
        }
    };
    let result_tys = match lower_all(t, &result_tys) {
        Ok(r) => r,
        Err(d) => {
            diags.push(d);
            return;
        }
    };

    // ---- FnState: locals for plan values, root slots, driver temps.
    let mut local_tys: Vec<LeType> = Vec::new();
    let mut value_locals: Vec<LeLocal> = Vec::new();
    if tail_target {
        local_tys.push(LeType::Ref); // local 0: the step frame
    }
    for value in &func.values {
        let ty = match value.ty {
            Some(ty) => match t.rep.lower_type(t.plan, ty) {
                Ok(ty) => ty,
                Err(d) => {
                    diags.push(d);
                    LeType::I32
                }
            },
            // Values without a meaningful type (side-effect markers) are
            // never referenced as operands.
            None => LeType::I32,
        };
        value_locals.push(LeLocal(local_tys.len() as u32));
        local_tys.push(ty);
    }
    let mut ref_slots: BTreeMap<LeLocal, u32> = BTreeMap::new();
    // Reserve driver temps when a plain-form function contains a non-import
    // tail call (one frame + one cursor; reused — drivers never overlap).
    let mut driver = None;
    if !tail_target {
        let needs_driver = func.blocks.iter().any(|b| match &b.terminator {
            PlanTerminator::TailCall { call, .. } => match call {
                PlanCallKind::Direct { callee } => !matches!(
                    &t.plan.functions[callee.0 as usize].kind,
                    PlanFunctionKind::Import { .. }
                ),
                _ => true,
            },
            _ => false,
        });
        if needs_driver {
            let frame = LeLocal(local_tys.len() as u32);
            local_tys.push(LeType::Ref);
            let cur = LeLocal(local_tys.len() as u32);
            local_tys.push(LeType::Ref);
            driver = Some((frame, cur));
        }
    }
    if t.collects {
        let mut slot = 0u32;
        for (i, ty) in local_tys.iter().enumerate() {
            if ty.is_ref() {
                ref_slots.insert(LeLocal(i as u32), slot);
                slot += 1;
            }
        }
    }
    let state = FnState {
        value_locals,
        ref_slots,
        step_frame: tail_target.then_some(LeLocal(0)),
        driver,
        param_tys: param_tys.clone(),
        result_tys: result_tys.clone(),
    };

    // ---- blocks.
    let mut blocks = Vec::new();
    for (block_index, block) in func.blocks.iter().enumerate() {
        let mut cx = InstCx {
            t,
            f: &state,
            local_tys: &mut local_tys,
            helpers,
            helper_names,
            diags,
            stmts: vec![LeStmt::Label(LeBlockId(block_index as u32))],
            at: (Some(plan_func), Some(block_index as u32), None),
        };
        if block_index == 0 {
            if t.collects && !state.ref_slots.is_empty() {
                cx.emit(LeStmt::RootFrameEnter {
                    slots: state.ref_slots.len() as u32,
                });
                if let Some(frame) = state.step_frame {
                    cx.root_store(frame);
                }
            }
            if let Some(frame) = state.step_frame {
                // Unpack frame slots into the entry params' locals.
                let entry_params = block
                    .params
                    .iter()
                    .map(|v| state.value_locals[v.0 as usize])
                    .collect::<Vec<_>>();
                for (i, local) in entry_params.iter().enumerate() {
                    let ty = cx.ty_of(*local);
                    let word = t.rep.frame_load(&mut cx, &frame, 1 + i as u32);
                    cx.emit(LeStmt::Assign(
                        *local,
                        LeExpr::FromSlot(ty, Box::new(word)),
                    ));
                    cx.root_store(*local);
                }
            } else {
                // Plain form: root the ref params (assigned by the caller).
                for v in &block.params {
                    let local = state.value_locals[v.0 as usize];
                    cx.root_store(local);
                }
            }
        }
        for (inst_index, inst) in block.insts.iter().enumerate() {
            cx.at.2 = Some(inst_index as u32);
            cx.emit_inst(inst);
        }
        cx.at.2 = None;
        cx.emit_terminator(block);
        blocks.push(LeBlock {
            id: LeBlockId(block_index as u32),
            params: block
                .params
                .iter()
                .map(|v| state.value_locals[v.0 as usize])
                .collect(),
            body: cx.stmts,
        });
    }

    // ---- assemble.
    let locals: Vec<(LeLocal, LeType)> = local_tys
        .iter()
        .enumerate()
        .map(|(i, t)| (LeLocal(i as u32), *t))
        .collect();
    if tail_target {
        let step = LeFunc {
            name: format!("{}$step", func.name),
            params: vec![(LeLocal(0), LeType::Ref)],
            results: vec![LeType::Ref],
            locals,
            body: LeBody::Blocks(blocks),
        };
        bodies.insert(t.step_ids[&plan_func.0].0, step);
        let wrapper = build_wrapper(t, func, plan_func, &state, helpers, helper_names, diags);
        bodies.insert(t.func_ids[&plan_func.0].0, wrapper);
    } else {
        let params = func.blocks[0]
            .params
            .iter()
            .map(|v| {
                let local = state.value_locals[v.0 as usize];
                (local, state.param_tys[value_param_index(func, v)])
            })
            .collect();
        bodies.insert(
            t.func_ids[&plan_func.0].0,
            LeFunc {
                name: func.name.clone(),
                params,
                results: result_tys,
                locals,
                body: LeBody::Blocks(blocks),
            },
        );
    }
}

fn func_sig_of(
    plan: &BackendPlan,
    func: &PlanFunction,
) -> Result<(Vec<TypeId>, Vec<TypeId>), LeDiag> {
    match &plan.signatures[func.signature.index()] {
        PlanSignature::Func { params, returns, .. } => Ok((params.clone(), returns.clone())),
        _ => Err(LeDiag {
            function: None,
            block: None,
            inst: None,
            message: format!("function {} has a non-function signature", func.name),
        }),
    }
}

fn lower_all<R: Representation>(t: Tables<'_, R>, tys: &[TypeId]) -> Result<Vec<LeType>, LeDiag> {
    tys.iter().map(|ty| t.rep.lower_type(t.plan, *ty)).collect()
}

/// Index of a plan value among the function's parameters (entry block
/// params), for wrapper/plain param typing.
fn value_param_index(func: &PlanFunction, v: &crate::backend_plan::PlanValueId) -> usize {
    func.blocks[0]
        .params
        .iter()
        .position(|p| p == v)
        .unwrap_or(0)
}

/// Build the trampoline wrapper for a tail-target function (see the
/// `low_emit` module header, "Tail-call trampoline contract").
fn build_wrapper<R: Representation>(
    t: Tables<'_, R>,
    func: &PlanFunction,
    plan_func: PlanFuncId,
    state: &FnState,
    helpers: &mut Vec<LeFunc>,
    helper_names: &mut BTreeMap<String, LeFuncId>,
    diags: &mut Vec<LeDiag>,
) -> LeFunc {
    let mut local_tys: Vec<LeType> = state.param_tys.clone();
    let wrapper_state = FnState {
        value_locals: Vec::new(),
        ref_slots: BTreeMap::new(),
        step_frame: None,
        driver: None,
        param_tys: state.param_tys.clone(),
        result_tys: state.result_tys.clone(),
    };
    let mut cx = InstCx {
        t,
        f: &wrapper_state,
        local_tys: &mut local_tys,
        helpers,
        helper_names,
        diags,
        stmts: Vec::new(),
        at: (Some(plan_func), None, None),
    };
    let frame = cx.temp(LeType::Ref);
    let cur = cx.temp(LeType::Ref);
    if t.collects {
        cx.emit(LeStmt::RootFrameEnter { slots: 1 });
    }
    if let Err(d) = t.rep.frame_alloc(&mut cx, t.frame_slots, frame) {
        cx.diags.push(d);
    }
    if t.collects {
        cx.emit(LeStmt::RootStore {
            slot: 0,
            value: frame,
        });
    }
    for (i, ty) in state.param_tys.iter().enumerate() {
        let word = LeExpr::ToSlot(*ty, Box::new(LeExpr::Local(LeLocal(i as u32))));
        t.rep.frame_store(&mut cx, &frame, 1 + i as u32, word);
    }
    let slot = t.table_slots[&plan_func];
    let fref = t.rep.funcref_value(plan_func, slot);
    cx.emit(LeStmt::Assign(cur, fref));
    let mut loop_body = Vec::new();
    loop_body.push(LeStmt::Call {
        callee: LeCallee::Ptr(Box::new(LeExpr::TableElem {
            table: 1,
            index: Box::new(LeExpr::Local(cur)),
        })),
        args: vec![LeExpr::Local(frame)],
        results: vec![frame],
    });
    let next_word = t.rep.frame_load(&mut cx, &frame, 0);
    loop_body.push(LeStmt::Assign(
        cur,
        LeExpr::FromSlot(LeType::Ref, Box::new(next_word)),
    ));
    loop_body.push(LeStmt::If {
        cond: LeExpr::Binary(
            LeBinOp::I64Eq,
            Box::new(LeExpr::Local(cur)),
            Box::new(LeExpr::ConstI64(0)),
        ),
        then: vec![LeStmt::Break],
        else_: vec![],
    });
    cx.emit(LeStmt::Loop(loop_body));
    let mut results = Vec::new();
    for (i, ty) in state.result_tys.iter().enumerate() {
        let word = t.rep.frame_load(&mut cx, &frame, 1 + i as u32);
        let temp = cx.temp(*ty);
        cx.emit(LeStmt::Assign(
            temp,
            LeExpr::FromSlot(*ty, Box::new(word)),
        ));
        results.push(LeExpr::Local(temp));
    }
    if t.collects {
        cx.emit(LeStmt::RootFramePop);
    }
    cx.emit(LeStmt::Return(results));

    let body_stmts = cx.stmts;
    let locals: Vec<(LeLocal, LeType)> = cx
        .local_tys
        .iter()
        .enumerate()
        .map(|(i, ty)| (LeLocal(i as u32), *ty))
        .collect();
    LeFunc {
        name: func.name.clone(),
        params: (0..state.param_tys.len() as u32)
            .map(|i| (LeLocal(i), state.param_tys[i as usize]))
            .collect(),
        results: state.result_tys.clone(),
        locals,
        body: LeBody::Structured(body_stmts),
    }
}

// ---------------------------------------------------------------------------
// Instruction and terminator lowering (InstCx methods)
// ---------------------------------------------------------------------------

impl<'a, R: Representation> InstCx<'a, R> {
    fn local_of(&self, value: crate::backend_plan::PlanValueId) -> LeLocal {
        self.f.value_locals[value.0 as usize]
    }

    fn expr_of(&self, value: crate::backend_plan::PlanValueId) -> LeExpr {
        LeExpr::Local(self.local_of(value))
    }

    /// Assign `expr` to `out`, publishing root stores for ref-typed locals.
    fn assign(&mut self, out: LeLocal, expr: LeExpr) {
        self.emit(LeStmt::Assign(out, expr));
        self.root_store(out);
    }

    fn root_store(&mut self, local: LeLocal) {
        if let Some(&slot) = self.f.ref_slots.get(&local) {
            self.emit(LeStmt::RootStore { slot, value: local });
        }
    }

    fn emit_inst(&mut self, inst: &PlanInst) {
        let name = inst.operator.as_str();
        // Waffle gives even void-producing operators (e.g. a call to a
        // void function) a placeholder `Value`, so `inst.results` can list
        // one more value than `inst.result_types`; the types are the truth.
        let results: Vec<LeLocal> = if inst.result_types.len() == inst.results.len() {
            inst.results.iter().map(|v| self.local_of(*v)).collect()
        } else if inst.result_types.is_empty() {
            Vec::new()
        } else {
            self.error(format!(
                "operator {name}: {} result values but {} result types",
                inst.results.len(),
                inst.result_types.len()
            ));
            Vec::new()
        };

        // Constants.
        if let Some(konst) = &inst.const_value {
            let expr = match konst {
                PlanConst::I32(v) => LeExpr::ConstI32(*v),
                PlanConst::I64(v) => LeExpr::ConstI64(*v),
                PlanConst::F32(v) => LeExpr::ConstF32(*v),
                PlanConst::F64(v) => LeExpr::ConstF64(*v),
            };
            if let [out] = results[..] {
                self.assign(out, expr);
            } else if !results.is_empty() {
                self.error(format!("constant {name} has {} results", results.len()));
            }
            return;
        }

        let args: Vec<LeExpr> = inst.args.iter().map(|v| self.expr_of(*v)).collect();

        // Shared scalar operators.
        if let Some(op) = scalar_binop(name) {
            if args.len() == 2 && results.len() == 1 {
                let expr = LeExpr::Binary(
                    op,
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                );
                self.assign(results[0], expr);
            } else {
                self.error(format!("{name} has {} args / {} results", args.len(), results.len()));
            }
            return;
        }
        if let Some(op) = scalar_unop(name) {
            if args.len() == 1 && results.len() == 1 {
                let expr = LeExpr::Unary(op, Box::new(args[0].clone()));
                self.assign(results[0], expr);
            } else {
                self.error(format!("{name} has {} args / {} results", args.len(), results.len()));
            }
            return;
        }
        if let Some((op, from, to, min, max)) = trapping_unop(name) {
            let helper = self.trunc_guard(name, op, from, to, min, max);
            self.emit(LeStmt::Call {
                callee: LeCallee::Direct(helper),
                args,
                results: results.clone(),
            });
            for out in &results {
                self.root_store(*out);
            }
            return;
        }

        match name {
            // ---- shared reference scheme (see the module header: 0 = null,
            // odd = i31, else representation storage; word arithmetic uses
            // i64 operators).
            "RefNull" => {
                if let [out] = results[..] {
                    self.assign(out, LeExpr::ConstI64(0));
                }
            }
            "RefIsNull" => {
                if let [out] = results[..] {
                    let expr = LeExpr::Binary(
                        LeBinOp::I64Eq,
                        Box::new(args[0].clone()),
                        Box::new(LeExpr::ConstI64(0)),
                    );
                    self.assign(out, expr);
                }
            }
            "RefEq" => {
                if let [out] = results[..] {
                    let expr = LeExpr::Binary(
                        LeBinOp::I64Eq,
                        Box::new(args[0].clone()),
                        Box::new(args[1].clone()),
                    );
                    self.assign(out, expr);
                }
            }
            "RefI31" => {
                if let [out] = results[..] {
                    // ((v & 0x7fffffff) << 1) | 1, computed in word width.
                    let masked = LeExpr::Binary(
                        LeBinOp::I64And,
                        Box::new(LeExpr::Unary(
                            LeUnOp::I64ExtendI32U,
                            Box::new(args[0].clone()),
                        )),
                        Box::new(LeExpr::ConstI64(0x7fff_ffff)),
                    );
                    let shifted = LeExpr::Binary(
                        LeBinOp::I64Shl,
                        Box::new(masked),
                        Box::new(LeExpr::ConstI64(1)),
                    );
                    let tagged = LeExpr::Binary(
                        LeBinOp::I64Or,
                        Box::new(shifted),
                        Box::new(LeExpr::ConstI64(1)),
                    );
                    self.assign(out, tagged);
                }
            }
            "I31GetS" => {
                if let [out] = results[..] {
                    // Sign-extend from payload bit 30: (w << 32) >>s 33.
                    let shifted = LeExpr::Binary(
                        LeBinOp::I64Shl,
                        Box::new(args[0].clone()),
                        Box::new(LeExpr::ConstI64(32)),
                    );
                    let back = LeExpr::Binary(
                        LeBinOp::I64ShrS,
                        Box::new(shifted),
                        Box::new(LeExpr::ConstI64(33)),
                    );
                    self.assign(out, LeExpr::Unary(LeUnOp::I32WrapI64, Box::new(back)));
                }
            }
            "I31GetU" => {
                if let [out] = results[..] {
                    let shifted = LeExpr::Binary(
                        LeBinOp::I64ShrU,
                        Box::new(args[0].clone()),
                        Box::new(LeExpr::ConstI64(1)),
                    );
                    self.assign(out, LeExpr::Unary(LeUnOp::I32WrapI64, Box::new(shifted)));
                }
            }
            "TypedSelect" => {
                // Plan order is (if_true, if_false, cond); LeExpr::Select is
                // (cond, if_true, if_false).
                if args.len() == 3 && results.len() == 1 {
                    let expr = LeExpr::Select(
                        Box::new(args[2].clone()),
                        Box::new(args[0].clone()),
                        Box::new(args[1].clone()),
                    );
                    self.assign(results[0], expr);
                } else {
                    self.error("TypedSelect without 3 args / 1 result");
                }
            }
            "GlobalGet" => {
                if let (Some(global), [out]) = (inst.payload.global, &results[..]) {
                    self.assign(*out, LeExpr::GlobalGet(global));
                } else {
                    self.error("GlobalGet without global payload");
                }
            }
            "GlobalSet" => {
                if let Some(global) = inst.payload.global {
                    self.emit(LeStmt::GlobalSet(global, args[0].clone()));
                } else {
                    self.error("GlobalSet without global payload");
                }
            }
            "RefFunc" => {
                if let (Some(target), [out]) = (inst.ref_func, &results[..]) {
                    let slot = self.t.table_slots.get(&target).copied().unwrap_or(0);
                    let word = self.t.rep.funcref_value(target, slot);
                    self.assign(*out, word);
                } else {
                    self.error("RefFunc without target payload");
                }
            }
            "Call" | "CallRef" | "CallIndirect" => {
                let Some(call) = &inst.call else {
                    self.error(format!("{name} without call payload"));
                    return;
                };
                self.emit_call(call, args, results);
            }

            // ---- representation surface ------------------------------------
            "StructNew" | "StructNewDefault" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(format!("{name} without signature payload"));
                    return;
                };
                let fields = if name == "StructNew" {
                    Some(&args[..])
                } else {
                    None
                };
                let rep = self.t.rep;
                let out = results.first().copied();
                let r = match out {
                    Some(out) => rep.struct_new(self, sig, fields, out),
                    None => {
                        self.error(format!("{name} without result"));
                        return;
                    }
                };
                if let Err(d) = r {
                    self.diags.push(d);
                }
                if let Some(out) = out {
                    self.root_store(out);
                }
            }
            "StructGet" => {
                let (Some(sig), Some(field)) = (inst.payload.sig, inst.payload.field) else {
                    self.error("StructGet without signature/field payload");
                    return;
                };
                let rep = self.t.rep;
                if let [out] = results[..] {
                    if let Err(d) = rep.struct_get(self, sig, field, args[0].clone(), out) {
                        self.diags.push(d);
                    }
                    self.root_store(out);
                }
            }
            "StructSet" => {
                let (Some(sig), Some(field)) = (inst.payload.sig, inst.payload.field) else {
                    self.error("StructSet without signature/field payload");
                    return;
                };
                let rep = self.t.rep;
                if let Err(d) = rep.struct_set(self, sig, field, args[0].clone(), args[1].clone())
                {
                    self.diags.push(d);
                }
            }
            "ArrayNewDefault" | "ArrayNewFixed" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(format!("{name} without signature payload"));
                    return;
                };
                let rep = self.t.rep;
                if let [out] = results[..] {
                    let r = if name == "ArrayNewFixed" {
                        rep.array_new(
                            self,
                            sig,
                            LeExpr::ConstI32(args.len() as u32),
                            Some(&args),
                            out,
                        )
                    } else {
                        rep.array_new(self, sig, args[0].clone(), None, out)
                    };
                    if let Err(d) = r {
                        self.diags.push(d);
                    }
                    self.root_store(out);
                }
            }
            "ArrayGet" | "ArrayGetS" | "ArrayGetU" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(format!("{name} without signature payload"));
                    return;
                };
                let packed = match name {
                    "ArrayGetS" => Some(true),
                    "ArrayGetU" => Some(false),
                    _ => None,
                };
                let rep = self.t.rep;
                if let [out] = results[..] {
                    if let Err(d) =
                        rep.array_get(self, sig, args[0].clone(), args[1].clone(), packed, out)
                    {
                        self.diags.push(d);
                    }
                    self.root_store(out);
                }
            }
            "ArraySet" => {
                let Some(sig) = inst.payload.sig else {
                    self.error("ArraySet without signature payload");
                    return;
                };
                let rep = self.t.rep;
                if let Err(d) = rep.array_set(
                    self,
                    sig,
                    args[0].clone(),
                    args[1].clone(),
                    args[2].clone(),
                ) {
                    self.diags.push(d);
                }
            }
            "ArrayLen" => {
                let rep = self.t.rep;
                if let [out] = results[..] {
                    if let Err(d) = rep.array_len(self, args[0].clone(), out) {
                        self.diags.push(d);
                    }
                }
            }
            "ArrayCopy" => {
                let rep = self.t.rep;
                if let Err(d) = rep.array_copy(
                    self,
                    args[0].clone(),
                    args[1].clone(),
                    args[2].clone(),
                    args[3].clone(),
                    args[4].clone(),
                ) {
                    self.diags.push(d);
                }
            }
            "RefTest" | "RefCast" => {
                let (Some(heap), Some(nullable)) = (inst.payload.heap.clone(), inst.payload.nullable)
                else {
                    self.error(format!("{name} without heap/nullable payload"));
                    return;
                };
                let rep = self.t.rep;
                if let [out] = results[..] {
                    let r = if name == "RefTest" {
                        rep.ref_test(self, &heap, nullable, args[0].clone(), out)
                    } else {
                        rep.ref_cast(self, &heap, nullable, args[0].clone(), out)
                    };
                    if let Err(d) = r {
                        self.diags.push(d);
                    }
                    self.root_store(out);
                }
            }
            other => {
                self.error(format!(
                    "operator {other} is outside the low-level emission surface"
                ));
            }
        }
    }

    /// Trapping f64→int conversion guard: NaN and out-of-range trap.
    fn trunc_guard(
        &mut self,
        name: &str,
        op: LeUnOp,
        _from: LeType,
        to: LeType,
        min: f64,
        max: f64,
    ) -> LeFuncId {
        self.helper(
            name,
            vec![LeType::F64],
            vec![to],
            move |b, params| {
                let x = LeExpr::Local(params[0]);
                let is_nan = LeExpr::Binary(
                    LeBinOp::F64Ne,
                    Box::new(x.clone()),
                    Box::new(x.clone()),
                );
                b.emit(LeStmt::If {
                    cond: is_nan,
                    then: vec![LeStmt::Trap],
                    else_: vec![],
                });
                let too_low = LeExpr::Binary(
                    LeBinOp::F64Le,
                    Box::new(x.clone()),
                    Box::new(LeExpr::ConstF64(min.to_bits())),
                );
                let too_high = LeExpr::Binary(
                    LeBinOp::F64Ge,
                    Box::new(x.clone()),
                    Box::new(LeExpr::ConstF64(max.to_bits())),
                );
                let out_of_range = LeExpr::Binary(
                    LeBinOp::I32Or,
                    Box::new(too_low),
                    Box::new(too_high),
                );
                b.emit(LeStmt::If {
                    cond: out_of_range,
                    then: vec![LeStmt::Trap],
                    else_: vec![],
                });
                b.emit(LeStmt::Return(vec![LeExpr::Unary(op, Box::new(x))]));
            },
        )
    }

    // -- calls ------------------------------------------------------------------

    fn emit_call(&mut self, call: &PlanCall, args: Vec<LeExpr>, results: Vec<LeLocal>) {
        let (callee, call_args) = match &call.kind {
            PlanCallKind::Direct { callee } => {
                let Some(&id) = self.t.func_ids.get(&callee.0) else {
                    self.error(format!("call to non-plan function {callee}"));
                    return;
                };
                (LeCallee::Direct(id), args)
            }
            PlanCallKind::Ref => {
                // The callee funcref is the last operand (nano_mvp's
                // convention).
                let Some((funcref, rest)) = args.split_last() else {
                    self.error("call_ref without callee operand");
                    return;
                };
                (
                    self.t.rep.callable(funcref.clone(), call.signature),
                    rest.to_vec(),
                )
            }
            PlanCallKind::Indirect { .. } => {
                // call_indirect's last operand is the table index; funcref
                // words are table slots in both lanes, so this is the same
                // callable recovery.
                let Some((index, rest)) = args.split_last() else {
                    self.error("call_indirect without index operand");
                    return;
                };
                (
                    self.t.rep.callable(index.clone(), call.signature),
                    rest.to_vec(),
                )
            }
        };
        self.emit(LeStmt::Call {
            callee,
            args: call_args,
            results: results.clone(),
        });
        for out in &results {
            self.root_store(*out);
        }
    }

    // -- terminators ------------------------------------------------------------

    fn emit_terminator(&mut self, block: &crate::backend_plan::PlanBlock) {
        match &block.terminator {
            PlanTerminator::Br { target } => {
                self.emit_edge(target);
            }
            PlanTerminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                // Per-arm structured edges: moves on each arm write only the
                // successor's params, so the arms cannot interfere.
                let mut then_stmts = Vec::new();
                self.emit_edge_into(&mut then_stmts, if_true);
                let mut else_stmts = Vec::new();
                self.emit_edge_into(&mut else_stmts, if_false);
                self.emit(LeStmt::If {
                    cond: self.expr_of(*cond),
                    then: then_stmts,
                    else_: else_stmts,
                });
            }
            PlanTerminator::Select {
                value,
                targets,
                default,
            } => {
                let v = self.expr_of(*value);
                let mut chain = Vec::new();
                self.emit_edge_into(&mut chain, default);
                for (i, target) in targets.iter().enumerate().rev() {
                    let mut then_stmts = Vec::new();
                    self.emit_edge_into(&mut then_stmts, target);
                    chain = vec![LeStmt::If {
                        cond: LeExpr::Binary(
                            LeBinOp::I32Eq,
                            Box::new(v.clone()),
                            Box::new(LeExpr::ConstI32(i as u32)),
                        ),
                        then: then_stmts,
                        else_: chain,
                    }];
                }
                for stmt in chain {
                    self.emit(stmt);
                }
            }
            PlanTerminator::Return { values } => {
                let exprs: Vec<LeExpr> = values.iter().map(|v| self.expr_of(*v)).collect();
                self.emit_plan_return(exprs);
            }
            PlanTerminator::TailCall { call, args, .. } => {
                self.emit_tail_call(call, args);
            }
            PlanTerminator::Trap => self.emit(LeStmt::Trap),
            PlanTerminator::Ub => self.emit(LeStmt::Unreachable),
        }
    }

    /// A plan-level return, respecting the step form (store results into the
    /// frame and return it) versus the plain form (return values).
    fn emit_plan_return(&mut self, values: Vec<LeExpr>) {
        if let Some(frame) = self.f.step_frame {
            let frame = frame;
            for (i, (value, ty)) in values
                .into_iter()
                .zip(self.f.result_tys.iter().copied())
                .enumerate()
            {
                let word = LeExpr::ToSlot(ty, Box::new(value));
                let rep = self.t.rep;
                rep.frame_store(self, &frame, 1 + i as u32, word);
            }
            let rep = self.t.rep;
            rep.frame_store(self, &frame, 0, LeExpr::ConstI64(0));
            if t_collects(self) {
                self.emit(LeStmt::RootFramePop);
            }
            self.emit(LeStmt::Return(vec![LeExpr::Local(frame)]));
        } else {
            if t_collects(self) {
                self.emit(LeStmt::RootFramePop);
            }
            self.emit(LeStmt::Return(values));
        }
    }

    fn emit_tail_call(&mut self, call: &PlanCallKind, args: &[crate::backend_plan::PlanValueId]) {
        // Split off the callee operand for ref/indirect forms.
        let (callee_word, call_args): (LeExpr, Vec<LeExpr>) = match call {
            PlanCallKind::Direct { callee } => {
                let func = &self.t.plan.functions[callee.0 as usize];
                if matches!(func.kind, PlanFunctionKind::Import { .. }) {
                    // A tail call to an import can never re-enter: plain
                    // call + return.
                    let id = self.t.func_ids[&callee.0];
                    let temps: Vec<LeLocal> = self
                        .f
                        .result_tys
                        .clone()
                        .into_iter()
                        .map(|ty| self.temp(ty))
                        .collect();
                    self.emit(LeStmt::Call {
                        callee: LeCallee::Direct(id),
                        args: args.iter().map(|v| self.expr_of(*v)).collect(),
                        results: temps.clone(),
                    });
                    for t in &temps {
                        self.root_store(*t);
                    }
                    let exprs = temps.into_iter().map(LeExpr::Local).collect();
                    self.emit_plan_return(exprs);
                    return;
                }
                let slot = self.t.table_slots.get(callee).copied().unwrap_or(0);
                (
                    self.t.rep.funcref_value(*callee, slot),
                    args.iter().map(|v| self.expr_of(*v)).collect(),
                )
            }
            PlanCallKind::Ref => {
                let Some((funcref, rest)) = args.split_last() else {
                    self.error("tail call_ref without callee operand");
                    return;
                };
                (
                    self.expr_of(*funcref),
                    rest.iter().map(|v| self.expr_of(*v)).collect(),
                )
            }
            PlanCallKind::Indirect { .. } => {
                let Some((index, rest)) = args.split_last() else {
                    self.error("tail call_indirect without index operand");
                    return;
                };
                (
                    self.expr_of(*index),
                    rest.iter().map(|v| self.expr_of(*v)).collect(),
                )
            }
        };

        if let Some(frame) = self.f.step_frame {
            // Step form: fill the shared frame and hand it back.
            let frame = frame;
            let arg_tys: Vec<LeType> = call_args
                .iter()
                .map(|e| {
                    if let LeExpr::Local(l) = e {
                        self.ty_of(*l)
                    } else {
                        LeType::I64
                    }
                })
                .collect();
            for (i, (value, ty)) in call_args.into_iter().zip(arg_tys).enumerate() {
                let word = LeExpr::ToSlot(ty, Box::new(value));
                let rep = self.t.rep;
                rep.frame_store(self, &frame, 1 + i as u32, word);
            }
            let rep = self.t.rep;
            rep.frame_store(self, &frame, 0, callee_word);
            if t_collects(self) {
                self.emit(LeStmt::RootFramePop);
            }
            self.emit(LeStmt::Return(vec![LeExpr::Local(frame)]));
        } else {
            // Plain form: run a local driver loop (see the module header).
            let Some((frame, cur)) = self.f.driver else {
                self.error("tail call without reserved driver temps");
                return;
            };
            let rep = self.t.rep;
            if let Err(d) = rep.frame_alloc(self, self.t.frame_slots, frame) {
                self.diags.push(d);
            }
            self.root_store(frame);
            let arg_tys: Vec<LeType> = call_args
                .iter()
                .map(|e| {
                    if let LeExpr::Local(l) = e {
                        self.ty_of(*l)
                    } else {
                        LeType::I64
                    }
                })
                .collect();
            for (i, (value, ty)) in call_args.into_iter().zip(arg_tys).enumerate() {
                let word = LeExpr::ToSlot(ty, Box::new(value));
                rep.frame_store(self, &frame, 1 + i as u32, word);
            }
            self.emit(LeStmt::Assign(cur, callee_word));
            self.root_store(cur);
            let mut loop_body = Vec::new();
            loop_body.push(LeStmt::Call {
                callee: LeCallee::Ptr(Box::new(LeExpr::TableElem {
                    table: 1,
                    index: Box::new(LeExpr::Local(cur)),
                })),
                args: vec![LeExpr::Local(frame)],
                results: vec![frame],
            });
            let next_word = rep.frame_load(self, &frame, 0);
            loop_body.push(LeStmt::Assign(
                cur,
                LeExpr::FromSlot(LeType::Ref, Box::new(next_word)),
            ));
            loop_body.push(LeStmt::If {
                cond: LeExpr::Binary(
                    LeBinOp::I64Eq,
                    Box::new(LeExpr::Local(cur)),
                    Box::new(LeExpr::ConstI64(0)),
                ),
                then: vec![LeStmt::Break],
                else_: vec![],
            });
            self.emit(LeStmt::Loop(loop_body));
            let mut results = Vec::new();
            for (i, ty) in self.f.result_tys.clone().into_iter().enumerate() {
                let word = rep.frame_load(self, &frame, 1 + i as u32);
                let temp = self.temp(ty);
                self.emit(LeStmt::Assign(
                    temp,
                    LeExpr::FromSlot(ty, Box::new(word)),
                ));
                self.root_store(temp);
                results.push(LeExpr::Local(temp));
            }
            if t_collects(self) {
                self.emit(LeStmt::RootFramePop);
            }
            self.emit(LeStmt::Return(results));
        }
    }

    /// Emit an edge: parallel-move the args into the target block's params
    /// (temp-mediated to survive cycles), then goto.
    fn emit_edge(&mut self, target: &PlanTarget) {
        let mut stmts = Vec::new();
        self.emit_edge_into(&mut stmts, target);
        for stmt in stmts {
            self.emit(stmt);
        }
    }

    fn emit_edge_into(&mut self, stmts: &mut Vec<LeStmt>, target: &PlanTarget) {
        let params: Vec<LeLocal> = self.t.plan.functions[self.at.0.expect("function").0 as usize]
            .blocks[target.block.0 as usize]
            .params
            .iter()
            .map(|v| self.f.value_locals[v.0 as usize])
            .collect();
        let args: Vec<LeExpr> = target.args.iter().map(|v| self.expr_of(*v)).collect();
        // Parallel move: args -> temps -> params.
        let temps: Vec<LeLocal> = args
            .iter()
            .map(|e| {
                let ty = if let LeExpr::Local(l) = e {
                    self.ty_of(*l)
                } else {
                    LeType::I64
                };
                self.temp(ty)
            })
            .collect();
        for (temp, arg) in temps.iter().zip(args.iter()) {
            stmts.push(LeStmt::Assign(*temp, arg.clone()));
        }
        for (param, temp) in params.iter().zip(temps.iter()) {
            stmts.push(LeStmt::Assign(*param, LeExpr::Local(*temp)));
            if let Some(&slot) = self.f.ref_slots.get(param) {
                stmts.push(LeStmt::RootStore {
                    slot,
                    value: *param,
                });
            }
        }
        stmts.push(LeStmt::Goto(LeBlockId(target.block.0)));
    }
}

fn t_collects<R: Representation>(cx: &InstCx<'_, R>) -> bool {
    cx.t.collects
}
