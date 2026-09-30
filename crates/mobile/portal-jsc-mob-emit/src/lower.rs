//! Lower a waffle `FunctionBody` to the structured SIR.
//!
//! This mirrors `WasmFuncBackend::compile`/`lower_to_sink`/`lower_block`/
//! `lower_value`/`lower_inst` in `waffle-backend` step for step: the body
//! is validated and reduced, then the same `Trees`, Stackify control tree,
//! and `Localifier` local assignment the Wasm opcode encoder consumes are
//! walked to produce target-neutral structured statements. The only
//! difference is the final step: instead of encoding opcodes we build
//! [`SFunc`]. Semantics therefore agree with the Wasm backend by
//! construction.

use anyhow::{Context as _, bail};
use portal_pc_waffle::backend::CFGInfo;
use portal_pc_waffle::backend::backend::localify::Localifier;
use portal_pc_waffle::backend::backend::reducify::Reducifier;
use portal_pc_waffle::backend::backend::stackify::{Context as StackifyContext, WasmBlock};
use portal_pc_waffle::backend::backend::treeify::Trees;
use portal_pc_waffle::{EntityRef, FunctionBody, Type, Value, ValueDef};

use crate::sir::{SExpr, SFunc, SLabel, SStmt};

/// Lower one function body to the structured SIR.
///
/// The body must already satisfy the feature closure (see
/// [`crate::audit`]); violations discovered while walking are hard errors.
pub fn lower_body(body: &FunctionBody) -> anyhow::Result<SFunc> {
    body.validate()
        .context("SIR lowering requires a valid body")?;
    // For ownership reasons (to avoid a self-referential struct with the
    // `Cow::Owned` case when the Reducifier modifies the body), run the
    // Reducifier first — exactly like the Wasm backend.
    let body = Reducifier::new(body).run();
    let body: &FunctionBody = &body;
    let cfg = CFGInfo::new(body);
    let trees = Trees::compute(body);
    let ctrl = StackifyContext::new(body, &cfg)?.compute();
    let locals = Localifier::compute(body, &cfg, &trees);
    let n_params = body.blocks[body.entry].params.len();
    let mut walker = Walker {
        body,
        trees: &trees,
        locals: &locals,
        out_locals: locals.locals.values().copied().collect(),
        next_label: 0,
    };
    let mut stmts = Vec::new();
    let mut labels: Vec<(SLabel, LabelKind)> = Vec::new();
    for block in &ctrl {
        walker.walk_block(block, &mut stmts, &mut labels)?;
    }
    // Mirror the encoder: if the control tree can complete normally, end
    // with a trap — reaching the end of the function is unreachable in a
    // valid module.
    if matches!(
        ctrl.last(),
        Some(WasmBlock::Block { .. } | WasmBlock::Loop { .. } | WasmBlock::If { .. })
    ) {
        stmts.push(SStmt::Unreachable);
    }
    Ok(SFunc {
        locals: walker.out_locals,
        n_params,
        rets: body.rets.clone(),
        body: stmts,
    })
}

#[derive(Clone, Copy)]
enum LabelKind {
    /// Branching to this label jumps to its end (`Break`).
    Block,
    /// Branching to this label jumps to its top (`Continue`).
    Loop,
}

struct Walker<'a, 'b> {
    body: &'a FunctionBody,
    trees: &'b Trees,
    locals: &'b Localifier,
    /// SIR local types: the localify locals followed by temps minted for
    /// block-param transfers.
    out_locals: Vec<Type>,
    next_label: SLabel,
}

impl<'a, 'b> Walker<'a, 'b> {
    fn fresh_label(&mut self) -> SLabel {
        let label = self.next_label;
        self.next_label += 1;
        label
    }

    fn fresh_temp(&mut self, ty: Type) -> u32 {
        self.out_locals.push(ty);
        (self.out_locals.len() - 1) as u32
    }

    fn local_of(&self, value: Value) -> anyhow::Result<u32> {
        let locals = &self.locals.values[value];
        anyhow::ensure!(
            locals.len() == 1,
            "value {value} has {} locals (multi-result values are unsupported)",
            locals.len()
        );
        Ok(locals[0].index() as u32)
    }

    fn walk_block(
        &mut self,
        block: &WasmBlock<'_>,
        into: &mut Vec<SStmt>,
        labels: &mut Vec<(SLabel, LabelKind)>,
    ) -> anyhow::Result<()> {
        match block {
            WasmBlock::Block { body, .. } => {
                let label = self.fresh_label();
                labels.push((label, LabelKind::Block));
                let mut inner = Vec::new();
                for sub in body {
                    self.walk_block(sub, &mut inner, labels)?;
                }
                labels.pop();
                into.push(SStmt::Block { label, body: inner });
            }
            WasmBlock::Loop { body, .. } => {
                let label = self.fresh_label();
                labels.push((label, LabelKind::Loop));
                let mut inner = Vec::new();
                for sub in body {
                    self.walk_block(sub, &mut inner, labels)?;
                }
                labels.pop();
                into.push(SStmt::Loop { label, body: inner });
            }
            WasmBlock::Br { target } => {
                let depth = target.index() as usize;
                anyhow::ensure!(
                    depth < labels.len(),
                    "branch target depth {depth} escapes the label stack"
                );
                let (label, kind) = labels[labels.len() - 1 - depth];
                into.push(match kind {
                    LabelKind::Block => SStmt::Break { label },
                    LabelKind::Loop => SStmt::Continue { label },
                });
            }
            WasmBlock::If {
                cond,
                if_true,
                if_false,
            } => {
                let cond = self.value_expr(*cond)?;
                // An `if` occupies one label depth; branching to it jumps
                // to its end, like a block.
                let label = self.fresh_label();
                labels.push((label, LabelKind::Block));
                let mut t = Vec::new();
                for sub in if_true {
                    self.walk_block(sub, &mut t, labels)?;
                }
                let mut f = Vec::new();
                for sub in if_false {
                    self.walk_block(sub, &mut f, labels)?;
                }
                labels.pop();
                into.push(SStmt::If {
                    label,
                    cond,
                    if_true: t,
                    if_false: f,
                });
            }
            WasmBlock::Select { .. } => {
                bail!("br_table select is outside the mobile feature closure")
            }
            WasmBlock::Leaf { block } => {
                for inst in &self.body.blocks[*block].insts {
                    let value = inst.value;
                    // Owned/rematerialized values are lowered in place at
                    // their single use site, not here.
                    if self.trees.owner.contains_key(&value) || self.trees.remat.contains(&value) {
                        continue;
                    }
                    if let ValueDef::Operator(..) = &self.body.values[value] {
                        self.root_inst(value, into)?;
                    }
                }
            }
            WasmBlock::BlockParams { from, to, prefix } => {
                anyhow::ensure!(
                    *prefix == 0,
                    "block-param prefix transfers are outside the mobile feature closure"
                );
                // Parallel assignment: evaluate all sources into fresh
                // temps before touching any target local (the sources may
                // read locals that are also targets). Mirroring the
                // encoder, transfers whose target has no local are
                // skipped entirely.
                let mut pairs = Vec::with_capacity(from.len());
                for (&from_value, &(to_ty, to_value)) in from.iter().zip(to.iter()) {
                    if self.locals.values[to_value].is_empty() {
                        continue;
                    }
                    let temp = self.fresh_temp(to_ty);
                    let expr = self.value_expr(from_value)?;
                    into.push(SStmt::Assign { local: temp, expr });
                    pairs.push((temp, to_value));
                }
                for (temp, to_value) in pairs {
                    into.push(SStmt::Assign {
                        local: self.local_of(to_value)?,
                        expr: SExpr::LocalGet(temp),
                    });
                }
            }
            WasmBlock::Return { values } => {
                anyhow::ensure!(
                    values.len() <= 1,
                    "multi-value return is outside the mobile feature closure"
                );
                let value = values.first().map(|&v| self.value_expr(v)).transpose()?;
                into.push(SStmt::Return { value });
            }
            WasmBlock::ReturnCall { func, values } => {
                let args = values
                    .iter()
                    .map(|&v| self.value_expr(v))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                into.push(SStmt::TailCall { func: *func, args });
            }
            WasmBlock::ReturnCallRef { sig, values } => {
                let args = values
                    .iter()
                    .map(|&v| self.value_expr(v))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                into.push(SStmt::TailCallRef { sig: *sig, args });
            }
            WasmBlock::ReturnCallIndirect { .. } => {
                bail!("return_call_indirect is outside the mobile feature closure")
            }
            WasmBlock::Unreachable => into.push(SStmt::Unreachable),
        }
        Ok(())
    }

    /// Mirror of the encoder's `lower_value`: resolve aliases, inline
    /// rematerialized values, read everything else from its local.
    fn value_expr(&self, value: Value) -> anyhow::Result<SExpr> {
        let value = self.body.resolve_alias(value);
        if self.trees.remat.contains(&value) {
            return self.inst_expr(value);
        }
        match &self.body.values[value] {
            ValueDef::BlockParam(..) | ValueDef::Operator(..) => {
                Ok(SExpr::LocalGet(self.local_of(value)?))
            }
            ValueDef::PickOutput(orig, idx, _) => {
                let locals = &self.locals.values[*orig];
                anyhow::ensure!(
                    (*idx as usize) < locals.len(),
                    "PickOutput index {idx} out of range for value {orig}"
                );
                Ok(SExpr::LocalGet(locals[*idx as usize].index() as u32))
            }
            other => bail!("unexpected value definition {other:?}"),
        }
    }

    /// Mirror of the encoder's `lower_inst`: emit an operator with its
    /// owned/rematerialized args inlined as subexpressions.
    fn inst_expr(&self, value: Value) -> anyhow::Result<SExpr> {
        let value = self.body.resolve_alias(value);
        match &self.body.values[value] {
            ValueDef::Operator(op, args, tys) => {
                let mut exprs = Vec::with_capacity(args.len());
                for &arg in &self.body.arg_pool[*args] {
                    let arg = self.body.resolve_alias(arg);
                    if self.trees.owner.contains_key(&arg) || self.trees.remat.contains(&arg) {
                        exprs.push(self.inst_expr(arg)?);
                    } else {
                        exprs.push(self.value_expr(arg)?);
                    }
                }
                anyhow::ensure!(
                    tys.len() <= 1,
                    "multi-result operator {op:?} is outside the mobile feature closure"
                );
                let ty = if tys.len() == 1 {
                    Some(self.body.type_pool[*tys][0])
                } else {
                    None
                };
                Ok(SExpr::Op {
                    op: op.clone(),
                    args: exprs,
                    ty,
                })
            }
            ValueDef::PickOutput(..) => self.value_expr(value),
            other => bail!("unexpected value definition {other:?}"),
        }
    }

    /// Mirror of the encoder's `lower_inst(root = true)`: assign the
    /// operator's local, or drop its result.
    fn root_inst(&mut self, value: Value, into: &mut Vec<SStmt>) -> anyhow::Result<()> {
        let expr = self.inst_expr(value)?;
        let locals = &self.locals.values[value];
        match locals.len() {
            0 => into.push(SStmt::Effect { expr }),
            1 => into.push(SStmt::Assign {
                local: locals[0].index() as u32,
                expr,
            }),
            n => bail!("value {value} has {n} locals (multi-result values are unsupported)"),
        }
        Ok(())
    }
}
