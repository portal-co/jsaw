use std::collections::BTreeSet;

use id_arena::{Arena, Id};
use swc_ecma_ast::Lit;
use swc_ssa::{SCatch, SPostcedent, STarget, STerm, SValue, ch::ConstVal, simplify::SValGetter};
use swc_tac::Item;
pub mod impls;
pub mod into;
pub use portal_jsc_swc_util::r#type::{ObjType, OptType};
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum OptValue<I = Id<OptValueW>, B = Id<OptBlock>, F = OptFunc, D = ()> {
    Deopt {
        value: I,
        deoptimizer: D,
    },
    Assert {
        val: I,
        ty: Option<OptType>,
    },
    Emit {
        val: SValue<I, B, F>,
        ty: Option<OptType>,
    },
}
impl<I, B, F, D> OptValue<I, B, F, D> {
    pub fn as_ref<'a>(&'a self) -> OptValue<&'a I, &'a B, &'a F, &'a D> {
        match self {
            OptValue::Deopt { value, deoptimizer } => OptValue::Deopt { value, deoptimizer },
            OptValue::Assert { val, ty } => OptValue::Assert {
                val,
                ty: ty.clone(),
            },
            OptValue::Emit { val, ty } => OptValue::Emit {
                val: val.as_ref(),
                ty: ty.clone(),
            },
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> OptValue<&'a mut I, &'a mut B, &'a mut F, &'a mut D> {
        match self {
            OptValue::Deopt { value, deoptimizer } => OptValue::Deopt { value, deoptimizer },
            OptValue::Assert { val, ty } => OptValue::Assert {
                val,
                ty: ty.clone(),
            },
            OptValue::Emit { val, ty } => OptValue::Emit {
                val: val.as_mut(),
                ty: ty.clone(),
            },
        }
    }
    pub fn map<Ctx, E, I2: Ord, B2, F2, D2>(
        self,
        ctx: &mut Ctx,
        i: &mut (dyn FnMut(&mut Ctx, I) -> Result<I2, E> + '_),
        b: &mut (dyn FnMut(&mut Ctx, B) -> Result<B2, E> + '_),
        f: &mut (dyn FnMut(&mut Ctx, F) -> Result<F2, E> + '_),
        d: &mut (dyn FnMut(&mut Ctx, D) -> Result<D2, E> + '_),
    ) -> Result<OptValue<I2, B2, F2, D2>, E> {
        Ok(match self {
            OptValue::Deopt { value, deoptimizer } => OptValue::Deopt {
                value: i(ctx, value)?,
                deoptimizer: d(ctx, deoptimizer)?,
            },
            OptValue::Assert { val, ty } => OptValue::Assert {
                val: i(ctx, val)?,
                ty,
            },
            OptValue::Emit { val, ty } => OptValue::Emit {
                val: val.map(ctx, i, b, f)?,
                ty,
            },
        })
    }
}
#[derive(Clone, Debug)]
pub struct OptValueW { pub value: OptValue }
#[derive(Default, Clone, Debug)]
pub struct OptBlock {
    pub params: Vec<(Id<OptValueW>, Option<OptType>)>,
    pub insts: Vec<Id<OptValueW>>,
    pub postcedent: OptPostcedent,
}
pub type OptPostcedent = SPostcedent<Id<OptValueW>, Id<OptBlock>>;
pub type OptTarget = STarget<Id<OptValueW>, Id<OptBlock>>;
pub type OptTerm = STerm<Id<OptValueW>, Id<OptBlock>>;
pub type OptCatch = SCatch<Id<OptValueW>, Id<OptBlock>>;
#[derive(Default, Clone, Debug)]
pub struct OptCfg {
    pub values: Arena<OptValueW>,
    pub blocks: Arena<OptBlock>,
    pub decls: BTreeSet<swc_ecma_ast::Id>,
}
impl OptValueW {
    pub fn ty(&self, cfg: &OptCfg) -> Option<OptType> {
        match &self.value {
            OptValue::Deopt { value: d, .. } => {
                let x = cfg.values[*d].ty(cfg);
                x.and_then(|y| y.parent(Default::default()))
            }
            OptValue::Assert { val, ty } => ty.clone(),
            OptValue::Emit { val, ty } => ty.clone(),
        }
    }
    pub fn constant(&self, cfg: &OptCfg) -> Option<Lit> {
        match &self.value {
            OptValue::Deopt { value: a, .. } => cfg.values[*a].constant(cfg),
            OptValue::Assert { val, ty } => cfg.values[*val].constant(cfg),
            OptValue::Emit { val, ty } => match val {
                SValue::Item { item: i, span } => match i {
                    Item::Lit { lit } => Some(lit.clone()),
                    _ => None,
                },
                _ => None,
            },
        }
    }
}
impl SValGetter<Id<OptValueW>, Id<OptBlock>, OptFunc> for OptCfg {
    fn val(&self, id: Id<OptValueW>) -> Option<&SValue<Id<OptValueW>, Id<OptBlock>, OptFunc>> {
        match &self.values[id].value {
            OptValue::Deopt { value: a, .. } => self.val(*a),
            OptValue::Assert { val, ty } => self.val(*val),
            OptValue::Emit { val, ty } => Some(val),
        }
    }
}
#[derive(Clone, Debug)]
pub struct OptFunc {
    pub cfg: OptCfg,
    pub entry: Id<OptBlock>,
    pub is_generator: bool,
    pub is_async: bool,
}
impl OptCfg {
    pub fn add_blockparam(&mut self, k: Id<OptBlock>, ty: Option<OptType>) -> Id<OptValueW> {
        let v = self.values.alloc(OptValueW { value: OptValue::Emit {
            val: SValue::Param {
                block: k,
                idx: self.blocks[k].params.len(),
                ty: (),
            },
            ty: ty.clone(),
        } });
        self.blocks[k].params.push((v, ty));
        return v;
    }
}
