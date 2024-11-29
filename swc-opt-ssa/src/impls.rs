use std::{
    collections::BTreeSet,
    convert::Infallible,
    iter::{empty, once},
};

use id_arena::{Arena, Id};
use ssa_traits::{Func, HasChainableValues, HasValues, Val};

use crate::*;

impl cfg_traits::Func for OptFunc {
    type Block = Id<OptBlock>;

    type Blocks = Arena<OptBlock>;

    fn blocks(&self) -> &Self::Blocks {
        &self.cfg.blocks
    }

    fn blocks_mut(&mut self) -> &mut Self::Blocks {
        &mut self.cfg.blocks
    }

    fn entry(&self) -> Self::Block {
        self.entry
    }
}
impl cfg_traits::Block<OptFunc> for OptBlock {
    type Terminator = OptPostcedent;

    fn term(&self) -> &Self::Terminator {
        &self.postcedent
    }

    fn term_mut(&mut self) -> &mut Self::Terminator {
        &mut self.postcedent
    }
}
impl cfg_traits::Term<OptFunc> for OptPostcedent {
    type Target = OptTarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        Box::new(self.term.targets().chain(self.catch.targets()))
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        Box::new(self.term.targets_mut().chain(self.catch.targets_mut()))
    }
}
impl cfg_traits::Term<OptFunc> for OptTerm {
    type Target = OptTarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        match self {
            OptTerm::Throw(id) => Box::new(empty()),
            OptTerm::Return(id) => Box::new(empty()),
            OptTerm::Jmp(starget) => Box::new(once(starget)),
            OptTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new([if_true, if_false].into_iter()),
            OptTerm::Switch { x, blocks, default } => {
                Box::new(blocks.iter().map(|a| &a.1).chain(once(default)))
            }
            OptTerm::Default => Box::new(empty()),
        }
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        match self {
            OptTerm::Throw(id) => Box::new(empty()),
            OptTerm::Return(id) => Box::new(empty()),
            OptTerm::Jmp(starget) => Box::new(once(starget)),
            OptTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new([if_true, if_false].into_iter()),
            OptTerm::Switch { x, blocks, default } => {
                Box::new(blocks.iter_mut().map(|a| &mut a.1).chain(once(default)))
            }
            OptTerm::Default => Box::new(empty()),
        }
    }
}
impl cfg_traits::Term<OptFunc> for OptCatch {
    type Target = OptTarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        match self {
            OptCatch::Throw => Box::new(empty()),
            OptCatch::Just { target } => Box::new(once(target)),
        }
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        match self {
            OptCatch::Throw => Box::new(empty()),
            OptCatch::Just { target } => Box::new(once(target)),
        }
    }
}
impl cfg_traits::Term<OptFunc> for OptTarget {
    type Target = OptTarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        Box::new(once(self))
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        OptFunc: 'a,
    {
        Box::new(once(self))
    }
}
impl cfg_traits::Target<OptFunc> for OptTarget {
    fn block(&self) -> <OptFunc as cfg_traits::Func>::Block {
        self.block
    }

    fn block_mut(&mut self) -> &mut <OptFunc as cfg_traits::Func>::Block {
        &mut self.block
    }
}
impl ssa_traits::Func for OptFunc {
    type Value = Id<OptValueW>;

    type Values = Arena<OptValueW>;

    fn values(&self) -> &Self::Values {
        &self.cfg.values
    }

    fn values_mut(&mut self) -> &mut Self::Values {
        &mut self.cfg.values
    }
}
impl ssa_traits::Value<OptFunc> for OptValueW {}
impl ssa_traits::HasChainableValues<OptFunc> for OptValueW {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        match &self.0 {
            OptValue::Emit { val, ty } => match val {
                SValue::Param { block, idx, ty } => Box::new(empty()),
                SValue::Item(item) => match item {
                    swc_tac::Item::Just { id } => Box::new(once(*id)),
                    swc_tac::Item::Bin { left, right, op } => Box::new([*left, *right].into_iter()),
                    swc_tac::Item::Un { arg, op } => Box::new(once(*arg)),
                    swc_tac::Item::Mem { obj, mem } => Box::new([*obj, *mem].into_iter()),
                    swc_tac::Item::Func { func } => Box::new(empty()),
                    swc_tac::Item::Lit { lit } => Box::new(empty()),
                    swc_tac::Item::Call { r#fn, member, args } => Box::new(
                        once(*r#fn)
                            .chain(member.iter().cloned())
                            .chain(args.iter().cloned()),
                    ),
                    swc_tac::Item::Obj { members } => Box::new(members.iter().flat_map(|m| {
                        let v = once(m.1);
                        let w: Box<dyn Iterator<Item = &Id<OptValueW>> + '_> = match &m.0 {
                            swc_tac::PropKey::Lit(_) => Box::new(empty()),
                            swc_tac::PropKey::Computed(c) => Box::new(once(c)),
                        };
                        v.chain(w.cloned())
                    })),
                    swc_tac::Item::Arr { members } => Box::new(members.iter().cloned()),
                    swc_tac::Item::Yield { value, delegate } => Box::new(value.iter().cloned()),
                    swc_tac::Item::Await { value } => Box::new(once(*value)),
                    swc_tac::Item::Undef => Box::new(empty()),
                },
                SValue::Assign { target, val } => {
                    let v = once(*val);
                    let w: Box<dyn Iterator<Item = &Id<OptValueW>> + '_> = match target {
                        swc_tac::LId::Id { id } => todo!(),
                        swc_tac::LId::Member { obj, mem } => Box::new([obj, mem].into_iter()),
                    };
                    Box::new(v.chain(w.cloned()))
                }
                SValue::LoadId(_) => Box::new(empty()),
                SValue::StoreId { target, val } => Box::new(once(*val)),
            },
            OptValue::Deopt(a) => Box::new(once(*a)),
            OptValue::Assert { val, ty } => Box::new(once(*val)),
        }
    }

    fn values_chain_mut<'a>(
        &'a mut self,
        // g: &'a mut OptFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        match &mut self.0 {
            OptValue::Emit { val, ty } => match val {
                SValue::Param { block, idx, ty } => Box::new(empty()),
                SValue::Item(item) => match item {
                    swc_tac::Item::Just { id } => Box::new(once(id)),
                    swc_tac::Item::Bin { left, right, op } => Box::new([left, right].into_iter()),
                    swc_tac::Item::Un { arg, op } => Box::new(once(arg)),
                    swc_tac::Item::Mem { obj, mem } => Box::new([obj, mem].into_iter()),
                    swc_tac::Item::Func { func } => Box::new(empty()),
                    swc_tac::Item::Lit { lit } => Box::new(empty()),
                    swc_tac::Item::Call { r#fn, member, args } => {
                        Box::new(once(r#fn).chain(member.iter_mut()).chain(args.iter_mut()))
                    }
                    swc_tac::Item::Obj { members } => Box::new(members.iter_mut().flat_map(|m| {
                        let v = once(&mut m.1);
                        let w: Box<dyn Iterator<Item = &mut Id<OptValueW>> + '_> = match &mut m.0 {
                            swc_tac::PropKey::Lit(_) => Box::new(empty()),
                            swc_tac::PropKey::Computed(c) => Box::new(once(c)),
                        };
                        v.chain(w)
                    })),
                    swc_tac::Item::Arr { members } => Box::new(members.iter_mut()),
                    swc_tac::Item::Yield { value, delegate } => Box::new(value.iter_mut()),
                    swc_tac::Item::Await { value } => Box::new(once(value)),
                    swc_tac::Item::Undef => Box::new(empty()),
                },
                SValue::Assign { target, val } => {
                    let v = once(val);
                    let w: Box<dyn Iterator<Item = &mut Id<OptValueW>> + '_> = match target {
                        swc_tac::LId::Id { id } => todo!(),
                        swc_tac::LId::Member { obj, mem } => Box::new([obj, mem].into_iter()),
                    };
                    Box::new(v.chain(w))
                }
                SValue::LoadId(_) => Box::new(empty()),
                SValue::StoreId { target, val } => Box::new(once(val)),
            },
            OptValue::Deopt(a) => Box::new(once(a)),
            OptValue::Assert { val, ty } => Box::new(once(val)),
        }
    }
}
impl HasValues<OptFunc> for OptValueW {
    fn values<'a>(
        &'a self,
        f: &'a OptFunc,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut OptFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl ssa_traits::Block<OptFunc> for OptBlock {
    fn insts(&self) -> impl Iterator<Item = <OptFunc as ssa_traits::Func>::Value> {
        self.insts.iter().cloned()
    }

    fn add_inst(
        func: &mut OptFunc,
        key: <OptFunc as cfg_traits::Func>::Block,
        v: <OptFunc as ssa_traits::Func>::Value,
    ) {
        func.cfg.blocks[key].insts.push(v);
    }
}
impl ssa_traits::Target<OptFunc> for OptTarget {
    fn push_value(&mut self, v: <OptFunc as ssa_traits::Func>::Value) {
        self.args.push(v);
    }

    fn from_values_and_block(
        a: impl Iterator<Item = <OptFunc as ssa_traits::Func>::Value>,
        k: <OptFunc as cfg_traits::Func>::Block,
    ) -> Self {
        OptTarget {
            block: k,
            args: a.collect(),
        }
    }
}
impl HasChainableValues<OptFunc> for OptTarget {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        Box::new(self.args.iter().cloned())
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        Box::new(self.args.iter_mut())
    }
}
impl HasValues<OptFunc> for OptTarget {
    fn values<'a>(
        &'a self,
        f: &'a OptFunc,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut OptFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl HasChainableValues<OptFunc> for OptTerm {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        match self {
            OptTerm::Throw(id) => Box::new(once(*id)),
            OptTerm::Return(id) => Box::new(id.iter().cloned()),
            OptTerm::Jmp(starget) => starget.values_chain(),
            OptTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new(
                once(*cond)
                    .chain(if_true.values_chain())
                    .chain(if_false.values_chain()),
            ),
            OptTerm::Switch { x, blocks, default } => Box::new(
                once(*x).chain(default.values_chain()).chain(
                    blocks
                        .iter()
                        .flat_map(|(a, b)| once(*a).chain(b.values_chain())),
                ),
            ),
            OptTerm::Default => Box::new(empty()),
        }
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        match self {
            OptTerm::Throw(id) => Box::new(once(id)),
            OptTerm::Return(id) => Box::new(id.iter_mut()),
            OptTerm::Jmp(starget) => starget.values_chain_mut(),
            OptTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new(
                once(cond)
                    .chain(if_true.values_chain_mut())
                    .chain(if_false.values_chain_mut()),
            ),
            OptTerm::Switch { x, blocks, default } => Box::new(
                once(x).chain(default.values_chain_mut()).chain(
                    blocks
                        .iter_mut()
                        .flat_map(|(a, b)| once(a).chain(b.values_chain_mut())),
                ),
            ),
            OptTerm::Default => Box::new(empty()),
        }
    }
}
impl HasChainableValues<OptFunc> for OptCatch {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        match self {
            OptCatch::Throw => Box::new(empty()),
            OptCatch::Just { target } => target.values_chain(),
        }
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        match self {
            OptCatch::Throw => Box::new(empty()),
            OptCatch::Just { target } => target.values_chain_mut(),
        }
    }
}
impl HasChainableValues<OptFunc> for OptPostcedent {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        Box::new(self.term.values_chain().chain(self.catch.values_chain()))
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        Box::new(
            self.term
                .values_chain_mut()
                .chain(self.catch.values_chain_mut()),
        )
    }
}
impl HasValues<OptFunc> for OptTerm {
    fn values<'a>(
        &'a self,
        f: &'a OptFunc,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut OptFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl HasValues<OptFunc> for OptCatch {
    fn values<'a>(
        &'a self,
        f: &'a OptFunc,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut OptFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl HasValues<OptFunc> for OptPostcedent {
    fn values<'a>(
        &'a self,
        f: &'a OptFunc,
    ) -> Box<dyn Iterator<Item = <OptFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut OptFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <OptFunc as ssa_traits::Func>::Value> + 'a>
    where
        OptFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl ssa_traits::TypedFunc for OptFunc {
    type Ty = Option<OptType>;

    fn add_blockparam(&mut self, k: Self::Block, y: Self::Ty) -> Self::Value {
        self.cfg.add_blockparam(k, y)
    }
}
impl ssa_traits::TypedBlock<OptFunc> for OptBlock {
    fn params(
        &self,
    ) -> impl Iterator<
        Item = (
            <OptFunc as ssa_traits::TypedFunc>::Ty,
            <OptFunc as ssa_traits::Func>::Value,
        ),
    > {
        return self.params.iter().map(|(a, b)| (*b, *a));
    }
}
impl ssa_traits::TypedValue<OptFunc> for OptValueW {
    fn ty(&self, f: &OptFunc) -> <OptFunc as ssa_traits::TypedFunc>::Ty {
        match &self.0 {
            OptValue::Deopt(d) => {
                let x = f.values()[*d].ty(f);
                x.and_then(|y| y.parent())
            }
            OptValue::Assert { val, ty } => ty.clone(),
            OptValue::Emit { val, ty } => ty.clone(),
        }
    }
}