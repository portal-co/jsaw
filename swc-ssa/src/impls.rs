use std::{
    collections::BTreeSet,
    convert::Infallible,
    iter::{empty, once},
};

use id_arena::{Arena, Id};
use ssa_traits::{HasChainableValues, HasValues};

use crate::{SBlock, SCatch, SFunc, SPostcedent, STarget, STerm, SValue};

impl cfg_traits::Func for SFunc {
    type Block = Id<SBlock>;

    type Blocks = Arena<SBlock>;

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
impl cfg_traits::Block<SFunc> for SBlock {
    type Terminator = SPostcedent;

    fn term(&self) -> &Self::Terminator {
        &self.postcedent
    }

    fn term_mut(&mut self) -> &mut Self::Terminator {
        &mut self.postcedent
    }
}
impl cfg_traits::Term<SFunc> for SPostcedent {
    type Target = STarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        Box::new(self.term.targets().chain(self.catch.targets()))
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        Box::new(self.term.targets_mut().chain(self.catch.targets_mut()))
    }
}
impl cfg_traits::Term<SFunc> for STerm {
    type Target = STarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            STerm::Throw(id) => Box::new(empty()),
            STerm::Return(id) => Box::new(empty()),
            STerm::Jmp(starget) => Box::new(once(starget)),
            STerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new([if_true, if_false].into_iter()),
            STerm::Switch { x, blocks, default } => {
                Box::new(blocks.iter().map(|a| &a.1).chain(once(default)))
            }
            STerm::Default => Box::new(empty()),
        }
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            STerm::Throw(id) => Box::new(empty()),
            STerm::Return(id) => Box::new(empty()),
            STerm::Jmp(starget) => Box::new(once(starget)),
            STerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new([if_true, if_false].into_iter()),
            STerm::Switch { x, blocks, default } => {
                Box::new(blocks.iter_mut().map(|a| &mut a.1).chain(once(default)))
            }
            STerm::Default => Box::new(empty()),
        }
    }
}
impl cfg_traits::Term<SFunc> for SCatch {
    type Target = STarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            SCatch::Throw => Box::new(empty()),
            SCatch::Just { target } => Box::new(once(target)),
        }
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            SCatch::Throw => Box::new(empty()),
            SCatch::Just { target } => Box::new(once(target)),
        }
    }
}
impl cfg_traits::Term<SFunc> for STarget {
    type Target = STarget;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        Box::new(once(self))
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        SFunc: 'a,
    {
        Box::new(once(self))
    }
}
impl cfg_traits::Target<SFunc> for STarget {
    fn block(&self) -> <SFunc as cfg_traits::Func>::Block {
        self.block
    }

    fn block_mut(&mut self) -> &mut <SFunc as cfg_traits::Func>::Block {
        &mut self.block
    }
}
impl ssa_traits::Func for SFunc {
    type Value = Id<SValue>;

    type Values = Arena<SValue>;

    fn values(&self) -> &Self::Values {
        &self.cfg.values
    }

    fn values_mut(&mut self) -> &mut Self::Values {
        &mut self.cfg.values
    }
}
impl ssa_traits::Value<SFunc> for SValue {}
impl ssa_traits::HasChainableValues<SFunc> for SValue {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        match self {
            SValue::Param { block, idx, ty } => Box::new(empty()),
            SValue::Item(item) => match item {
                swc_tac::Item::Just { id } => Box::new(once(*id)),
                swc_tac::Item::Bin { left, right, op } => Box::new([*left, *right].into_iter()),
                swc_tac::Item::Un { arg, op } => Box::new(once(*arg)),
                swc_tac::Item::Mem { obj, mem } => Box::new([*obj, *mem].into_iter()),
                swc_tac::Item::Func { func } => Box::new(empty()),
                swc_tac::Item::Lit { lit } => Box::new(empty()),
                swc_tac::Item::Call { r#fn, member, args } => {
                    Box::new(once(*r#fn).chain(member.iter().cloned()).chain(args.iter().cloned()))
                }
                swc_tac::Item::Obj { members } => Box::new(members.iter().flat_map(|m| {
                    let v = once(m.1);
                    let w: Box<dyn Iterator<Item = &Id<SValue>> + '_> = match &m.0 {
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
                let w: Box<dyn Iterator<Item = &Id<SValue>> + '_> = match target {
                    swc_tac::LId::Id { id } => todo!(),
                    swc_tac::LId::Member { obj, mem } => Box::new([obj, mem].into_iter()),
                };
                Box::new(v.chain(w.cloned()))
            }
            SValue::LoadId(_) => Box::new(empty()),
            SValue::StoreId { target, val } => Box::new(once(*val)),
        }
    }

    fn values_chain_mut<'a>(
        &'a mut self,
        // g: &'a mut SFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            SValue::Param { block, idx, ty } => Box::new(empty()),
            SValue::Item(item) => match item {
                swc_tac::Item::Just { id } => Box::new(once(id)),
                swc_tac::Item::Bin { left, right, op } => Box::new([left, right].into_iter()),
                swc_tac::Item::Un { arg, op } => Box::new(once(arg)),
                swc_tac::Item::Mem { obj, mem } => Box::new([obj, mem].into_iter()),
                swc_tac::Item::Func { func } => Box::new(empty()),
                swc_tac::Item::Lit { lit } => Box::new(empty()),
                swc_tac::Item::Call { r#fn, member, args } => Box::new(once(r#fn).chain(member.iter_mut()).chain(args.iter_mut())),
                swc_tac::Item::Obj { members } => Box::new(members.iter_mut().flat_map(|m| {
                    let v = once(&mut m.1);
                    let w: Box<dyn Iterator<Item = &mut Id<SValue>> + '_> = match &mut m.0 {
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
                let w: Box<dyn Iterator<Item = &mut Id<SValue>> + '_> = match target {
                    swc_tac::LId::Id { id } => todo!(),
                    swc_tac::LId::Member { obj, mem } => Box::new([obj, mem].into_iter()),
                };
                Box::new(v.chain(w))
            }
            SValue::LoadId(_) => Box::new(empty()),
            SValue::StoreId { target, val } => Box::new(once(val)),
        }
    }
}
impl HasValues<SFunc> for SValue {
    fn values<'a>(
        &'a self,
        f: &'a SFunc,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut SFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl ssa_traits::Block<SFunc> for SBlock {
    fn insts(&self) -> impl Iterator<Item = <SFunc as ssa_traits::Func>::Value> {
        self.stmts.iter().cloned()
    }

    fn add_inst(
        func: &mut SFunc,
        key: <SFunc as cfg_traits::Func>::Block,
        v: <SFunc as ssa_traits::Func>::Value,
    ) {
        func.cfg.blocks[key].stmts.push(v);
    }
}
impl ssa_traits::Target<SFunc> for STarget {
    fn push_value(&mut self, v: <SFunc as ssa_traits::Func>::Value) {
        self.args.push(v);
    }

    fn from_values_and_block(
        a: impl Iterator<Item = <SFunc as ssa_traits::Func>::Value>,
        k: <SFunc as cfg_traits::Func>::Block,
    ) -> Self {
        STarget {
            block: k,
            args: a.collect(),
        }
    }
}
impl HasChainableValues<SFunc> for STarget {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        Box::new(self.args.iter().cloned())
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        Box::new(self.args.iter_mut())
    }
}
impl HasValues<SFunc> for STarget {
    fn values<'a>(
        &'a self,
        f: &'a SFunc,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut SFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl HasChainableValues<SFunc> for STerm {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        match self {
            STerm::Throw(id) => Box::new(once(*id)),
            STerm::Return(id) => Box::new(id.iter().cloned()),
            STerm::Jmp(starget) => starget.values_chain(),
            STerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new(
                once(*cond)
                    .chain(if_true.values_chain())
                    .chain(if_false.values_chain()),
            ),
            STerm::Switch { x, blocks, default } => Box::new(
                once(*x).chain(default.values_chain()).chain(
                    blocks
                        .iter()
                        .flat_map(|(a, b)| once(*a).chain(b.values_chain())),
                ),
            ),
            STerm::Default => Box::new(empty()),
        }
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            STerm::Throw(id) => Box::new(once(id)),
            STerm::Return(id) => Box::new(id.iter_mut()),
            STerm::Jmp(starget) => starget.values_chain_mut(),
            STerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new(
                once(cond)
                    .chain(if_true.values_chain_mut())
                    .chain(if_false.values_chain_mut()),
            ),
            STerm::Switch { x, blocks, default } => Box::new(
                once(x).chain(default.values_chain_mut()).chain(
                    blocks
                        .iter_mut()
                        .flat_map(|(a, b)| once(a).chain(b.values_chain_mut())),
                ),
            ),
            STerm::Default => Box::new(empty()),
        }
    }
}
impl HasChainableValues<SFunc> for SCatch {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        match self {
            SCatch::Throw => Box::new(empty()),
            SCatch::Just { target } => target.values_chain(),
        }
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        match self {
            SCatch::Throw => Box::new(empty()),
            SCatch::Just { target } => target.values_chain_mut(),
        }
    }
}
impl HasChainableValues<SFunc> for SPostcedent {
    fn values_chain<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        Box::new(self.term.values_chain().chain(self.catch.values_chain()))
    }

    fn values_chain_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        Box::new(
            self.term
                .values_chain_mut()
                .chain(self.catch.values_chain_mut()),
        )
    }
}
impl HasValues<SFunc> for STerm {
    fn values<'a>(
        &'a self,
        f: &'a SFunc,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut SFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl HasValues<SFunc> for SCatch {
    fn values<'a>(
        &'a self,
        f: &'a SFunc,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut SFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        self.values_chain_mut()
    }
}
impl HasValues<SFunc> for SPostcedent {
    fn values<'a>(
        &'a self,
        f: &'a SFunc,
    ) -> Box<dyn Iterator<Item = <SFunc as ssa_traits::Func>::Value> + 'a> {
        self.values_chain()
    }

    fn values_mut<'a>(
        &'a mut self,
        g: &'a mut SFunc,
    ) -> Box<dyn Iterator<Item = &'a mut <SFunc as ssa_traits::Func>::Value> + 'a>
    where
        SFunc: 'a,
    {
        self.values_chain_mut()
    }
}
