use std::iter::{empty, once};

use cfg_traits::{Block, Func, Target, Term};

use super::*;
impl<D: TacDialect> Func for TSimplFunc<D> {
    type Block = Id<TSimplBlock<D>>;

    type Blocks = Arena<TSimplBlock<D>>;

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
impl<D: TacDialect> Block<TSimplFunc<D>> for TSimplBlock<D> {
    type Terminator = TSimplTerm<D>;

    fn term(&self) -> &Self::Terminator {
        &self.term
    }

    fn term_mut(&mut self) -> &mut Self::Terminator {
        &mut self.term
    }
}
impl<D: TacDialect> Term<TSimplFunc<D>> for TSimplTerm<D> {
    type Target = Id<TSimplBlock<D>>;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        TSimplFunc<D>: 'a,
    {
        match self {
            TSimplTerm::Return(simpl_path_id) => Box::new(empty()),
            TSimplTerm::Jmp(id) => Box::new(once(id)),
            TSimplTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new([if_true, if_false].into_iter()),
            TSimplTerm::Select { scrutinee, cases } => Box::new(cases.values().map(|a| &a.0)),
            TSimplTerm::Default => Box::new(empty()),
            TSimplTerm::Switch { scrutinee, cases } => Box::new(cases.iter().map(|a| &a.1)),
        }
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        TSimplFunc<D>: 'a,
    {
        match self {
            TSimplTerm::Return(simpl_path_id) => Box::new(empty()),
            TSimplTerm::Jmp(id) => Box::new(once(id)),
            TSimplTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => Box::new([if_true, if_false].into_iter()),
            TSimplTerm::Select { scrutinee, cases } => {
                Box::new(cases.values_mut().map(|a| &mut a.0))
            }
            TSimplTerm::Default => Box::new(empty()),
            TSimplTerm::Switch { scrutinee, cases } => Box::new(cases.iter_mut().map(|a| &mut a.1)),
        }
    }
}
impl<D: TacDialect> Target<TSimplFunc<D>> for Id<TSimplBlock<D>> {
    fn block(&self) -> <TSimplFunc<D> as Func>::Block {
        *self
    }

    fn block_mut(&mut self) -> &mut <TSimplFunc<D> as Func>::Block {
        self
    }
}
impl<D: TacDialect> Term<TSimplFunc<D>> for Id<TSimplBlock<D>> {
    type Target = Id<TSimplBlock<D>>;

    fn targets<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Target> + 'a>
    where
        TSimplFunc<D>: 'a,
    {
        Box::new(once(self))
    }

    fn targets_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Self::Target> + 'a>
    where
        TSimplFunc<D>: 'a,
    {
        Box::new(once(self))
    }
}
