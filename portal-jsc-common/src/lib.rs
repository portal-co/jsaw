#![no_std]

use core::iter::once;

use either::Either;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum LId<I, M: IntoIterator<Item = I> = [I; 1]> {
    Id { id: I },
    Member { obj: I, mem: M },
}
impl<I> LId<I> {
    pub fn map<J, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<LId<J>, E> {
        Ok(match self {
            LId::Id { id } => LId::Id { id: f(id)? },
            LId::Member { obj, mem: [mem] } => LId::Member {
                obj: f(obj)?,
                mem: [f(mem)?],
            },
        })
    }
}
impl<I, M: IntoIterator<Item = I>> LId<I, M> {
    pub fn as_ref<'a>(&'a self) -> LId<&'a I,&'a M> where &'a M: IntoIterator<Item = &'a I>{
        match self{
            LId::Id { id } => LId::Id { id },
            LId::Member { obj, mem } => LId::Member { obj, mem },
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> LId<&'a mut I,&'a mut M> where &'a mut M: IntoIterator<Item = &'a mut I>{
        match self{
            LId::Id { id } => LId::Id { id },
            LId::Member { obj, mem } => LId::Member { obj, mem },
        }
    }
    pub fn refs(self) -> impl Iterator<Item = I> {
        match self {
            LId::Id { id } => Either::Left(once(id)),
            LId::Member { obj, mem } => Either::Right(once(obj).chain(mem)),
        }
    }
    pub fn map2<J, N: IntoIterator<Item = J>, E>(
        self,
        f: &mut impl FnMut(I) -> Result<J, E>,
        g: &mut impl FnMut(M) -> Result<N, E>,
    ) -> Result<LId<J, N>, E> {
        Ok(match self {
            LId::Id { id } => LId::Id { id: f(id)? },
            LId::Member { obj, mem } => LId::Member {
                obj: f(obj)?,
                mem: g(mem)?,
            },
        })
    }
}