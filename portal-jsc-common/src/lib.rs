#![no_std]
use core::iter::once;
pub use portal_pc_asm_common as asm;

use either::Either;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum Native<E> {
    AssertString(E),
    AssertNumber(E),
    AssertStaticFn(E),
    FastAdd { lhs: E, rhs: E },
    FastAnd { lhs: E, rhs: E },
    FastOr { lhs: E, rhs: E },
    FastEq { lhs: E, rhs: E },
    FastSub { lhs: E, rhs: E },
    FastMul { lhs: E, rhs: E, imul: bool },
    FastShl { lhs: E, rhs: E },
}
impl Native<()> {
    pub fn of(a: &str) -> Option<Self> {
        Some(match a {
            "assert_string" => Self::AssertString(()),
            "assert_number" => Self::AssertNumber(()),
            "assert_static_fn" => Self::AssertStaticFn(()),
            "fast_add" => Self::FastAdd { lhs: (), rhs: () },
            "fast_and" => Self::FastAnd { lhs: (), rhs: () },
            "fast_or" => Self::FastOr { lhs: (), rhs: () },
            "fast_eq" => Self::FastEq { lhs: (), rhs: () },
            "fast_sub" => Self::FastSub { lhs: (), rhs: () },
            "fast_shl" => Self::FastShl { lhs: (), rhs: () },
            "fast_mul" => Self::FastMul {
                lhs: (),
                rhs: (),
                imul: false,
            },
            "fast_imul" => Self::FastMul {
                lhs: (),
                rhs: (),
                imul: true,
            },
            _ => return None,
        })
    }
}
impl<E> Native<E> {
    pub fn key(&self) -> &'static str {
        match self {
            Native::AssertString(_) => "assert_string",
            Native::AssertNumber(_) => "aasert_number",
            Native::AssertStaticFn(_) => "assert_static_fn",
            Native::FastAdd { lhs, rhs } => "fast_add",
            Native::FastAnd { lhs, rhs } => "fast_and",
            Native::FastOr { lhs, rhs } => "fast_or",
            Native::FastEq { lhs, rhs } => "fast_eq",
            Native::FastSub { lhs, rhs } => "fast_sub",
            Native::FastMul { lhs, rhs, imul } => {
                if *imul {
                    "fast_imul"
                } else {
                    "fast_mul"
                }
            }
            Native::FastShl { lhs, rhs } => "fast_shl",
        }
    }
    pub fn as_ref<'a>(&'a self) -> Native<&'a E> {
        match self {
            Native::AssertString(a) => Native::AssertString(a),
            Native::AssertNumber(a) => Native::AssertNumber(a),
            Native::AssertStaticFn(a) => Native::AssertStaticFn(a),
            Native::FastAdd { lhs, rhs } => Native::FastAdd { lhs, rhs },
            Native::FastAnd { lhs, rhs } => Native::FastAnd { lhs, rhs },
            Native::FastOr { lhs, rhs } => Native::FastOr { lhs, rhs },
            Native::FastEq { lhs, rhs } => Native::FastEq { lhs, rhs },
            Native::FastSub { lhs, rhs } => Native::FastSub { lhs, rhs },
            Native::FastMul { lhs, rhs, imul } => Native::FastMul {
                lhs,
                rhs,
                imul: *imul,
            },
            Native::FastShl { lhs, rhs } => Native::FastShl { lhs, rhs },
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> Native<&'a mut E> {
        match self {
            Native::AssertString(a) => Native::AssertString(a),
            Native::AssertNumber(a) => Native::AssertNumber(a),
            Native::AssertStaticFn(a) => Native::AssertStaticFn(a),
            Native::FastAdd { lhs, rhs } => Native::FastAdd { lhs, rhs },
            Native::FastAnd { lhs, rhs } => Native::FastAnd { lhs, rhs },
            Native::FastOr { lhs, rhs } => Native::FastOr { lhs, rhs },
            Native::FastEq { lhs, rhs } => Native::FastEq { lhs, rhs },
            Native::FastSub { lhs, rhs } => Native::FastSub { lhs, rhs },
            Native::FastMul { lhs, rhs, imul } => Native::FastMul {
                lhs,
                rhs,
                imul: *imul,
            },
            Native::FastShl { lhs, rhs } => Native::FastShl { lhs, rhs },
        }
    }
    pub fn map<E2, Er>(self, f: &mut impl FnMut(E) -> Result<E2, Er>) -> Result<Native<E2>, Er> {
        Ok(match self {
            Native::AssertString(a) => Native::AssertString(f(a)?),
            Native::AssertNumber(a) => Native::AssertNumber(f(a)?),
            Native::AssertStaticFn(a) => Native::AssertStaticFn(f(a)?),
            Native::FastAdd { lhs, rhs } => Native::FastAdd {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
            },
            Native::FastAnd { lhs, rhs } => Native::FastAnd {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
            },
            Native::FastOr { lhs, rhs } => Native::FastOr {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
            },
            Native::FastEq { lhs, rhs } => Native::FastEq {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
            },
            Native::FastSub { lhs, rhs } => Native::FastSub {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
            },
            Native::FastMul { lhs, rhs, imul } => Native::FastMul {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
                imul,
            },
            Native::FastShl { lhs, rhs } => Native::FastShl {
                lhs: f(lhs)?,
                rhs: f(rhs)?,
            },
        })
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
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
    pub fn as_ref<'a>(&'a self) -> LId<&'a I, &'a M>
    where
        &'a M: IntoIterator<Item = &'a I>,
    {
        match self {
            LId::Id { id } => LId::Id { id },
            LId::Member { obj, mem } => LId::Member { obj, mem },
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> LId<&'a mut I, &'a mut M>
    where
        &'a mut M: IntoIterator<Item = &'a mut I>,
    {
        match self {
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
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ImportMap<T> {
    Default,
    Star,
    Named { name: T },
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[non_exhaustive]
pub enum Asm<I> {
    OrZero(I),
}
impl<I> Asm<I> {
    pub fn map<J, E>(self, f: &mut impl FnMut(I) -> Result<J, E>) -> Result<Asm<J>, E> {
        Ok(match self {
            Asm::OrZero(a) => Asm::OrZero(f(a)?),
        })
    }
    pub fn refs(&self) -> impl Iterator<Item = &I> {
        match self {
            Asm::OrZero(a) => once(a),
        }
    }
    pub fn refs_mut(&mut self) -> impl Iterator<Item = &mut I> {
        match self {
            Asm::OrZero(a) => once(a),
        }
    }
}
