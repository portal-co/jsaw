#![no_std]
use core::iter::once;
pub use portal_pc_asm_common as asm;

use either::Either;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum Native<E> {
    AssertString { value: E, comptime: bool },
    AssertNumber { value: E, comptime: bool },
    AssertStaticFn { value: E, comptime: bool },
    FastAdd { lhs: E, rhs: E },
    FastAnd { lhs: E, rhs: E },
    FastOr { lhs: E, rhs: E },
    FastEq { lhs: E, rhs: E },
    FastSub { lhs: E, rhs: E },
    FastMul { lhs: E, rhs: E, imul: bool },
    FastShl { lhs: E, rhs: E },
    InlineMe,
}
impl Native<()> {
    pub fn all() -> impl Iterator<Item = Self> {
        [
            "fast_add",
            "fast_and",
            "fast_or",
            "fast_eq",
            "fast_sub",
            "fast_shl",
            "fast_mul",
            "fast_imul",
            "inlineme",
        ]
        .into_iter()
        .filter_map(|a| Self::of(a))
        .chain([true, false].into_iter().flat_map(|a| {
            [
                Self::AssertNumber {
                    value: (),
                    comptime: a,
                },
                Self::AssertStaticFn {
                    value: (),
                    comptime: a,
                },
                Self::AssertString {
                    value: (),
                    comptime: a,
                },
            ]
        }))
    }
    pub fn of(a: &str) -> Option<Self> {
        Some(match a {
            "assert_string" => Self::AssertString {
                value: (),
                comptime: false,
            },
            "assert_number" => Self::AssertNumber {
                value: (),
                comptime: false,
            },
            "assert_static_fn" => Self::AssertStaticFn {
                value: (),
                comptime: false,
            },
            "comptime_string" => Self::AssertString {
                value: (),
                comptime: true,
            },
            "comptime_number" => Self::AssertNumber {
                value: (),
                comptime: true,
            },
            "comptime_static_fn" => Self::AssertStaticFn {
                value: (),
                comptime: true,
            },
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
            "inlineme" => Self::InlineMe,
            _ => return None,
        })
    }
}
impl<E> Native<E> {
    pub fn key(&self) -> &'static str {
        match self {
            Native::AssertString { value, comptime } => {
                if *comptime {
                    "comptime_string"
                } else {
                    "assert_string"
                }
            }
            Native::AssertNumber { value, comptime } => {
                if *comptime {
                    "comptime_number"
                } else {
                    "aasert_number"
                }
            }
            Native::AssertStaticFn { value, comptime } => {
                if *comptime {
                    "comptime_static_fn"
                } else {
                    "assert_static_fn"
                }
            }
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
            Native::InlineMe => "inlineme",
        }
    }
    pub fn as_ref<'a>(&'a self) -> Native<&'a E> {
        match self {
            Native::AssertString { value, comptime } => Native::AssertString {
                value,
                comptime: *comptime,
            },
            Native::AssertNumber { value, comptime } => Native::AssertNumber {
                value,
                comptime: *comptime,
            },
            Native::AssertStaticFn { value, comptime } => Native::AssertStaticFn {
                value,
                comptime: *comptime,
            },
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
            Native::InlineMe => Native::InlineMe,
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> Native<&'a mut E> {
        match self {
            Native::AssertString { value, comptime } => Native::AssertString {
                value,
                comptime: *comptime,
            },
            Native::AssertNumber { value, comptime } => Native::AssertNumber {
                value,
                comptime: *comptime,
            },
            Native::AssertStaticFn { value, comptime } => Native::AssertStaticFn {
                value,
                comptime: *comptime,
            },
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
            Native::InlineMe => Native::InlineMe,
        }
    }
    pub fn map<E2, Er>(
        self,
        f: &mut (dyn FnMut(E) -> Result<E2, Er> + '_),
    ) -> Result<Native<E2>, Er> {
        Ok(match self {
            Native::AssertString { value, comptime } => Native::AssertString {
                value: f(value)?,
                comptime,
            },
            Native::AssertNumber { value, comptime } => Native::AssertNumber {
                value: f(value)?,
                comptime,
            },
            Native::AssertStaticFn { value, comptime } => Native::AssertStaticFn {
                value: f(value)?,
                comptime,
            },
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
            Native::InlineMe => Native::InlineMe,
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
        self.map2(f, &mut |cx, a| cx(a), &mut |cx, [a]| cx(a).map(|b| [b]))
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
    pub fn map2<Cx, J, N: IntoIterator<Item = J>, E>(
        self,
        cx: &mut Cx,
        f: &mut (dyn FnMut(&mut Cx, I) -> Result<J, E> + '_),
        g: &mut (dyn FnMut(&mut Cx, M) -> Result<N, E> + '_),
    ) -> Result<LId<J, N>, E> {
        Ok(match self {
            LId::Id { id } => LId::Id { id: f(cx, id)? },
            LId::Member { obj, mem } => LId::Member {
                obj: f(cx, obj)?,
                mem: g(cx, mem)?,
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
    pub fn as_ref(&self) -> Asm<&I> {
        match self {
            Asm::OrZero(a) => Asm::OrZero(a),
        }
    }
    pub fn as_mut(&mut self) -> Asm<&mut I> {
        match self {
            Asm::OrZero(a) => Asm::OrZero(a),
        }
    }
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
