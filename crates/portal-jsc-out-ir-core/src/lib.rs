#[cfg(feature = "llir")]
pub mod llir;
#[cfg(feature = "mobir")]
pub mod mobir;
#[cfg(feature = "soup")]
pub mod soup;

use std::convert::Infallible;

pub use swc_atoms::Atom;
pub use swc_common::{Mark, Span, Spanned, SyntaxContext};
macro_rules! withs {
    ($([$a:ident => $b:ident])*) => {
        $(
        #[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
        pub struct $b<T>{
            pub wrap: T,
            pub item: $a,
        }
        impl<T> $b<T>{
            pub fn as_ref<'a>(&'a self) -> $b<&'a T>{
                $b{
                    item: self.item,
                    wrap: &self.wrap,
                }
            }
            pub fn as_mut<'a>(&'a mut self) -> $b<&'a mut T>{
                $b{
                    item: self.item,
                    wrap: &mut self.wrap,
                }
            }
            pub fn map<U,E>(self, f: impl FnOnce(T) -> Result<U,E>) -> Result<$b<U>,E>{
                Ok($b{
                    wrap: f(self.wrap)?,
                    item: self.item,
                })
            }
        }
    )*
    };
}
withs!([Span => WithSpan] [SyntaxContext => Contextual]);
impl<T: Default> Default for WithSpan<T>{
    fn default() -> Self {
        Self { wrap: Default::default(), item: Span::dummy_with_cmt() }
    }
}
impl<T: Default> Default for Contextual<T>{
    fn default() -> Self {
        Self { wrap: Default::default(), item: Default::default() }
    }
}
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Debug,Default)]
pub struct ContextuallyWithSpan<T>(pub WithSpan<Contextual<T>>);
impl<T> ContextuallyWithSpan<T> {
    pub fn as_ref<'a>(&'a self) -> ContextuallyWithSpan<&'a T> {
        ContextuallyWithSpan(
            self.0
                .as_ref()
                .map(|a| Ok::<_, Infallible>(a.as_ref()))
                .unwrap(),
        )
    }
    pub fn as_mut<'a>(&'a mut self) -> ContextuallyWithSpan<&'a mut T> {
        ContextuallyWithSpan(
            self.0
                .as_mut()
                .map(|a| Ok::<_, Infallible>(a.as_mut()))
                .unwrap(),
        )
    }
    pub fn map<U, E>(
        self,
        f: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<ContextuallyWithSpan<U>, E> {
        self.0.map(move |a| a.map(f)).map(ContextuallyWithSpan)
    }
}
impl<T> Spanned for WithSpan<T> {
    fn span(&self) -> Span {
        self.item
    }
}
impl<T: Spanned> Spanned for Contextual<T> {
    fn span(&self) -> Span {
        self.wrap.span()
    }
}
impl<T> Spanned for ContextuallyWithSpan<T> {
    fn span(&self) -> Span {
        self.0.span()
    }
}
