use std::sync::LazyLock;

use boxing::nan::NanBox;
static UNDEF: LazyLock<Payload> = LazyLock::new(||Payload::Undefined);
#[derive(Clone)]
pub struct O(pub NanBox<'static,Payload>);
impl Default for O{
    fn default() -> Self {
        Self(NanBox::from_inline(&*UNDEF))
    }
}
#[derive(Clone)]
pub enum Payload{
    Undefined
}
pub enum Err<W>{
    Rust(W),
    JS(O)
}
impl O{
    pub fn error<W,O>(self) -> Result<O,Err<W>>{
        Err(Err::JS(self))
    }
    pub fn raw(self) -> f64{
        return self.0.into_float_unchecked();
    }
    ///SAFETY: Floats passed in MUST be from `raw`
    pub unsafe fn get(a: f64) -> Self{
        //SAFETY: same size and layout
        unsafe{
            std::mem::transmute(a)
        }
    }
}
impl<W> Err<W>{
    pub fn obj(&self) -> Option<O>{
        match self{
            Err::Rust(_) => None,
            Err::JS(o) => Some(o.clone()),
        }
    }
}