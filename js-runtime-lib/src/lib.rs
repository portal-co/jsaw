use std::{
    collections::BTreeMap,
    convert::Infallible,
    sync::{LazyLock, Mutex},
};

use boxing::nan::NanBox;
use dumpster::{sync::Gc, Trace};
static UNDEF: LazyLock<Payload> = LazyLock::new(|| Payload::Undefined);

pub type OCell = Gc<Mutex<O>>;

#[derive(Clone)]
pub struct O(pub NanBox<'static, Payload>);

unsafe impl Send for O {}

impl Default for O {
    fn default() -> Self {
        Self(NanBox::from_inline(&*UNDEF))
    }
}
unsafe impl Trace for O {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        if let Some(p) = self.0.try_ref_boxed() {
            p.accept(visitor)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct PayClosureFn(pub fn(&[OCell], &[O]) -> Result<O, Err<Infallible>>);

unsafe impl Trace for PayClosureFn {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

#[derive(Clone, Trace)]
pub enum Payload {
    Undefined,
    CellVec(Gc<Mutex<Vec<O>>>),
    CallStrMap(Gc<Mutex<BTreeMap<String, O>>>),
    Closure {
        cells: Box<[OCell]>,
        r#fn: PayClosureFn,
    },
}
pub enum Err<W> {
    Rust(W),
    JS(O),
}
impl O {
    pub fn error<W, O>(self) -> Result<O, Err<W>> {
        Err(Err::JS(self))
    }
    pub fn raw(self) -> f64 {
        return self.0.into_float_unchecked();
    }
    ///SAFETY: Floats passed in MUST be from `raw`
    pub unsafe fn new_unchecked(a: f64) -> Self {
        //SAFETY: same size and layout
        unsafe { std::mem::transmute(a) }
    }
    pub fn new(a: f64) -> Self {
        Self(NanBox::from_float(a))
    }
    pub fn array(a: impl Iterator<Item = O>) -> Self {
        return Self(NanBox::from_box(Box::new(Payload::CellVec(Gc::new(
            Mutex::new(a.collect()),
        )))));
    }
    pub fn closure(a: &[OCell], b: fn(&[OCell], &[O]) -> Result<O, Err<Infallible>>) -> Self {
        return Self(NanBox::from_box(Box::new(Payload::Closure {
            cells: a.to_owned().into(),
            r#fn: PayClosureFn(b),
        })));
    }
    pub fn call(&self, args: &[O]) -> Result<O, Err<Infallible>> {
        if let Some(a) = self.0.try_ref_boxed() {
            if let Payload::Closure { cells, r#fn } = a {
                return (r#fn.0)(cells.as_ref(), args);
            }
        }
        return Err(Err::JS(O::default()));
    }
    pub fn get(&self, b: &O) -> Result<O, ()> {
        match self.0.try_ref_boxed() {
            None => {}
            Some(a) => match a {
                Payload::Undefined => {}
                Payload::CellVec(gc) => {
                    let mut lock = gc.lock().unwrap();
                    if let Some(b) = b.0.try_ref_float() {
                        let b = *b as u32;
                        if let Some(x) = lock.get((b as usize)) {
                            return Ok(x.clone());
                        }
                    }
                }
                Payload::CallStrMap(gc) => todo!(),
                Payload::Closure { cells, r#fn } => todo!(),
            },
        }
        return Err(());
    }
    pub fn set(&self, b: &O, c: &O) {
        match self.0.try_ref_boxed() {
            None => {}
            Some(a) => match a {
                Payload::Undefined => {}
                Payload::CellVec(gc) => {
                    let mut lock = gc.lock().unwrap();
                    if let Some(b) = b.0.try_ref_float() {
                        let b = *b as u32;
                        let l = lock.len();
                        lock.resize((b as usize).max(l), Default::default());
                        lock[b as usize] = c.clone();
                    }
                }
                Payload::CallStrMap(gc) => todo!(),
                Payload::Closure { cells, r#fn } => todo!(),
            },
        }
    }
}
impl<W> Err<W> {
    pub fn obj(&self) -> Option<O> {
        match self {
            Err::Rust(_) => None,
            Err::JS(o) => Some(o.clone()),
        }
    }
}
