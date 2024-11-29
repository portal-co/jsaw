#[cfg(any(target_family = "wasm", test))]
mod wasm;

pub mod synth;

use core::f64;
use std::{
    borrow::Cow,
    collections::BTreeMap,
    convert::Infallible,
    str::FromStr,
    sync::{LazyLock, Mutex},
};

use boxing::nan::NanBox;
use dumpster::{sync::Gc, Trace};
use itertools::Itertools;
static UNDEF: LazyLock<Payload> = LazyLock::new(|| Payload::Undefined);

pub type OCell = Gc<Mutex<O>>;

#[derive(Clone)]
pub struct O(pub NanBox<'static, Payload>);

pub type R = Box<Result<O, Err<Infallible>>>;

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
pub struct PayClosureFn(pub fn(*const Box<OCell>, usize, &O, *const O, usize) -> R);

unsafe impl Trace for PayClosureFn {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

#[derive(Clone)]
pub enum Payload {
    Undefined,
    CellVec(Gc<Mutex<Vec<O>>>),
    CellMap(Gc<Mutex<BTreeMap<String, O>>>),
    Closure {
        cells: Box<[Box<OCell>]>,
        r#fn: PayClosureFn,
    },
    Str(String),
    Flu(u64),
    Big(num_bigint::BigInt),
}
unsafe impl Trace for Payload {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Payload::Undefined => Ok(()),
            Payload::CellVec(gc) => gc.accept(visitor),
            Payload::CellMap(gc) => gc.accept(visitor),
            Payload::Closure { cells, r#fn } => cells.accept(visitor),
            Payload::Str(_) => Ok(()),
            Payload::Flu(_) => Ok(()),
            Payload::Big(big_int) => Ok(()),
        }
    }
}
impl From<Payload> for O {
    fn from(value: Payload) -> Self {
        Self(NanBox::from_box(Box::new(value)))
    }
}

pub enum Err<W> {
    Rust(W),
    JS(O),
}
macro_rules! binary_shim {
    ($name:ident) => {
        #[cfg(any(target_family = "wasm", test))]
        const _: () = {
            #[unsafe(export_name = concat!("jrl_object_",stringify!(name)))]
            fn go(a: O, b: O) -> R {
                return a.$name(&b);
            }
        };
    };
}
macro_rules! binop {
    ($name:ident  => |$lhs:ident, $rhs:ident| $body:expr; $big_body:expr) => {
        pub fn $name(&self, other: &Self) -> R {
            binary_shim!($name);
            match (self.float(), other.float()) {
                (Some($lhs), Some($rhs)) => return Box::new(Ok(O::new($body))),
                _ => match (self.pay(), other.pay()) {
                    (Some(Payload::Big($lhs)), Some(Payload::Big($rhs))) => {
                        return Box::new(Ok(Payload::Big($big_body).into()))
                    }
                    _ => crate::synth::throw_type_error(),
                },
            }
        }
    };
}
macro_rules! cmpop {
    ($name:ident  => |$lhs:ident, $rhs:ident| $body:expr) => {
        pub fn $name(&self, other: &Self) -> R {
            binary_shim!($name);
            match (self.pay(), other.pay()) {
                (
                    Some(Payload::Str(_) | Payload::CellVec(_)),
                    Some(Payload::Str(_) | Payload::CellVec(_)),
                ) => {
                    let $lhs = self.coe_str(false).unwrap();
                    let $rhs = self.coe_str(false).unwrap();
                    return Box::new(Ok(O::bool($body)));
                }
                _ => match (self.float(), other.float()) {
                    (Some($lhs), Some($rhs)) => return Box::new(Ok(O::bool($body))),
                    _ => match (self.pay(), other.pay()) {
                        (Some(Payload::Big($lhs)), Some(Payload::Big($rhs))) => {
                            return Box::new(Ok(O::bool($body)))
                        }
                        _ => crate::synth::throw_type_error(),
                    },
                },
            }
        }
    };
}
macro_rules! unary_shim {
    ($name:ident) => {
        #[cfg(any(target_family = "wasm", test))]
        const _: () = {
            #[unsafe(export_name = concat!("jrl_object_",stringify!(name)))]
            fn go(a: O) -> R {
                return a.$name();
            }
        };
    };
}
macro_rules! uniop {
    ($name:ident => |$val:ident|$body:expr) => {
        pub fn $name(&self) -> R {
            unary_shim!($name);
            match self.float() {
                Some($val) => return Box::new(Ok(O::new($body))),
                None => crate::synth::throw_type_error(),
            }
        }
    };
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
        if a.is_nan() {
            return Payload::Flu(unsafe { std::mem::transmute(a) }).into();
        }
        Self(NanBox::from_float(a))
    }
    pub fn bool(b: bool) -> Self {
        Self(NanBox::from_inline(b))
    }
    pub fn float(&self) -> Option<f64> {
        match self.0.try_ref_float() {
            Some(f) => Some(*f),
            None => match self.pay() {
                Some(Payload::Flu(u)) => Some(unsafe { std::mem::transmute(*u) }),
                _ => None,
            },
        }
    }
    pub fn array(a: impl Iterator<Item = O>) -> Self {
        return Self(NanBox::from_box(Box::new(Payload::CellVec(Gc::new(
            Mutex::new(a.collect()),
        )))));
    }
    pub fn closure(a: &[OCell], b: fn(*const Box<OCell>, usize, &O, *const O, usize) -> R) -> Self {
        return Self(NanBox::from_box(Box::new(Payload::Closure {
            cells: a.iter().cloned().map(Box::new).collect(),
            r#fn: PayClosureFn(b),
        })));
    }
    pub fn call(&self, this: &O, args: &[O]) -> Result<O, Err<Infallible>> {
        if let Some(a) = self.0.try_ref_boxed() {
            if let Payload::Closure { cells, r#fn } = a {
                return *(r#fn.0)(
                    cells.as_ref().as_ptr(),
                    cells.len(),
                    this,
                    args.as_ptr(),
                    args.len(),
                );
            }
        }
        return Err(Err::JS(O::default()));
    }
    pub fn pay(&self) -> Option<&Payload> {
        self.0
            .try_ref_boxed()
            .or_else(|| self.0.clone().try_into_inline().ok())
    }
    pub fn get(&self, b: &O) -> Result<O, ()> {
        match self.pay() {
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
                Payload::Closure { cells, r#fn } => todo!(),
                Payload::CellMap(gc) => todo!(),
                Payload::Str(_) => todo!(),
                Payload::Flu(_) => {}
                Payload::Big(big_int) => todo!(),
            },
        }
        return Err(());
    }
    pub fn set(&self, b: &O, c: &O) {
        match self.pay() {
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
                Payload::Closure { cells, r#fn } => todo!(),
                Payload::CellMap(gc) => todo!(),
                Payload::Str(_) => todo!(),
                Payload::Flu(_) => {}
                Payload::Big(big_int) => todo!(),
            },
        }
    }
    pub fn coe_str(&self, array: bool) -> Option<Cow<'static, str>> {
        match self.float() {
            Some(a) => Some(Cow::Owned(a.to_string())),
            None => match self.pay() {
                None => None,
                Some(p) => match p {
                    Payload::Undefined => Some(if array {
                        Cow::Borrowed("")
                    } else {
                        Cow::Borrowed("undefined")
                    }),
                    Payload::CellVec(gc) => gc
                        .lock()
                        .ok()?
                        .iter()
                        .map(|a| a.coe_str(true))
                        .collect::<Option<Vec<_>>>()
                        .map(|a| a.join(","))
                        .map(Cow::Owned),
                    Payload::CellMap(gc) => Some(Cow::Borrowed("[object Object]")),
                    Payload::Closure { cells, r#fn } => todo!(),
                    Payload::Str(s) => Some(Cow::Owned(s.clone())),
                    Payload::Flu(_) => todo!(),
                    Payload::Big(big_int) => Some(Cow::Owned(big_int.to_str_radix(10))),
                },
            },
        }
    }
    pub fn coe_number(&self) -> Option<f64> {
        match self.float() {
            Some(a) => Some(a),
            None => Some(
                FromStr::from_str(self.coe_str(false)?.as_ref())
                    .ok()
                    .unwrap_or_else(|| f64::NAN),
            ),
        }
    }
    pub fn coe_u64(&self) -> Option<u64>{
        match self.coe_number().and_then(|a|num_traits::cast::<_,u64>(a)){
            Some(a) => Some(a),
            None => match self.pay()?{
                Payload::Big(b) => Some(b.iter_u64_digits().last().unwrap_or(0)),
                _ => None
            },
        }
    }
    pub fn BinaryOp_Add(&self, other: &Self) -> R {
        binary_shim!(BinaryOp_Add);
        match (self.pay(), other.pay()) {
            (
                Some(Payload::Str(_) | Payload::CellVec(_)),
                Some(Payload::Str(_) | Payload::CellVec(_)),
            ) => {
                return Box::new(Ok(Payload::Str(format!(
                    "{}{}",
                    self.coe_str(false).unwrap(),
                    other.coe_str(false).unwrap()
                ))
                .into()))
            },
            _ => match (self.float(), other.float()) {
                (Some(a), Some(b)) => return Box::new(Ok(O::new(a + b))),
                _ => match (self.pay(), other.pay()) {
                    (Some(Payload::Big(a)), Some(Payload::Big(b))) => {
                        Box::new(Ok(Payload::Big(a + b).into()))
                    }
                    _ => crate::synth::throw_type_error(),
                },
            },
        }
    }
    pub fn UnaryOp_Plus(&self) -> R {
        unary_shim!(UnaryOp_Plus);
        match self.coe_str(false) {
            Some(s) => match f64::from_str(s.as_ref()) {
                Ok(a) => Box::new(Ok(O::new(a))),
                Result::Err(_) => Box::new(Ok(O::new(f64::NAN))),
            },
            _ => crate::synth::throw_type_error(),
        }
    }
    binop!(BinaryOp_Sub => |lhs,rhs| lhs - rhs; lhs - rhs);
    binop!(BinaryOp_Mul => |lhs,rhs| lhs * rhs; lhs * rhs);
    binop!(BinaryOp_Div => |lhs,rhs|lhs / rhs; lhs / rhs);
    binop!(BinaryOp_Mod => |lhs,rhs|lhs % rhs; lhs % rhs);
    binop!(BinaryOp_Exp => |lhs,rhs|lhs.powf(rhs); lhs.pow((rhs.iter_u32_digits().last().unwrap_or(0))));
    binop!(BinaryOp_BitAnd => |lhs,rhs| ((lhs as i32) & (rhs as i32)) as f64; lhs & rhs);
    binop!(BinaryOp_BitOr => |lhs,rhs| ((lhs as i32) | (rhs as i32)) as f64; lhs | rhs);
    binop!(BinaryOp_BitXor => |lhs,rhs| ((lhs as i32) ^ (rhs as i32)) as f64; lhs ^ rhs);
    binop!(BinaryOp_LShift => |lhs,rhs| ((lhs as i32) << (rhs as i32)) as f64; lhs << (rhs.iter_u64_digits().last().unwrap_or(0)));
    binop!(BinaryOp_RShift => |lhs,rhs| ((lhs as i32) >> (rhs as i32)) as f64; lhs >> (rhs.iter_u64_digits().last().unwrap_or(0)));
    binop!(BinaryOp_ZeroFillRShift => |lhs,rhs| ((lhs as u32 as i32) >> (rhs as u32 as i32)) as u32 as f64; lhs >> (rhs.iter_u64_digits().last().unwrap_or(0)));
    cmpop!(BinaryOp_Lt => |lhs,rhs| lhs < rhs);
    cmpop!(BinaryOp_LtEq => |lhs,rhs| lhs <= rhs);
    cmpop!(BinaryOp_Gt => |lhs,rhs| lhs > rhs);
    cmpop!(BinaryOp_GtEq => |lhs,rhs| lhs >= rhs);
    uniop!(UnaryOp_Minus => |val|-val);
    uniop!(UnaryOp_Tilde => |val|((val as i32 as u32) ^ 0xffffffff) as i32 as f64);
}
impl<W> Err<W> {
    pub fn obj(&self) -> Option<O> {
        match self {
            Err::Rust(_) => None,
            Err::JS(o) => Some(o.clone()),
        }
    }
}
