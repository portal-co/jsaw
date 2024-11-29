use crate::*;

#[unsafe(export_name = "jrl_result_value")]
extern "C" fn jrl_result_value(a: Box<Result<O, Err<Infallible>>>) -> O {
    match *a {
        Ok(a) => a,
        Result::Err(e) => match e {
            Err::Rust(a) => match a {},
            Err::JS(o) => o,
        },
    }
}
#[unsafe(export_name = "jrl_result_ok")]
extern "C" fn jrl_result_ok(a: &Result<O, Err<Infallible>>) -> u32 {
    a.is_ok() as u32
}
#[unsafe(export_name = "jrl_result_new")]
extern "C" fn jrl_result_new(a: O, b: u32) -> Box<Result<O, Err<Infallible>>> {
    Box::new(if b == 0 {
        Result::Err(Err::JS(a))
    } else {
        Ok(a)
    })
}
#[unsafe(export_name = "jrl_cell_new")]
extern "C" fn jrl_cell_new(a: O) -> Box<OCell> {
    Box::new(OCell::new(Mutex::new(a)))
}
#[unsafe(export_name = "jrl_cell_destroy")]
extern "C" fn jrl_cell_destroy(a: Box<OCell>) {}
#[unsafe(export_name = "jrl_cell_get")]
extern "C" fn jrl_cell_get(a: &OCell) -> O {
    a.lock().unwrap().clone()
}
#[unsafe(export_name = "jrl_cell_set")]
extern "C" fn jrl_cell_set(a: &OCell, b: O) {
    *a.lock().unwrap() = b;
}
#[unsafe(export_name = "jrl_undef")]
extern "C" fn jrl_undef() -> O {
    return O::default();
}
#[unsafe(export_name = "jrl_member_set")]
extern "C" fn jrl_member_set(a: O, b: O, c: O) {
    a.set(&b, &c);
}
#[unsafe(export_name = "jrl_member_get")]
extern "C" fn jrl_member_get(a: O, b: O) -> Box<Result<O, Err<Infallible>>> {
    return Box::new(match a.get(&b) {
        Ok(a) => Ok(a),
        Err(_) => crate::synth::throw_reference_error(),
    });
}
#[unsafe(export_name = "jrl_object_new_float")]
extern "C" fn jrl_object_new_float(f: f64) -> O {
    return O::new(f);
}
#[unsafe(export_name = "jrl_object_clone")]
extern "C" fn jrl_object_clone(f: f64) -> O {
    let x = &f as *const f64 as *const O;
    return unsafe { &*x }.clone();
}
#[unsafe(export_name = "jrl_object_drop")]
extern "C" fn jrl_object_drop(x: O) {

}
#[unsafe(export_name = "jrl_object_as_float")]
extern "C" fn jrl_object_as_float(x: O) -> f64{
    x.float().unwrap()
}
#[unsafe(export_name = "jrl_object_is_float")]
extern "C" fn jrl_object_is_float(x: O) -> u32{
    x.float().is_some() as u32
}
#[unsafe(export_name = "jrl_object_new_u64")]
extern "C" fn jrl_object_new_u64(a: u64) -> O {
    return Payload::Big(a.into()).into();
}
#[unsafe(export_name = "jrl_object_as_u64")]
extern "C" fn jrl_object_as_u64(x: O) -> u64{
    x.coe_u64().unwrap()
}
#[unsafe(export_name = "jrl_object_is_u64")]
extern "C" fn jrl_object_is_u64(x: O) -> u32{
    x.coe_u64().is_some() as u32
}