pub fn throw_type_error<T>() -> T{
    panic!("type error")
}
pub fn throw_runtime_error<T>() -> T{
    panic!("runtime error")
}
pub fn throw_reference_error<T>() -> T{
    panic!("reference error")
}