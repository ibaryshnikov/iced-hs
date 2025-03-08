pub(crate) fn from_raw<T>(ptr: *mut T) -> T {
    unsafe { *Box::from_raw(ptr) }
}

pub(crate) fn into_raw<T>(value: T) -> *mut T {
    Box::into_raw(Box::new(value))
}
