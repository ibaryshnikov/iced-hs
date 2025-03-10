use std::ffi::{c_char, c_uchar, CString};

pub(crate) fn from_raw<T>(ptr: *mut T) -> T {
    unsafe { *Box::from_raw(ptr) }
}

pub(crate) fn into_raw<T>(value: T) -> *mut T {
    Box::into_raw(Box::new(value))
}

pub(crate) fn read_c_string(input: *mut c_char) -> String {
    let c_string = unsafe { CString::from_raw(input) };
    c_string
        .into_string()
        .expect("Should convert CString to String")
}

pub(crate) fn read_vec(len: usize, array_ptr: *const u8) -> Vec<u8> {
    let slice = unsafe { std::slice::from_raw_parts(array_ptr, len) };
    slice.to_vec()
}

pub(crate) fn read_c_bool(input: c_uchar) -> bool {
    match input {
        0 => false,
        1 => true,
        other => panic!("Non boolean value passed as CBool: {other}"),
    }
}
