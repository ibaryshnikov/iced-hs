use std::ffi::c_char;

use iced::advanced::svg::Handle;

use crate::ffi::{into_raw, read_c_string, read_vec};

#[no_mangle]
extern "C" fn svg_handle_from_path(path_raw: *mut c_char) -> *mut Handle {
    let path = read_c_string(path_raw);
    into_raw(Handle::from_path(path))
}

#[no_mangle]
extern "C" fn svg_handle_from_memory(len: usize, array_ptr: *const u8) -> *mut Handle {
    let bytes = read_vec(len, array_ptr);
    into_raw(Handle::from_memory(bytes))
}
