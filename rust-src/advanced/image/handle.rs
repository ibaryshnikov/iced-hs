use std::ffi::{c_char, c_uint};

use iced::advanced::image::Handle;

use crate::ffi::{into_raw, read_c_string, read_vec};

#[no_mangle]
extern "C" fn image_handle_from_path(path_raw: *mut c_char) -> *mut Handle {
    let path = read_c_string(path_raw);
    into_raw(Handle::from_path(path))
}

#[no_mangle]
extern "C" fn image_handle_from_bytes(len: usize, array_ptr: *const u8) -> *mut Handle {
    let bytes = read_vec(len, array_ptr);
    into_raw(Handle::from_bytes(bytes))
}

#[no_mangle]
extern "C" fn image_handle_from_rgba(
    width: c_uint,
    height: c_uint,
    len: usize,
    array_ptr: *const u8,
) -> *mut Handle {
    let pixels = read_vec(len, array_ptr);
    into_raw(Handle::from_rgba(width, height, pixels))
}
