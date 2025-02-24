use std::ffi::{c_char, c_uint};

use iced::widget::{image, Image};
use iced::Length;
use image::Handle;

use super::{read_c_string, read_vec, ElementPtr};

type SelfPtr = *mut Image<Handle>;

#[no_mangle]
extern "C" fn image_handle_from_path(path_raw: *mut c_char) -> *mut Handle {
    let path = read_c_string(path_raw);
    Box::into_raw(Box::new(Handle::from_path(path)))
}

#[no_mangle]
extern "C" fn image_handle_from_bytes(len: usize, array_ptr: *const u8) -> *mut Handle {
    let bytes = read_vec(len, array_ptr);
    Box::into_raw(Box::new(Handle::from_bytes(bytes)))
}

#[no_mangle]
extern "C" fn image_handle_from_rgba(
    width: c_uint,
    height: c_uint,
    len: usize,
    array_ptr: *const u8,
) -> *mut Handle {
    let pixels = read_vec(len, array_ptr);
    Box::into_raw(Box::new(Handle::from_rgba(width, height, pixels)))
}

#[no_mangle]
extern "C" fn image_new(handle_ptr: *mut Handle) -> SelfPtr {
    let handle = unsafe { *Box::from_raw(handle_ptr) };
    Box::into_raw(Box::new(image(handle)))
}

#[no_mangle]
extern "C" fn image_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let image = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(image.width(width)))
}

#[no_mangle]
extern "C" fn image_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let image = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(image.height(height)))
}

#[no_mangle]
extern "C" fn image_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let image = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(image.into()))
}
