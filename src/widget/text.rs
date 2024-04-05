use std::ffi::{c_char, c_float};

use iced::widget::{text, Text};
use iced::Length;

use super::{c_string_to_rust, ElementPtr};

type SelfPtr = *mut Text<'static>;

#[no_mangle]
pub extern "C" fn text_new(input: *mut c_char) -> SelfPtr {
    let string = c_string_to_rust(input);
    Box::into_raw(Box::new(text(string)))
}

#[no_mangle]
pub extern "C" fn text_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(text.height(height)))
}

#[no_mangle]
pub extern "C" fn text_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text.size(size)))
}

#[no_mangle]
pub extern "C" fn text_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(text.width(width)))
}

#[no_mangle]
pub extern "C" fn text_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text.into()))
}
