use std::ffi::{c_char, c_float};

use iced::widget::{text, Text};
use iced::Length;

type TextPtr = *mut Text<'static>;

use super::{c_string_to_rust, ElementPtr};

#[no_mangle]
pub extern "C" fn new_text(input: *mut c_char) -> TextPtr {
    let string = c_string_to_rust(input);
    Box::into_raw(Box::new(text(string)))
}

#[no_mangle]
pub extern "C" fn text_into_element(pointer: TextPtr) -> ElementPtr {
    let text = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(text.into()))
}

#[no_mangle]
pub extern "C" fn text_height(pointer: TextPtr, height: *mut Length) -> TextPtr {
    let text = unsafe { Box::from_raw(pointer) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(text.height(height)))
}

#[no_mangle]
pub extern "C" fn text_size(pointer: TextPtr, size: c_float) -> TextPtr {
    let text = unsafe { Box::from_raw(pointer) };
    Box::into_raw(Box::new(text.size(size)))
}

#[no_mangle]
pub extern "C" fn text_width(pointer: TextPtr, width: *mut Length) -> TextPtr {
    let text = unsafe { Box::from_raw(pointer) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(text.width(width)))
}
