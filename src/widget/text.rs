use std::ffi::c_char;

use iced::widget::{text, Text};

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
