use std::ffi::c_char;

use iced::widget::text;

use super::{c_string_to_rust, ElementPtr};

#[no_mangle]
pub extern "C" fn new_text(input: *mut c_char) -> ElementPtr {
    let string = c_string_to_rust(input);
    let element = text(string).into();
    Box::into_raw(Box::new(element))
}
