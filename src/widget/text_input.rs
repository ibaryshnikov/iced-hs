use std::ffi::{c_char, CString};

use iced::widget::text_input;

use crate::{HaskellMessage, IcedMessage};

use super::{c_string_to_rust, ElementPtr};

type InputCallback = unsafe extern "C" fn(input: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn new_text_input(
    placeholder: *mut c_char,
    value: *mut c_char,
    maybe_on_input: Option<InputCallback>,
    on_submit_ptr: *const u8,
) -> ElementPtr {
    let placeholder = c_string_to_rust(placeholder);
    let value = c_string_to_rust(value);
    let mut text = text_input(&placeholder, &value);
    if let Some(on_input) = maybe_on_input {
        text = text.on_input(move |new_value| {
            let c_string = CString::new(new_value).expect("Should create a CString");
            let string_ptr = c_string.into_raw();
            let message_ptr = unsafe { on_input(string_ptr) };
            // free CString
            let _ = unsafe { CString::from_raw(string_ptr) };
            IcedMessage::Ptr(HaskellMessage { ptr: message_ptr })
        })
    }
    text = text.on_submit(IcedMessage::Ptr(HaskellMessage { ptr: on_submit_ptr }));
    Box::into_raw(Box::new(text.into()))
}
