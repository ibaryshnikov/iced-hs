use std::ffi::{c_char, CString};

use iced::widget::{text_input, TextInput};

use super::{c_string_to_rust, ElementPtr, IcedMessage};

type SelfPtr = *mut TextInput<'static, IcedMessage>;

type InputCallback = unsafe extern "C" fn(input: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn text_input_new(placeholder: *mut c_char, value: *mut c_char) -> SelfPtr {
    let placeholder = c_string_to_rust(placeholder);
    let value = c_string_to_rust(value);
    Box::into_raw(Box::new(text_input(&placeholder, &value)))
}

#[no_mangle]
pub extern "C" fn text_input_on_input(self_ptr: SelfPtr, on_input: InputCallback) -> SelfPtr {
    let mut text_input = unsafe { *Box::from_raw(self_ptr) };
    text_input = text_input.on_input(move |new_value| {
        let c_string = CString::new(new_value).expect("Should create a CString");
        let string_ptr = c_string.into_raw();
        let message_ptr = unsafe { on_input(string_ptr) };
        // free CString
        let _ = unsafe { CString::from_raw(string_ptr) };
        IcedMessage::ptr(message_ptr)
    });
    Box::into_raw(Box::new(text_input))
}

#[no_mangle]
pub extern "C" fn text_input_on_submit(self_ptr: SelfPtr, on_submit_ptr: *const u8) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    let text_input = text_input.on_submit(IcedMessage::ptr(on_submit_ptr));
    Box::into_raw(Box::new(text_input))
}

#[no_mangle]
pub extern "C" fn text_input_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text_input = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_input.into()))
}
