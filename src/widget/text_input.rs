use std::ffi::{c_char, c_float};

use iced::widget::{text_input, TextInput};
use iced::Padding;

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut TextInput<'static, IcedMessage>;

type OnInputFFI = unsafe extern "C" fn(input: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn text_input_new(placeholder: *mut c_char, value: *mut c_char) -> SelfPtr {
    let placeholder = read_c_string(placeholder);
    let value = read_c_string(value);
    Box::into_raw(Box::new(text_input(&placeholder, &value)))
}

#[no_mangle]
pub extern "C" fn text_input_on_input(self_ptr: SelfPtr, on_input_ffi: OnInputFFI) -> SelfPtr {
    let text_input = unsafe { *Box::from_raw(self_ptr) };
    let on_input = super::wrap_callback_with_string(on_input_ffi);
    Box::into_raw(Box::new(text_input.on_input(on_input)))
}

#[no_mangle]
pub extern "C" fn text_input_on_submit(self_ptr: SelfPtr, on_submit_ptr: *const u8) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    let text_input = text_input.on_submit(IcedMessage::ptr(on_submit_ptr));
    Box::into_raw(Box::new(text_input))
}

#[no_mangle]
pub extern "C" fn text_input_padding(
    self_ptr: SelfPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(text_input.padding(padding)))
}

#[no_mangle]
pub extern "C" fn text_input_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text_input = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_input.into()))
}
