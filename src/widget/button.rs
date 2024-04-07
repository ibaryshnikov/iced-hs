use std::ffi::c_char;

use iced::widget::{text, Button};
use iced::Length;

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut Button<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn button_new(input: *mut c_char) -> SelfPtr {
    let string = read_c_string(input);
    let button = Button::new(text(string));
    Box::into_raw(Box::new(button))
}

#[no_mangle]
pub extern "C" fn button_on_press(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    Box::into_raw(Box::new(button.on_press(message)))
}

#[no_mangle]
pub extern "C" fn button_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(button.width(width)))
}

#[no_mangle]
pub extern "C" fn button_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(button.height(height)))
}

#[no_mangle]
pub extern "C" fn button_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let button = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(button.into()))
}
