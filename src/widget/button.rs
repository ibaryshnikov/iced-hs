use std::ffi::c_char;

use iced::widget::{text, Button};
use iced::Length;

use crate::{HaskellMessage, IcedMessage};

use super::{c_string_to_rust, ElementPtr};

type ButtonPtr = *mut Button<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn new_button(input: *mut c_char) -> ButtonPtr {
    let string = c_string_to_rust(input);
    let button = Button::new(text(string));
    Box::into_raw(Box::new(button))
}

#[no_mangle]
pub extern "C" fn button_on_press(pointer: ButtonPtr, message_ptr: *const u8) -> ButtonPtr {
    let button = unsafe { Box::from_raw(pointer) };
    let haskell_message = HaskellMessage { ptr: message_ptr };
    let message = IcedMessage::Ptr(haskell_message);
    Box::into_raw(Box::new(button.on_press(message)))
}

#[no_mangle]
pub extern "C" fn button_into_element(pointer: ButtonPtr) -> ElementPtr {
    let button = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(button.into()))
}

#[no_mangle]
pub extern "C" fn button_height(pointer: ButtonPtr, height: *mut Length) -> ButtonPtr {
    let button = unsafe { Box::from_raw(pointer) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(button.height(height)))
}

#[no_mangle]
pub extern "C" fn button_width(pointer: ButtonPtr, width: *mut Length) -> ButtonPtr {
    let button = unsafe { Box::from_raw(pointer) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(button.width(width)))
}
