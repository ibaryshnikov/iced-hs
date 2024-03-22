use std::ffi::c_char;

use iced::widget::{text, Button};

use crate::{HaskellMessage, IcedMessage};

use super::{c_string_to_rust, ElementPtr};

#[no_mangle]
pub extern "C" fn new_button(input: *mut c_char, message_ptr: *const u8) -> ElementPtr {
    let string = c_string_to_rust(input);
    let haskell_message = HaskellMessage { ptr: message_ptr };
    let message = IcedMessage::Ptr(haskell_message);
    let button = Button::new(text(string)).on_press(message);
    Box::into_raw(Box::new(button.into()))
}
