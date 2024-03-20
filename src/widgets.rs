use std::ffi::CString;
use std::os::raw::c_char;

use iced::widget::{text, Button, Column};

use crate::{HaskellMessage, IcedMessage};

pub(crate) type ElementPtr = *mut iced::Element<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn create_text(input: *mut c_char) -> ElementPtr {
    let c_string = unsafe { CString::from_raw(input) };
    let rust_str = c_string.to_str().expect("Should convert CString to str");
    let element = text(rust_str).into();
    Box::into_raw(Box::new(element))
}

#[no_mangle]
pub extern "C" fn create_button(input: *mut c_char, message_ptr: *const u8) -> ElementPtr {
    let c_string = unsafe { CString::from_raw(input) };
    let string = c_string
        .to_str()
        .expect("Should convert CString to str")
        .to_owned();
    let haskell_message = HaskellMessage { ptr: message_ptr };
    let message = IcedMessage::Ptr(haskell_message);
    let button = Button::new(text(string)).on_press(message);
    Box::into_raw(Box::new(button.into()))
}

#[no_mangle]
pub extern "C" fn create_column(len: libc::size_t, ptr: *const ElementPtr) -> ElementPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut column = Column::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        column = column.push(*boxed);
    }
    Box::into_raw(Box::new(column.into()))
}
