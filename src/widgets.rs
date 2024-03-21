use std::ffi::CString;
use std::os::raw::c_char;

use iced::widget::{text, text_input, Button, Column};

use crate::{HaskellMessage, IcedMessage};

pub(crate) type ElementPtr = *mut iced::Element<'static, IcedMessage>;

fn c_string_to_rust(input: *mut c_char) -> String {
    let c_string = unsafe { CString::from_raw(input) };
    c_string
        .into_string()
        .expect("Should convert CString to String")
}

#[no_mangle]
pub extern "C" fn create_text(input: *mut c_char) -> ElementPtr {
    let string = c_string_to_rust(input);
    let element = text(string).into();
    Box::into_raw(Box::new(element))
}

type InputCallback = unsafe extern "C" fn(input: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn create_text_input(
    placeholder: *mut c_char,
    value: *mut c_char,
    cb: Option<InputCallback>,
    on_submit_ptr: *const u8,
) -> ElementPtr {
    let placeholder = c_string_to_rust(placeholder);
    let value = c_string_to_rust(value);
    let mut text = text_input(&placeholder, &value);
    if let Some(callback) = cb {
        text = text.on_input(move |new_value| {
            let c_string = CString::new(new_value).expect("Should create a CString");
            let string_ptr = c_string.into_raw();
            let message_ptr = unsafe { callback(string_ptr) };
            // free CString
            let _ = unsafe { CString::from_raw(string_ptr) };
            IcedMessage::Ptr(HaskellMessage { ptr: message_ptr })
        })
    }
    text = text.on_submit(IcedMessage::Ptr(HaskellMessage { ptr: on_submit_ptr }));
    Box::into_raw(Box::new(text.into()))
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
