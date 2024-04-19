use std::ffi::{c_char, c_uchar, CString};

mod button;
mod canvas;
mod checkbox;
mod column;
mod combo_box;
mod container;
mod pick_list;
mod radio;
mod responsive;
mod row;
mod scrollable;
mod slider;
mod space;
mod text;
mod text_editor;
mod text_input;
mod toggler;
mod tooltip;
mod vertical_slider;

use crate::IcedMessage;

pub type ElementPtr = *mut iced::Element<'static, IcedMessage>;

pub fn read_c_string(input: *mut c_char) -> String {
    let c_string = unsafe { CString::from_raw(input) };
    c_string
        .into_string()
        .expect("Should convert CString to String")
}

fn read_c_bool(input: c_uchar) -> bool {
    match input {
        0 => false,
        1 => true,
        other => panic!("Non boolean value passed as CBool: {other}"),
    }
}

fn read_array_of_c_strings(
    len: usize,
    array_ptr: *const *mut c_char, // array of CString
) -> Vec<String> {
    let slice = unsafe { std::slice::from_raw_parts(array_ptr, len) };
    let mut strings = vec![];
    for option_ptr in slice {
        let option = read_c_string(*option_ptr);
        strings.push(option);
    }
    strings
}

// "" -> None
// s -> Some(s)
fn read_c_string_to_option(input: *mut c_char) -> Option<String> {
    let string = read_c_string(input);
    if string.is_empty() {
        None
    } else {
        Some(string)
    }
}

type CallbackForCString = unsafe extern "C" fn(input: *mut c_char) -> *const u8;

fn wrap_callback_with_string(callback: CallbackForCString) -> impl Fn(String) -> IcedMessage {
    move |input| {
        let c_string = CString::new(input).expect("Should create a CString");
        let string_ptr = c_string.into_raw();
        let message_ptr = unsafe { callback(string_ptr) };
        // free CString
        let _ = unsafe { CString::from_raw(string_ptr) };
        IcedMessage::ptr(message_ptr)
    }
}

type CallbackForCBool = unsafe extern "C" fn(input: c_uchar) -> *const u8;

fn wrap_callback_with_bool(callback: CallbackForCBool) -> impl Fn(bool) -> IcedMessage {
    move |input| {
        let message_ptr = unsafe { callback(input.into()) };
        IcedMessage::ptr(message_ptr)
    }
}
