use std::ffi::{c_char, c_uchar, CString};

use iced::widget::text::Shaping;

mod button;
mod canvas;
mod checkbox;
mod column;
mod combo_box;
mod container;
mod image;
mod markdown;
mod mouse_area;
mod pick_list;
mod progress_bar;
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

use crate::ffi::read_c_string;
use crate::IcedMessage;

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

type CallbackForCString = extern "C" fn(input: *mut c_char) -> *const u8;

fn wrap_callback_with_string(callback: CallbackForCString) -> impl Fn(String) -> IcedMessage {
    move |input| {
        let c_string = CString::new(input).expect("Should create a CString");
        let string_ptr = c_string.into_raw();
        let message_ptr = callback(string_ptr);
        // free CString
        let _ = unsafe { CString::from_raw(string_ptr) };
        IcedMessage::ptr(message_ptr)
    }
}

type CallbackForCBool = extern "C" fn(input: c_uchar) -> *const u8;

fn wrap_callback_with_bool(callback: CallbackForCBool) -> impl Fn(bool) -> IcedMessage {
    move |input| {
        let message_ptr = callback(input.into());
        IcedMessage::ptr(message_ptr)
    }
}

fn read_shaping(input: c_uchar) -> Shaping {
    match input {
        0 => Shaping::Basic,
        1 => Shaping::Advanced,
        other => panic!("Unexpected Shaping value: {other}"),
    }
}
