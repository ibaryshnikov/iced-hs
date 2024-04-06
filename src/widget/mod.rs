use std::ffi::{c_char, CString};

mod button;
mod canvas;
mod checkbox;
mod column;
mod container;
mod pick_list;
mod row;
mod scrollable;
mod space;
mod text;
mod text_editor;
mod text_input;
mod tooltip;

use crate::IcedMessage;

pub(crate) type ElementPtr = *mut iced::Element<'static, IcedMessage>;

pub(crate) fn c_string_to_rust(input: *mut c_char) -> String {
    let c_string = unsafe { CString::from_raw(input) };
    c_string
        .into_string()
        .expect("Should convert CString to String")
}
