use std::ffi::c_uchar;

use canvas::fill::Rule;
use canvas::{Fill, Style};
use iced::widget::canvas;

#[no_mangle]
extern "C" fn canvas_fill_new(style_ptr: *mut Style, rule_raw: c_uchar) -> *mut Fill {
    let style = unsafe { *Box::from_raw(style_ptr) };
    let rule = match rule_raw {
        0 => Rule::NonZero,
        1 => Rule::EvenOdd,
        other => panic!("Unexpected Rule value in canvas_fill_new: {other}"),
    };
    Box::into_raw(Box::new(Fill { style, rule }))
}
