use std::ffi::{c_char, c_float, c_uchar};

use iced::widget::{text, Text};
use iced::{Color, Length};
use text::Style;

use super::ElementPtr;
use crate::ffi::read_c_string;

type SelfPtr = *mut Text<'static>;

type StyleCallback = extern "C" fn(style: &mut Style, theme: c_uchar);

#[no_mangle]
extern "C" fn text_new(input: *mut c_char) -> SelfPtr {
    let string = read_c_string(input);
    Box::into_raw(Box::new(text(string)))
}

#[no_mangle]
extern "C" fn text_color(self_ptr: SelfPtr, color_ptr: *mut Color) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    let color = unsafe { *Box::from_raw(color_ptr) };
    Box::into_raw(Box::new(text.color(color)))
}

#[no_mangle]
extern "C" fn text_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text.size(size)))
}

#[no_mangle]
extern "C" fn text_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text.style(move |theme| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let mut style = Style::default();
        callback(&mut style, theme_raw);
        style
    })))
}

#[no_mangle]
extern "C" fn text_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(text.width(width)))
}

#[no_mangle]
extern "C" fn text_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let text = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(text.height(height)))
}

#[no_mangle]
extern "C" fn text_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text.into()))
}

#[no_mangle]
extern "C" fn text_style_set_color(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.color = Some(color);
}
