use std::ffi::{c_char, c_float, c_uchar};

use iced::widget::{text_input, TextInput};
use iced::{Background, Border, Color, Length, Padding};
use text_input::{Status, Style};

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut TextInput<'static, IcedMessage>;

type OnInputFFI = extern "C" fn(input: *mut c_char) -> *const u8;

type StyleCallback = extern "C" fn(style: &mut Style, theme: c_uchar, status: c_uchar);

#[no_mangle]
extern "C" fn text_input_new(placeholder: *mut c_char, value: *mut c_char) -> SelfPtr {
    let placeholder = read_c_string(placeholder);
    let value = read_c_string(value);
    Box::into_raw(Box::new(text_input(&placeholder, &value)))
}

#[no_mangle]
extern "C" fn text_input_on_input(self_ptr: SelfPtr, on_input_ffi: OnInputFFI) -> SelfPtr {
    let text_input = unsafe { *Box::from_raw(self_ptr) };
    let on_input = super::wrap_callback_with_string(on_input_ffi);
    Box::into_raw(Box::new(text_input.on_input(on_input)))
}

#[no_mangle]
extern "C" fn text_input_on_submit(self_ptr: SelfPtr, on_submit_ptr: *const u8) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    let text_input = text_input.on_submit(IcedMessage::ptr(on_submit_ptr));
    Box::into_raw(Box::new(text_input))
}

#[no_mangle]
extern "C" fn text_input_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    let padding = unsafe { *Box::from_raw(padding_ptr) };
    Box::into_raw(Box::new(text_input.padding(padding)))
}

fn status_to_raw(status: Status) -> c_uchar {
    match status {
        Status::Active => 0,
        Status::Hovered => 1,
        Status::Focused => 2,
        Status::Disabled => 3,
    }
}

#[no_mangle]
extern "C" fn text_input_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_input.style(move |theme, status| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let status_raw = status_to_raw(status);
        let mut style = text_input::default(theme, status);
        callback(&mut style, theme_raw, status_raw);
        style
    })))
}

#[no_mangle]
extern "C" fn text_input_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let text_input = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(text_input.width(width)))
}

#[no_mangle]
extern "C" fn text_input_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text_input = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_input.into()))
}

#[no_mangle]
extern "C" fn text_input_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.background = Background::Color(color);
}

#[no_mangle]
extern "C" fn text_input_style_set_border(
    style: &mut Style,
    color_ptr: *mut Color,
    width: c_float,
    radius: c_float,
) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.border = Border {
        color,
        width,
        radius: radius.into(),
    }
}

#[no_mangle]
extern "C" fn text_input_style_set_icon(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.icon = color;
}

#[no_mangle]
extern "C" fn text_input_style_set_placeholder(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.placeholder = color;
}

#[no_mangle]
extern "C" fn text_input_style_set_value(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.value = color;
}

#[no_mangle]
extern "C" fn text_input_style_set_selection(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.selection = color;
}
