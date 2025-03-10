use std::ffi::{c_char, c_float, c_uchar};

use button::{Status, Style};
use iced::widget::{button, text, Button};
use iced::{Background, Border, Color, Length, Padding};

use super::{ElementPtr, IcedMessage};
use crate::ffi::read_c_string;

type SelfPtr = *mut Button<'static, IcedMessage>;

type StyleCallback = extern "C" fn(style: &mut Style, theme: c_uchar, status: c_uchar);

enum BasicStyle {
    Primary,
    Secondary,
    Success,
    Danger,
    Text,
}

use BasicStyle::*;

impl BasicStyle {
    fn from_raw(value: u8) -> Self {
        match value {
            0 => Primary,
            1 => Secondary,
            2 => Success,
            3 => Danger,
            4 => Text,
            other => panic!("Unexpected value in button BasicStyle: {other}"),
        }
    }
}

#[no_mangle]
extern "C" fn button_new(input: *mut c_char) -> SelfPtr {
    let string = read_c_string(input);
    let button = button(text(string));
    Box::into_raw(Box::new(button))
}

#[no_mangle]
extern "C" fn button_on_press(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    Box::into_raw(Box::new(button.on_press(message)))
}

#[no_mangle]
extern "C" fn button_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let padding = unsafe { *Box::from_raw(padding_ptr) };
    Box::into_raw(Box::new(button.padding(padding)))
}

#[no_mangle]
extern "C" fn button_style_basic(self_ptr: SelfPtr, style_raw: c_uchar) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let style_fn = match BasicStyle::from_raw(style_raw) {
        Primary => button::primary,
        Secondary => button::secondary,
        Success => button::success,
        Danger => button::danger,
        Text => button::text,
    };
    Box::into_raw(Box::new(button.style(style_fn)))
}

fn status_to_raw(status: Status) -> c_uchar {
    match status {
        Status::Active => 0,
        Status::Hovered => 1,
        Status::Pressed => 2,
        Status::Disabled => 3,
    }
}

#[no_mangle]
extern "C" fn button_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(button.style(move |theme, status| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let status_raw = status_to_raw(status);
        let mut style = button::primary(theme, status);
        callback(&mut style, theme_raw, status_raw);
        style
    })))
}

#[no_mangle]
extern "C" fn button_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(button.width(width)))
}

#[no_mangle]
extern "C" fn button_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(button.height(height)))
}

#[no_mangle]
extern "C" fn button_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let button = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(button.into()))
}

#[no_mangle]
extern "C" fn button_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.background = Some(Background::Color(color));
}

#[no_mangle]
extern "C" fn button_style_set_border(
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
extern "C" fn button_style_set_text_color(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.text_color = color;
}
