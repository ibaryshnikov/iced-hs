use std::ffi::{c_float, c_uchar};

use iced::widget::{progress_bar, ProgressBar};
use iced::{Background, Border, Color, Length};
use progress_bar::Style;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::ElementPtr;

type SelfPtr = *mut ProgressBar<'static>;

type StyleCallback = extern "C" fn(style: &mut Style, theme: c_uchar);

enum BasicStyle {
    Primary,
    Secondary,
    Success,
    Danger,
}

use BasicStyle::*;

impl BasicStyle {
    fn from_raw(value: u8) -> Self {
        match value {
            0 => Primary,
            1 => Secondary,
            2 => Success,
            3 => Danger,
            other => panic!("Unexpected value in progress_bar BasicStyle: {other}"),
        }
    }
}

#[no_mangle]
pub extern "C" fn progress_bar_new(
    range_from: c_float,
    range_to: c_float,
    value: c_float,
) -> SelfPtr {
    let range = range_from..=range_to;
    into_raw(progress_bar(range, value))
}

#[no_mangle]
extern "C" fn progress_bar_style_basic(self_ptr: SelfPtr, style_raw: c_uchar) -> SelfPtr {
    let progress_bar = from_raw(self_ptr);
    let style_fn = match BasicStyle::from_raw(style_raw) {
        Primary => progress_bar::primary,
        Secondary => progress_bar::secondary,
        Success => progress_bar::success,
        Danger => progress_bar::danger,
    };
    into_raw(progress_bar.style(style_fn))
}

#[no_mangle]
extern "C" fn progress_bar_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let progress_bar = from_raw(self_ptr);
    let progress_bar = progress_bar.style(move |theme| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let mut style = progress_bar::primary(theme);
        callback(&mut style, theme_raw);
        style
    });
    into_raw(progress_bar)
}

#[no_mangle]
pub extern "C" fn progress_bar_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let progress_bar = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(progress_bar.width(width))
}

#[no_mangle]
pub extern "C" fn progress_bar_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let progress_bar = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(progress_bar.height(height))
}

#[no_mangle]
pub extern "C" fn progress_bar_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}

#[no_mangle]
extern "C" fn progress_bar_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.background = Background::Color(color);
}

#[no_mangle]
extern "C" fn progress_bar_style_set_bar(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.bar = Background::Color(color);
}

#[no_mangle]
extern "C" fn progress_bar_style_set_border(
    style: &mut Style,
    color_ptr: *mut Color,
    width: c_float,
    radius: c_float,
) {
    style.border = Border {
        color: from_raw(color_ptr),
        width,
        radius: radius.into(),
    }
}
