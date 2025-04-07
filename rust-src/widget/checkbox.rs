use std::ffi::{c_char, c_float, c_uchar, c_uint};

use checkbox::{Icon, Status, Style};
use iced::widget::{checkbox, text, Checkbox};
use iced::{Background, Border, Color, Font, Length};
use text::{LineHeight, Shaping};

use super::read_shaping;
use crate::ffi::{from_raw, into_element, into_raw, read_c_bool, read_c_string};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Checkbox<'static, IcedMessage>;
type IconPtr = *mut Icon<Font>;

type OnToggleFFI = super::CallbackForCBool;

type StyleCallback =
    extern "C" fn(style: &mut Style, theme: c_uchar, status: c_uchar, is_checked: c_uchar);

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
            other => panic!("Unexpected value in checkbox BasicStyle: {other}"),
        }
    }
}

#[no_mangle]
extern "C" fn checkbox_new(input: *mut c_char, is_checked_raw: c_uchar) -> SelfPtr {
    let label = read_c_string(input);
    let is_checked = read_c_bool(is_checked_raw);
    into_raw(checkbox(label, is_checked))
}

#[no_mangle]
extern "C" fn checkbox_on_toggle(self_ptr: SelfPtr, on_toggle_ffi: OnToggleFFI) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    let on_toggle = super::wrap_callback_with_bool(on_toggle_ffi);
    into_raw(checkbox.on_toggle(on_toggle))
}

#[no_mangle]
extern "C" fn checkbox_icon(self_ptr: SelfPtr, icon_ptr: IconPtr) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    let icon = from_raw(icon_ptr);
    into_raw(checkbox.icon(icon))
}

#[no_mangle]
extern "C" fn checkbox_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    into_raw(checkbox.size(size))
}

#[no_mangle]
extern "C" fn checkbox_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    into_raw(checkbox.spacing(pixels))
}

#[no_mangle]
extern "C" fn checkbox_style_basic(self_ptr: SelfPtr, style_raw: c_uchar) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    let style_fn = match BasicStyle::from_raw(style_raw) {
        Primary => checkbox::primary,
        Secondary => checkbox::secondary,
        Success => checkbox::success,
        Danger => checkbox::danger,
    };
    into_raw(checkbox.style(style_fn))
}

fn status_to_raw(status: Status) -> (c_uchar, bool) {
    match status {
        Status::Active { is_checked } => (0, is_checked),
        Status::Hovered { is_checked } => (1, is_checked),
        Status::Disabled { is_checked } => (2, is_checked),
    }
}

#[no_mangle]
extern "C" fn checkbox_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    into_raw(checkbox.style(move |theme, status| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let (status_raw, is_checked) = status_to_raw(status);
        let mut style = checkbox::primary(theme, status);
        callback(&mut style, theme_raw, status_raw, is_checked.into());
        style
    }))
}

#[no_mangle]
extern "C" fn checkbox_text_line_height(
    self_ptr: SelfPtr,
    line_height_ptr: *mut LineHeight,
) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    let line_height = from_raw(line_height_ptr);
    into_raw(checkbox.text_line_height(line_height))
}

#[no_mangle]
extern "C" fn checkbox_text_shaping(self_ptr: SelfPtr, shaping_raw: c_uchar) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    let shaping = read_shaping(shaping_raw);
    into_raw(checkbox.text_shaping(shaping))
}

#[no_mangle]
extern "C" fn checkbox_text_size(self_ptr: SelfPtr, text_size: c_float) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    into_raw(checkbox.text_size(text_size))
}

#[no_mangle]
extern "C" fn checkbox_width(self_ptr: SelfPtr, width_ptr: *mut Length) -> SelfPtr {
    let checkbox = from_raw(self_ptr);
    let width = from_raw(width_ptr);
    into_raw(checkbox.width(width))
}

#[no_mangle]
extern "C" fn checkbox_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}

#[no_mangle]
extern "C" fn checkbox_icon_new(code_point_raw: c_uint) -> IconPtr {
    let code_point = char::from_u32(code_point_raw).unwrap_or_default();
    let icon = Icon {
        font: Font::with_name("icons"),
        code_point,
        size: None,
        line_height: LineHeight::Relative(1.0),
        shaping: Shaping::Basic,
    };
    Box::into_raw(Box::new(icon))
}

#[no_mangle]
extern "C" fn checkbox_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.background = Background::Color(color);
}

#[no_mangle]
extern "C" fn checkbox_style_set_icon_color(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.icon_color = color;
}

#[no_mangle]
extern "C" fn checkbox_style_set_border(
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

#[no_mangle]
extern "C" fn checkbox_style_set_text_color(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.text_color = Some(color);
}
