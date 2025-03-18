use std::ffi::{c_char, c_float, c_uchar, c_uint};

use iced::widget::{radio, Radio};
use iced::{Background, Color, Length};
use radio::{Status, Style};

use crate::ffi::{from_raw, into_element, into_raw, read_c_string};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Radio<'static, IcedMessage>;

type OnSelectFFI = extern "C" fn(selected: c_uint) -> *const u8;

type StyleCallback =
    extern "C" fn(style: &mut Style, theme: c_uchar, status: c_uchar, is_selected: c_uchar);

#[no_mangle]
extern "C" fn radio_new(
    label_ptr: *mut c_char, // CString
    value: c_uint,          // Case number starting with 1
    selected_raw: c_uint,   // Case number, 0 converted to None
    on_select_ffi: OnSelectFFI,
) -> SelfPtr {
    let label = read_c_string(label_ptr);
    let selected = match selected_raw {
        0 => None,
        a => Some(a),
    };
    let on_select = move |input| {
        let message_ptr = on_select_ffi(input);
        IcedMessage::ptr(message_ptr)
    };
    let radio = radio(label, value, selected, on_select);
    into_raw(radio)
}

fn status_to_raw(status: Status) -> (c_uchar, bool) {
    match status {
        Status::Active { is_selected } => (0, is_selected),
        Status::Hovered { is_selected } => (1, is_selected),
    }
}

#[no_mangle]
extern "C" fn radio_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let radio = from_raw(self_ptr);
    let radio = radio.style(move |theme, status| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let (status_raw, is_selected) = status_to_raw(status);
        let mut style = radio::default(theme, status);
        callback(&mut style, theme_raw, status_raw, is_selected.into());
        style
    });
    into_raw(radio)
}

#[no_mangle]
extern "C" fn radio_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let radio = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(radio.width(width))
}

#[no_mangle]
extern "C" fn radio_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}

#[no_mangle]
extern "C" fn radio_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.background = Background::Color(color);
}

#[no_mangle]
extern "C" fn radio_style_set_dot_color(style: &mut Style, color_ptr: *mut Color) {
    style.dot_color = from_raw(color_ptr);
}

#[no_mangle]
extern "C" fn radio_style_set_border_width(style: &mut Style, width: c_float) {
    style.border_width = width;
}

#[no_mangle]
extern "C" fn radio_style_set_border_color(style: &mut Style, color_ptr: *mut Color) {
    style.border_color = from_raw(color_ptr);
}

#[no_mangle]
extern "C" fn radio_style_set_text_color(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.text_color = Some(color);
}
