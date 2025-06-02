use std::ffi::{c_char, c_float, c_uchar};
use std::sync::Arc;

use iced::widget::{pick_list, PickList};
use iced::{Background, Border, Color, Length, Padding};
use pick_list::{Status, Style};

use crate::ffi::{from_raw, into_element, into_raw, read_c_string};
use crate::{free_haskell_fun_ptr, ElementPtr, IcedMessage};

type SelfPtr = *mut PickList<'static, String, Vec<String>, String, IcedMessage>;

type OnSelectFFI = super::CallbackForCString;

#[repr(transparent)]
struct StyleCallback {
    inner: extern "C" fn(style: &mut Style, theme: c_uchar, status: c_uchar),
}

impl Drop for StyleCallback {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

#[no_mangle]
extern "C" fn pick_list_new(
    len: usize,
    options_ptr: *const *mut c_char, // array of CString
    selected_ptr: *mut c_char,       // CString
    on_select_ffi: OnSelectFFI,
) -> SelfPtr {
    let selected = super::read_c_string_to_option(selected_ptr);
    let options = super::read_array_of_c_strings(len, options_ptr);
    let on_select = super::wrap_callback_with_string(on_select_ffi);
    let pick_list = pick_list(options, selected, on_select);
    into_raw(pick_list)
}

#[no_mangle]
extern "C" fn pick_list_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let pick_list = from_raw(self_ptr);
    let padding = from_raw(padding_ptr);
    into_raw(pick_list.padding(padding))
}

#[no_mangle]
extern "C" fn pick_list_placeholder(self_ptr: SelfPtr, placeholder_ptr: *mut c_char) -> SelfPtr {
    let pick_list = from_raw(self_ptr);
    let placeholder = read_c_string(placeholder_ptr);
    into_raw(pick_list.placeholder(placeholder))
}

fn status_to_raw(status: Status) -> c_uchar {
    match status {
        Status::Active => 0,
        Status::Hovered => 1,
        Status::Opened => 2,
    }
}

#[no_mangle]
extern "C" fn pick_list_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let pick_list = from_raw(self_ptr);
    let callback = Arc::new(callback);
    let pick_list = pick_list.style(move |theme, status| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let status_raw = status_to_raw(status);
        let mut style = pick_list::default(theme, status);
        (callback.inner)(&mut style, theme_raw, status_raw);
        style
    });
    into_raw(pick_list)
}

#[no_mangle]
extern "C" fn pick_list_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let pick_list = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(pick_list.width(width))
}

#[no_mangle]
extern "C" fn pick_list_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}

#[no_mangle]
extern "C" fn pick_list_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.background = Background::Color(color);
}

#[no_mangle]
extern "C" fn pick_list_style_set_border(
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
extern "C" fn pick_list_style_set_text_color(style: &mut Style, color_ptr: *mut Color) {
    style.text_color = from_raw(color_ptr);
}

#[no_mangle]
extern "C" fn pick_list_style_set_placeholder_color(style: &mut Style, color_ptr: *mut Color) {
    style.placeholder_color = from_raw(color_ptr);
}

#[no_mangle]
extern "C" fn pick_list_style_set_handle_color(style: &mut Style, color_ptr: *mut Color) {
    style.handle_color = from_raw(color_ptr);
}
