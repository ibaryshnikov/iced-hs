use std::ffi::{c_char, c_float, c_uchar, c_uint};

use checkbox::Icon;
use iced::widget::{checkbox, text, Checkbox};
use iced::{theme, Font, Length};
use text::{LineHeight, Shaping};

use super::{read_c_bool, read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut Checkbox<'static, IcedMessage>;
type IconPtr = *mut Icon<Font>;

type OnToggleFFI = unsafe extern "C" fn(input: c_uchar) -> *const u8;

#[no_mangle]
extern "C" fn checkbox_new(input: *mut c_char, is_checked_raw: c_uchar) -> SelfPtr {
    let label = read_c_string(input);
    let is_checked = read_c_bool(is_checked_raw);
    Box::into_raw(Box::new(checkbox(label, is_checked)))
}

#[no_mangle]
extern "C" fn checkbox_on_toggle(self_ptr: SelfPtr, on_toggle_ffi: OnToggleFFI) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let on_toggle = super::wrap_callback_with_bool(on_toggle_ffi);
    Box::into_raw(Box::new(checkbox.on_toggle(on_toggle)))
}

#[no_mangle]
extern "C" fn checkbox_icon(self_ptr: SelfPtr, icon_ptr: IconPtr) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let icon = unsafe { *Box::from_raw(icon_ptr) };
    Box::into_raw(Box::new(checkbox.icon(icon)))
}

#[no_mangle]
extern "C" fn checkbox_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(checkbox.size(size)))
}

#[no_mangle]
extern "C" fn checkbox_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(checkbox.spacing(pixels)))
}

#[no_mangle]
extern "C" fn checkbox_text_line_height(
    self_ptr: SelfPtr,
    line_height_ptr: *mut LineHeight,
) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let line_height = unsafe { *Box::from_raw(line_height_ptr) };
    Box::into_raw(Box::new(checkbox.text_line_height(line_height)))
}

#[no_mangle]
extern "C" fn checkbox_text_size(self_ptr: SelfPtr, text_size: c_float) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(checkbox.text_size(text_size)))
}

#[no_mangle]
extern "C" fn checkbox_style(self_ptr: SelfPtr, style_ptr: *mut theme::Checkbox) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let style = unsafe { *Box::from_raw(style_ptr) };
    Box::into_raw(Box::new(checkbox.style(style)))
}

#[no_mangle]
extern "C" fn checkbox_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(checkbox.width(width)))
}

#[no_mangle]
extern "C" fn checkbox_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let checkbox = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(checkbox.into()))
}

#[no_mangle]
extern "C" fn checkbox_primary() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Primary))
}

#[no_mangle]
extern "C" fn checkbox_secondary() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Secondary))
}

#[no_mangle]
extern "C" fn checkbox_success() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Success))
}

#[no_mangle]
extern "C" fn checkbox_danger() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Danger))
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
