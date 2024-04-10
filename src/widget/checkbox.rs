use std::ffi::{c_char, c_uchar, c_uint};

use checkbox::Icon;
use iced::widget::{checkbox, text, Checkbox};
use iced::{theme, Font};

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut Checkbox<'static, IcedMessage>;
type IconPtr = *mut Icon<Font>;

type ToggleCallback = unsafe extern "C" fn(input: c_uchar) -> *const u8;

#[no_mangle]
pub extern "C" fn checkbox_new(input: *mut c_char, value: c_uchar) -> SelfPtr {
    let label = read_c_string(input);
    let is_checked = match value {
        0 => false,
        1 => true,
        _ => panic!("Non boolean value passed to checkbox"),
    };
    Box::into_raw(Box::new(checkbox(label, is_checked)))
}

#[no_mangle]
pub extern "C" fn checkbox_on_toggle(self_ptr: SelfPtr, on_toggle: ToggleCallback) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let checkbox = checkbox.on_toggle(move |new_value| {
        let message_ptr = unsafe { on_toggle(new_value.into()) };
        IcedMessage::ptr(message_ptr)
    });
    Box::into_raw(Box::new(checkbox))
}

#[no_mangle]
pub extern "C" fn checkbox_icon(self_ptr: SelfPtr, icon_ptr: IconPtr) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let icon = unsafe { *Box::from_raw(icon_ptr) };
    Box::into_raw(Box::new(checkbox.icon(icon)))
}

#[no_mangle]
pub extern "C" fn checkbox_style(self_ptr: SelfPtr, style_ptr: *mut theme::Checkbox) -> SelfPtr {
    let checkbox = unsafe { Box::from_raw(self_ptr) };
    let style = unsafe { *Box::from_raw(style_ptr) };
    Box::into_raw(Box::new(checkbox.style(style)))
}

#[no_mangle]
pub extern "C" fn checkbox_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let checkbox = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(checkbox.into()))
}

#[no_mangle]
pub extern "C" fn checkbox_primary() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Primary))
}

#[no_mangle]
pub extern "C" fn checkbox_secondary() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Secondary))
}

#[no_mangle]
pub extern "C" fn checkbox_success() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Success))
}

#[no_mangle]
pub extern "C" fn checkbox_danger() -> *mut theme::Checkbox {
    Box::into_raw(Box::new(theme::Checkbox::Danger))
}

#[no_mangle]
pub extern "C" fn checkbox_icon_new(code_point_raw: c_uint) -> IconPtr {
    let code_point = char::from_u32(code_point_raw).unwrap_or_default();
    let icon = Icon {
        font: Font::with_name("icons"),
        code_point,
        size: None,
        line_height: text::LineHeight::Relative(1.0),
        shaping: text::Shaping::Basic,
    };
    Box::into_raw(Box::new(icon))
}
