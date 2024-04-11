use std::ffi::{c_char, c_float, c_uchar};

use iced::widget::{toggler, Toggler};
use iced::Length;

use super::{read_c_bool, read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut Toggler<'static, IcedMessage>;

type OnToggleFFI = unsafe extern "C" fn(input: c_uchar) -> *const u8;

#[no_mangle]
pub extern "C" fn toggler_new(
    label_ptr: *mut c_char,
    is_toggled_raw: c_uchar,
    on_toggle_ffi: OnToggleFFI,
) -> SelfPtr {
    let label = read_c_string(label_ptr);
    let is_toggled = read_c_bool(is_toggled_raw);
    let on_toggle = super::wrap_callback_with_bool(on_toggle_ffi);
    Box::into_raw(Box::new(toggler(label, is_toggled, on_toggle)))
}

#[no_mangle]
pub extern "C" fn toggler_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let toggler = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(toggler.size(size)))
}

#[no_mangle]
pub extern "C" fn toggler_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let toggler = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(toggler.spacing(pixels)))
}

#[no_mangle]
pub extern "C" fn toggler_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let toggler = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(toggler.width(width)))
}

#[no_mangle]
pub extern "C" fn toggler_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let toggler = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(toggler.into()))
}
