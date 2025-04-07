use std::ffi::{c_char, c_float, c_uchar};

use iced::widget::{toggler, Toggler};
use iced::Length;

use crate::ffi::{from_raw, into_element, into_raw, read_c_bool, read_c_string};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Toggler<'static, IcedMessage>;

type OnToggleFFI = super::CallbackForCBool;

#[no_mangle]
extern "C" fn toggler_new(
    label_ptr: *mut c_char,
    is_toggled_raw: c_uchar,
    on_toggle_ffi: OnToggleFFI,
) -> SelfPtr {
    let label = read_c_string(label_ptr);
    let is_toggled = read_c_bool(is_toggled_raw);
    let on_toggle = super::wrap_callback_with_bool(on_toggle_ffi);
    let widget = toggler(is_toggled).label(label).on_toggle(on_toggle);
    into_raw(widget)
}

#[no_mangle]
extern "C" fn toggler_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let toggler = from_raw(self_ptr);
    into_raw(toggler.size(size))
}

#[no_mangle]
extern "C" fn toggler_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let toggler = from_raw(self_ptr);
    into_raw(toggler.spacing(pixels))
}

#[no_mangle]
extern "C" fn toggler_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let toggler = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(toggler.width(width))
}

#[no_mangle]
extern "C" fn toggler_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
