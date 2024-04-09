use std::ffi::{c_char, c_uint};

use iced::widget::{radio, Radio};
use iced::Length;

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut Radio<IcedMessage>;

type OnSelect = unsafe extern "C" fn(selected: c_uint) -> *const u8;

#[no_mangle]
pub extern "C" fn radio_new(
    label_ptr: *mut c_char, // CString
    value: c_uint,          // Case number starting with 1
    selected_raw: c_uint,   // Case number, 0 converted to None
    on_select_ffi: OnSelect,
) -> SelfPtr {
    let label = read_c_string(label_ptr);
    let selected = match selected_raw {
        0 => None,
        a => Some(a),
    };
    let on_select = move |input| {
        let message_ptr = unsafe { on_select_ffi(input) };
        IcedMessage::ptr(message_ptr)
    };
    let radio = radio(label, value, selected, on_select);
    Box::into_raw(Box::new(radio))
}

#[no_mangle]
pub extern "C" fn radio_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let radio = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(radio.width(width)))
}

#[no_mangle]
pub extern "C" fn radio_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let radio = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(radio.into()))
}
