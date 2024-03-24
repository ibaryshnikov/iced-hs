use std::ffi::{c_char, c_int};

use iced::widget;

use crate::{HaskellMessage, IcedMessage};

use super::{c_string_to_rust, ElementPtr};

type ToggleCallback = unsafe extern "C" fn(input: c_int) -> *const u8;

#[no_mangle]
pub extern "C" fn new_checkbox(
    input: *mut c_char,
    value: c_int,
    maybe_on_toggle: Option<ToggleCallback>,
) -> ElementPtr {
    let label = c_string_to_rust(input);
    let is_checked = match value {
        0 => false,
        1 => true,
        _ => panic!("Non boolean value passed to checkbox"),
    };
    let mut checkbox = widget::checkbox(label, is_checked);
    if let Some(on_toggle) = maybe_on_toggle {
        checkbox = checkbox.on_toggle(move |new_value| {
            let message_ptr = unsafe { on_toggle(new_value.into()) };
            IcedMessage::Ptr(HaskellMessage { ptr: message_ptr })
        });
    }
    Box::into_raw(Box::new(checkbox.into()))
}
