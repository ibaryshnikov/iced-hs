use std::ffi::c_float;

use iced::Padding;

#[no_mangle]
extern "C" fn padding_new(
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> *mut Padding {
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(padding))
}
