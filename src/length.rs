use std::ffi::{c_float, c_ushort};

use iced::Length;

#[no_mangle]
pub extern "C" fn length_fill() -> *mut Length {
    Box::into_raw(Box::new(Length::Fill))
}

#[no_mangle]
pub extern "C" fn length_fill_portion(portion: c_ushort) -> *mut Length {
    Box::into_raw(Box::new(Length::FillPortion(portion)))
}

#[no_mangle]
pub extern "C" fn length_shrink() -> *mut Length {
    Box::into_raw(Box::new(Length::Shrink))
}

#[no_mangle]
pub extern "C" fn length_fixed(value: c_float) -> *mut Length {
    Box::into_raw(Box::new(Length::Fixed(value)))
}
