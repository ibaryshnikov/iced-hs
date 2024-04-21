use std::ffi::c_float;

use iced::widget::text::LineHeight;

#[no_mangle]
extern "C" fn line_height_relative(value: c_float) -> *mut LineHeight {
    let line_height = LineHeight::Relative(value);
    Box::into_raw(Box::new(line_height))
}

#[no_mangle]
extern "C" fn line_height_absolute(value: c_float) -> *mut LineHeight {
    let line_height = LineHeight::Absolute(value.into());
    Box::into_raw(Box::new(line_height))
}
