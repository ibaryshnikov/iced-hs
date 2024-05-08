use std::ffi::c_float;

use iced::Color;

#[no_mangle]
extern "C" fn color_new(r: c_float, g: c_float, b: c_float, a: c_float) -> *mut Color {
    let color = Color::new(r, g, b, a);
    Box::into_raw(Box::new(color))
}
