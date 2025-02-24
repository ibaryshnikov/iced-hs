use std::ffi::c_float;

use iced::widget::canvas::{Gradient, Style};
use iced::Color;

#[no_mangle]
extern "C" fn canvas_style_solid(r: c_float, g: c_float, b: c_float, a: c_float) -> *mut Style {
    let color = Color::new(r, g, b, a);
    Box::into_raw(Box::new(Style::Solid(color)))
}

#[no_mangle]
extern "C" fn canvas_style_gradient(gradient_ptr: *mut Gradient) -> *mut Style {
    let gradient = unsafe { *Box::from_raw(gradient_ptr) };
    Box::into_raw(Box::new(Style::Gradient(gradient)))
}
