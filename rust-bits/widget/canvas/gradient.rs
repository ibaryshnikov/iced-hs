use std::ffi::c_float;

use canvas::gradient::Linear;
use canvas::Gradient;
use iced::gradient::ColorStop;
use iced::widget::canvas;
use iced::{Color, Point};

#[no_mangle]
extern "C" fn canvas_gradient_from_linear(linear_ptr: *mut Linear) -> *mut Gradient {
    let linear = unsafe { *Box::from_raw(linear_ptr) };
    Box::into_raw(Box::new(Gradient::Linear(linear)))
}

#[no_mangle]
extern "C" fn canvas_gradient_linear_new(
    start_x: c_float,
    start_y: c_float,
    end_x: c_float,
    end_y: c_float,
) -> *mut Linear {
    let start = Point::new(start_x, start_y);
    let end = Point::new(end_x, end_y);
    Box::into_raw(Box::new(Linear::new(start, end)))
}

const STOPS_LEN: usize = 8;

#[no_mangle]
extern "C" fn canvas_gradient_linear_new_with_stops(
    start_x: c_float,
    start_y: c_float,
    end_x: c_float,
    end_y: c_float,
    stops_ptr: *const Option<ColorStop>,
) -> *mut Linear {
    let start = Point::new(start_x, start_y);
    let end = Point::new(end_x, end_y);
    let mut stops: [Option<ColorStop>; 8] = [None; 8];
    let slice = unsafe { std::slice::from_raw_parts(stops_ptr, STOPS_LEN) };
    for i in 0..STOPS_LEN {
        if let Some(color_stop) = slice[i] {
            stops[i] = Some(color_stop);
        }
    }
    let linear = Linear { start, end, stops };
    Box::into_raw(Box::new(linear))
}

#[no_mangle]
extern "C" fn canvas_gradient_linear_add_stop(
    linear: &mut Linear,
    offset: c_float,
    r: c_float,
    g: c_float,
    b: c_float,
    a: c_float,
) {
    let color = Color::new(r, g, b, a);
    linear.add_stop(offset, color);
}
