use std::ffi::c_float;

use iced::advanced::svg::Svg;
use iced::widget::canvas::{Fill, Frame, Image, Path, Stroke, Text};
use iced::{Point, Rectangle, Size};

use crate::ffi::from_raw;

#[no_mangle]
extern "C" fn canvas_frame_draw_image(
    frame: &mut Frame,
    x: c_float,
    y: c_float,
    width: c_float,
    height: c_float,
    image_ptr: *mut Image,
) {
    let bounds = Rectangle {
        x,
        y,
        width,
        height,
    };
    let image = from_raw(image_ptr);
    frame.draw_image(bounds, image);
}

#[no_mangle]
extern "C" fn canvas_frame_draw_svg(
    frame: &mut Frame,
    x: c_float,
    y: c_float,
    width: c_float,
    height: c_float,
    svg_ptr: *mut Svg,
) {
    let bounds = Rectangle {
        x,
        y,
        width,
        height,
    };
    let svg = from_raw(svg_ptr);
    frame.draw_svg(bounds, svg);
}

#[no_mangle]
extern "C" fn canvas_frame_fill(frame: &mut Frame, path_ptr: *mut Path, fill_ptr: *mut Fill) {
    let path = unsafe { Box::from_raw(path_ptr) };
    let fill = unsafe { *Box::from_raw(fill_ptr) };
    frame.fill(&path, fill);
}

#[no_mangle]
extern "C" fn canvas_frame_fill_rectangle(
    frame: &mut Frame,
    top_left_x: c_float,
    top_left_y: c_float,
    size_width: c_float,
    size_height: c_float,
    fill_ptr: *mut Fill,
) {
    let top_left = Point::new(top_left_x, top_left_y);
    let size = Size::new(size_width, size_height);
    let fill = from_raw(fill_ptr);
    frame.fill_rectangle(top_left, size, fill);
}

#[no_mangle]
extern "C" fn canvas_frame_fill_text(frame: &mut Frame, text_ptr: *mut Text) {
    let text = from_raw(text_ptr);
    frame.fill_text(text);
}

#[no_mangle]
extern "C" fn canvas_frame_pop_transform(frame: &mut Frame) {
    frame.pop_transform();
}

#[no_mangle]
extern "C" fn canvas_frame_push_transform(frame: &mut Frame) {
    frame.push_transform();
}

#[no_mangle]
extern "C" fn canvas_frame_rotate(frame: &mut Frame, angle: c_float) {
    frame.rotate(angle);
}

#[no_mangle]
extern "C" fn canvas_frame_scale(frame: &mut Frame, value: c_float) {
    frame.scale(value);
}

#[no_mangle]
extern "C" fn canvas_frame_stroke(frame: &mut Frame, path_ptr: *mut Path, stroke_ptr: *mut Stroke) {
    let path = from_raw(path_ptr);
    let stroke = from_raw(stroke_ptr);
    frame.stroke(&path, stroke);
}
