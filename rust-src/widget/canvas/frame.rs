use std::ffi::c_float;

use iced::widget::canvas::Text;
use iced::widget::canvas::{Fill, Frame, Path, Stroke};
use iced::{Point, Size};

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
    let fill = unsafe { *Box::from_raw(fill_ptr) };
    frame.fill_rectangle(top_left, size, fill);
}

#[no_mangle]
extern "C" fn canvas_frame_fill_text(frame: &mut Frame, text_ptr: *mut Text) {
    let text = unsafe { *Box::from_raw(text_ptr) };
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
    let path = unsafe { Box::from_raw(path_ptr) };
    let stroke = unsafe { *Box::from_raw(stroke_ptr) };
    frame.stroke(&path, stroke);
}
