use std::ffi::{c_char, c_float, c_uchar};

use iced::widget::canvas::Text;
use iced::widget::text::LineHeight;
use iced::{Alignment, Color, Font, Point};

use crate::widget::{read_c_string, read_shaping};

#[no_mangle]
extern "C" fn canvas_text_new(
    content_ptr: *mut c_char,
    position_x: c_float,
    position_y: c_float,
    color_ptr: *mut Color,
    size: c_float,
    line_height_ptr: *mut LineHeight,
    horizontal_ptr: *mut Alignment,
    vertical_ptr: *mut Alignment,
    shaping_raw: c_uchar,
) -> *mut Text {
    let content = read_c_string(content_ptr);
    let position = Point::new(position_x, position_y);
    let color = unsafe { *Box::from_raw(color_ptr) };
    let line_height = unsafe { *Box::from_raw(line_height_ptr) };
    let font = Font::default();
    let horizontal = unsafe { *Box::from_raw(horizontal_ptr) };
    let vertical = unsafe { *Box::from_raw(vertical_ptr) };
    let shaping = read_shaping(shaping_raw);
    let text = Text {
        content,
        position,
        color,
        size: size.into(),
        line_height,
        font,
        horizontal_alignment: horizontal.into(),
        vertical_alignment: vertical.into(),
        shaping,
    };
    Box::into_raw(Box::new(text))
}
