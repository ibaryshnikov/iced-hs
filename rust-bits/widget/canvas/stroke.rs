use std::ffi::{c_float, c_uchar};

use iced::widget::canvas::{LineCap, LineDash, LineJoin, Stroke, Style};

type SelfPtr = *mut Stroke<'static>;

#[no_mangle]
extern "C" fn canvas_stroke_new(
    style_ptr: *mut Style,
    width: c_float,
    line_cap_raw: c_uchar,
    line_join_raw: c_uchar,
) -> SelfPtr {
    let style = unsafe { *Box::from_raw(style_ptr) };
    let line_cap = match line_cap_raw {
        0 => LineCap::Butt,
        1 => LineCap::Square,
        2 => LineCap::Round,
        other => panic!("Unexpected LineCap value in canvas_stroke_new: {other}"),
    };
    let line_join = match line_join_raw {
        0 => LineJoin::Miter,
        1 => LineJoin::Round,
        2 => LineJoin::Bevel,
        other => panic!("Unexpected LineJoin value in canvas_stroke_new: {other}"),
    };

    let stroke = Stroke {
        style,
        width,
        line_cap,
        line_join,
        line_dash: LineDash::default(),
    };
    Box::into_raw(Box::new(stroke))
}

#[no_mangle]
extern "C" fn canvas_stroke_default() -> SelfPtr {
    Box::into_raw(Box::default())
}

#[no_mangle]
extern "C" fn canvas_stroke_set_line_dash(
    stroke: &mut Stroke,
    line_dash_ptr: *mut LineDash<'static>,
) {
    let line_dash = unsafe { *Box::from_raw(line_dash_ptr) };
    stroke.line_dash = line_dash;
}
