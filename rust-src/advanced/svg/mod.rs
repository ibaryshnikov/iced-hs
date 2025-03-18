use std::ffi::c_float;

use iced::advanced::svg::{Handle, Svg};
use iced::Color;

use crate::ffi::{from_raw, into_raw};

mod handle;

type SelfPtr = *mut Svg;

#[no_mangle]
extern "C" fn advanced_svg_new(handle_ptr: *mut Handle) -> SelfPtr {
    let handle = from_raw(handle_ptr);
    into_raw(Svg::new(handle))
}

#[no_mangle]
extern "C" fn advanced_svg_color(self_ptr: SelfPtr, color_ptr: *mut Color) -> SelfPtr {
    let svg = from_raw(self_ptr);
    let color = from_raw(color_ptr);
    into_raw(svg.color(color))
}

#[no_mangle]
extern "C" fn advanced_svg_opacity(self_ptr: SelfPtr, opacity: c_float) -> SelfPtr {
    let svg = from_raw(self_ptr);
    into_raw(svg.opacity(opacity))
}

#[no_mangle]
extern "C" fn advanced_svg_rotation(self_ptr: SelfPtr, radians: c_float) -> SelfPtr {
    let svg = from_raw(self_ptr);
    into_raw(svg.rotation(radians))
}
