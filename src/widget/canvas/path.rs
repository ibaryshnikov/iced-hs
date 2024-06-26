use std::ffi::c_float;

use iced::widget::canvas::{self, Path};
use iced::Point;

use canvas::path::Builder;

type PathCallback = extern "C" fn(builder: &mut Builder);

#[no_mangle]
extern "C" fn path_new(callback: PathCallback) -> *mut Path {
    #[allow(clippy::redundant_closure)] // false positive
    let path = Path::new(|builder| callback(builder));
    Box::into_raw(Box::new(path))
}

#[no_mangle]
extern "C" fn path_free(pointer: *mut Path) {
    let _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
extern "C" fn path_line(
    from_x: c_float,
    from_y: c_float,
    to_x: c_float,
    to_y: c_float,
) -> *mut Path {
    let path = Path::line(Point::new(from_x, from_y), Point::new(to_x, to_y));
    Box::into_raw(Box::new(path))
}

#[no_mangle]
extern "C" fn path_circle(x: c_float, y: c_float, radius: c_float) -> *mut Path {
    let path = Path::circle(Point { x, y }, radius);
    Box::into_raw(Box::new(path))
}
