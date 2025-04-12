use std::ffi::c_float;
use std::sync::Arc;

use canvas::path::Builder;
use iced::widget::canvas::{self, Path};
use iced::Point;

use crate::free_haskell_fun_ptr;

#[repr(transparent)]
struct PathCallback {
    inner: extern "C" fn(builder: &mut Builder),
}

impl Drop for PathCallback {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) }
    }
}

#[no_mangle]
extern "C" fn path_new(callback: PathCallback) -> *mut Path {
    let callback = Arc::new(callback);
    let path = Path::new(move |builder| (callback.inner)(builder));
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
