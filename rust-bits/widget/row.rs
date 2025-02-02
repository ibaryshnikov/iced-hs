use std::ffi::c_float;

use iced::widget::Row;
use iced::{Alignment, Length, Padding};

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Row<'static, IcedMessage>;

#[no_mangle]
extern "C" fn row_new() -> SelfPtr {
    Box::into_raw(Box::new(Row::new()))
}

#[no_mangle]
extern "C" fn row_align_y(self_ptr: SelfPtr, alignment: *mut Alignment) -> SelfPtr {
    let row = unsafe { Box::from_raw(self_ptr) };
    let alignment = unsafe { *Box::from_raw(alignment) };
    Box::into_raw(Box::new(row.align_y(alignment)))
}

#[no_mangle]
extern "C" fn row_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let row = unsafe { Box::from_raw(self_ptr) };
    let padding = unsafe { *Box::from_raw(padding_ptr) };
    Box::into_raw(Box::new(row.padding(padding)))
}

#[no_mangle]
extern "C" fn row_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let row = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(row.spacing(pixels)))
}

#[no_mangle]
extern "C" fn row_with_children(len: usize, ptr: *const ElementPtr) -> SelfPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut row = Row::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        row = row.push(*boxed);
    }
    Box::into_raw(Box::new(row))
}

#[no_mangle]
extern "C" fn row_extend(
    self_ptr: SelfPtr,
    len: usize,
    elements_ptr: *const ElementPtr,
) -> SelfPtr {
    let mut row = unsafe { *Box::from_raw(self_ptr) };
    let slice = unsafe { std::slice::from_raw_parts(elements_ptr, len) };
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        row = row.push(*boxed);
    }
    Box::into_raw(Box::new(row))
}

#[no_mangle]
extern "C" fn row_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let row = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(row.width(width)))
}

#[no_mangle]
extern "C" fn row_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let row = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(row.height(height)))
}

#[no_mangle]
extern "C" fn row_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let row = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(row.into()))
}
