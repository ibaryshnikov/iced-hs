use std::ffi::c_float;

use iced::widget::Row;
use iced::{Alignment, Padding};

use crate::IcedMessage;

use super::ElementPtr;

type RowPtr = *mut Row<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn new_row() -> RowPtr {
    Box::into_raw(Box::new(Row::new()))
}

#[no_mangle]
pub extern "C" fn row_align_items(pointer: RowPtr, alignment: *mut Alignment) -> RowPtr {
    let row = unsafe { Box::from_raw(pointer) };
    let alignment = unsafe { *Box::from_raw(alignment) };
    Box::into_raw(Box::new(row.align_items(alignment)))
}

#[no_mangle]
pub extern "C" fn row_padding(
    pointer: RowPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> RowPtr {
    let row = unsafe { Box::from_raw(pointer) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(row.padding(padding)))
}

#[no_mangle]
pub extern "C" fn row_spacing(pointer: RowPtr, pixels: c_float) -> RowPtr {
    let row = unsafe { Box::from_raw(pointer) };
    Box::into_raw(Box::new(row.spacing(pixels)))
}

#[no_mangle]
pub extern "C" fn row_with_children(len: libc::size_t, ptr: *const ElementPtr) -> RowPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut row = Row::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        row = row.push(*boxed);
    }
    Box::into_raw(Box::new(row))
}

#[no_mangle]
pub extern "C" fn row_extend(
    pointer: RowPtr,
    len: libc::size_t,
    elements_ptr: *const ElementPtr,
) -> RowPtr {
    let mut row = unsafe { *Box::from_raw(pointer) };
    let slice = unsafe { std::slice::from_raw_parts(elements_ptr, len) };
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        row = row.push(*boxed);
    }
    Box::into_raw(Box::new(row))
}

#[no_mangle]
pub extern "C" fn row_into_element(pointer: RowPtr) -> ElementPtr {
    let row = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(row.into()))
}
