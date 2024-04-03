use std::ffi::c_float;

use iced::widget::Column;
use iced::{Alignment, Padding};

use crate::IcedMessage;

use super::ElementPtr;

type ColumnPtr = *mut Column<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn new_column() -> ColumnPtr {
    Box::into_raw(Box::new(Column::new()))
}

#[no_mangle]
pub extern "C" fn column_align_items(pointer: ColumnPtr, alignment: *mut Alignment) -> ColumnPtr {
    let column = unsafe { *Box::from_raw(pointer) };
    let alignment = unsafe { *Box::from_raw(alignment) };
    Box::into_raw(Box::new(column.align_items(alignment)))
}

#[no_mangle]
pub extern "C" fn column_padding(
    pointer: ColumnPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> ColumnPtr {
    let column = unsafe { *Box::from_raw(pointer) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(column.padding(padding)))
}

#[no_mangle]
pub extern "C" fn column_spacing(pointer: ColumnPtr, pixels: c_float) -> ColumnPtr {
    let column = unsafe { Box::from_raw(pointer) };
    Box::into_raw(Box::new(column.spacing(pixels)))
}

#[no_mangle]
pub extern "C" fn column_with_children(len: libc::size_t, ptr: *const ElementPtr) -> ColumnPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut column = Column::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        column = column.push(*boxed);
    }
    Box::into_raw(Box::new(column))
}

#[no_mangle]
pub extern "C" fn column_extend(
    pointer: ColumnPtr,
    len: libc::size_t,
    elements_ptr: *const ElementPtr,
) -> ColumnPtr {
    let mut column = unsafe { *Box::from_raw(pointer) };
    let slice = unsafe { std::slice::from_raw_parts(elements_ptr, len) };
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        column = column.push(*boxed);
    }
    Box::into_raw(Box::new(column))
}

#[no_mangle]
pub extern "C" fn column_into_element(pointer: ColumnPtr) -> ElementPtr {
    let column = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(column.into()))
}
