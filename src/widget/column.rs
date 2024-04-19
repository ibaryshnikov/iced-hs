use std::ffi::c_float;

use iced::widget::Column;
use iced::{Alignment, Length, Padding};

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Column<'static, IcedMessage>;

#[no_mangle]
extern "C" fn column_new() -> SelfPtr {
    Box::into_raw(Box::new(Column::new()))
}

#[no_mangle]
extern "C" fn column_align_items(self_ptr: SelfPtr, alignment: *mut Alignment) -> SelfPtr {
    let column = unsafe { Box::from_raw(self_ptr) };
    let alignment = unsafe { *Box::from_raw(alignment) };
    Box::into_raw(Box::new(column.align_items(alignment)))
}

#[no_mangle]
extern "C" fn column_padding(
    self_ptr: SelfPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> SelfPtr {
    let column = unsafe { Box::from_raw(self_ptr) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(column.padding(padding)))
}

#[no_mangle]
extern "C" fn column_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let column = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(column.spacing(pixels)))
}

#[no_mangle]
extern "C" fn column_with_children(len: usize, ptr: *const ElementPtr) -> SelfPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut column = Column::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        column = column.push(*boxed);
    }
    Box::into_raw(Box::new(column))
}

#[no_mangle]
extern "C" fn column_extend(
    self_ptr: SelfPtr,
    len: usize,
    elements_ptr: *const ElementPtr,
) -> SelfPtr {
    let mut column = unsafe { *Box::from_raw(self_ptr) };
    let slice = unsafe { std::slice::from_raw_parts(elements_ptr, len) };
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        column = column.push(*boxed);
    }
    Box::into_raw(Box::new(column))
}

#[no_mangle]
extern "C" fn column_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let column = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(column.width(width)))
}

#[no_mangle]
extern "C" fn column_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let column = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(column.height(height)))
}

#[no_mangle]
extern "C" fn column_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let column = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(column.into()))
}
