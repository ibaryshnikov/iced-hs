use std::ffi::c_float;

use iced::widget::Column;
use iced::{Alignment, Length, Padding};

use crate::ffi::{from_raw, into_element, into_raw};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Column<'static, IcedMessage>;

#[no_mangle]
extern "C" fn column_new() -> SelfPtr {
    into_raw(Column::new())
}

#[no_mangle]
extern "C" fn column_align_x(self_ptr: SelfPtr, alignment: *mut Alignment) -> SelfPtr {
    let column = from_raw(self_ptr);
    let alignment = from_raw(alignment);
    into_raw(column.align_x(alignment))
}

#[no_mangle]
extern "C" fn column_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let column = from_raw(self_ptr);
    let padding = from_raw(padding_ptr);
    into_raw(column.padding(padding))
}

#[no_mangle]
extern "C" fn column_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let column = from_raw(self_ptr);
    into_raw(column.spacing(pixels))
}

#[no_mangle]
extern "C" fn column_with_children(len: usize, ptr: *const ElementPtr) -> SelfPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut column = Column::new();
    for item in slice {
        column = column.push(from_raw(*item));
    }
    into_raw(column)
}

#[no_mangle]
extern "C" fn column_extend(
    self_ptr: SelfPtr,
    len: usize,
    elements_ptr: *const ElementPtr,
) -> SelfPtr {
    let mut column = from_raw(self_ptr);
    let slice = unsafe { std::slice::from_raw_parts(elements_ptr, len) };
    for item in slice {
        column = column.push(from_raw(*item));
    }
    into_raw(column)
}

#[no_mangle]
extern "C" fn column_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let column = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(column.width(width))
}

#[no_mangle]
extern "C" fn column_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let column = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(column.height(height))
}

#[no_mangle]
extern "C" fn column_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
