use std::ffi::c_float;

use iced::widget::Row;
use iced::{Alignment, Length, Padding};

use crate::ffi::{from_raw, into_element, into_raw};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Row<'static, IcedMessage>;

#[no_mangle]
extern "C" fn row_new() -> SelfPtr {
    into_raw(Row::new())
}

#[no_mangle]
extern "C" fn row_align_y(self_ptr: SelfPtr, alignment: *mut Alignment) -> SelfPtr {
    let row = from_raw(self_ptr);
    let alignment = from_raw(alignment);
    into_raw(row.align_y(alignment))
}

#[no_mangle]
extern "C" fn row_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let row = from_raw(self_ptr);
    let padding = from_raw(padding_ptr);
    into_raw(row.padding(padding))
}

#[no_mangle]
extern "C" fn row_spacing(self_ptr: SelfPtr, pixels: c_float) -> SelfPtr {
    let row = from_raw(self_ptr);
    into_raw(row.spacing(pixels))
}

#[no_mangle]
extern "C" fn row_with_children(len: usize, ptr: *const ElementPtr) -> SelfPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut row = Row::new();
    for item in slice {
        row = row.push(from_raw(*item));
    }
    into_raw(row)
}

#[no_mangle]
extern "C" fn row_extend(
    self_ptr: SelfPtr,
    len: usize,
    elements_ptr: *const ElementPtr,
) -> SelfPtr {
    let mut row = from_raw(self_ptr);
    let slice = unsafe { std::slice::from_raw_parts(elements_ptr, len) };
    for item in slice {
        row = row.push(from_raw(*item));
    }
    into_raw(row)
}

#[no_mangle]
extern "C" fn row_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let row = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(row.width(width))
}

#[no_mangle]
extern "C" fn row_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let row = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(row.height(height))
}

#[no_mangle]
extern "C" fn row_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
