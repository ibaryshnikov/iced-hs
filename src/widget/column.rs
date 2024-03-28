use iced::widget::Column;

use crate::IcedMessage;

use super::ElementPtr;

type ColumnPtr = *mut Column<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn new_column() -> ColumnPtr {
    Box::into_raw(Box::new(Column::new()))
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
