use iced::widget::Column;

use super::ElementPtr;

#[no_mangle]
pub extern "C" fn new_column(len: libc::size_t, ptr: *const ElementPtr) -> ElementPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut column = Column::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        column = column.push(*boxed);
    }
    Box::into_raw(Box::new(column.into()))
}
