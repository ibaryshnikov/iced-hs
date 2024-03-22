use iced::widget::Row;

use super::ElementPtr;

#[no_mangle]
pub extern "C" fn new_row(len: libc::size_t, ptr: *const ElementPtr) -> ElementPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut row = Row::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        row = row.push(*boxed);
    }
    Box::into_raw(Box::new(row.into()))
}
