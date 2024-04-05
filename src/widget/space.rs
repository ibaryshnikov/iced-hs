use iced::widget::{self, Space};

use super::ElementPtr;

type SelfPtr = *mut Space;

#[no_mangle]
pub extern "C" fn horizontal_space_new() -> SelfPtr {
    Box::into_raw(Box::new(widget::horizontal_space()))
}

#[no_mangle]
pub extern "C" fn vertical_space_new() -> SelfPtr {
    Box::into_raw(Box::new(widget::vertical_space()))
}

#[no_mangle]
pub extern "C" fn space_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let space = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(space.into()))
}
