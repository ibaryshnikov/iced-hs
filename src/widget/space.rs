use iced::widget::{self, Space};

use super::ElementPtr;

type SpacePtr = *mut Space;

#[no_mangle]
pub extern "C" fn new_horizontal_space() -> SpacePtr {
    Box::into_raw(Box::new(widget::horizontal_space()))
}

#[no_mangle]
pub extern "C" fn new_vertical_space() -> SpacePtr {
    Box::into_raw(Box::new(widget::vertical_space()))
}

#[no_mangle]
pub extern "C" fn space_into_element(pointer: SpacePtr) -> ElementPtr {
    let space = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(space.into()))
}
