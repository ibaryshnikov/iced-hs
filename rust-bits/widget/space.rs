use iced::widget::{self, Space};
use iced::Length;

use super::ElementPtr;

type SelfPtr = *mut Space;

#[no_mangle]
extern "C" fn space_new(width_ptr: *mut Length, height_ptr: *mut Length) -> SelfPtr {
    let width = unsafe { *Box::from_raw(width_ptr) };
    let height = unsafe { *Box::from_raw(height_ptr) };
    Box::into_raw(Box::new(Space::new(width, height)))
}

#[no_mangle]
extern "C" fn space_with_width(width_ptr: *mut Length) -> SelfPtr {
    let width = unsafe { *Box::from_raw(width_ptr) };
    Box::into_raw(Box::new(Space::with_width(width)))
}

#[no_mangle]
extern "C" fn space_with_height(height_ptr: *mut Length) -> SelfPtr {
    let height = unsafe { *Box::from_raw(height_ptr) };
    Box::into_raw(Box::new(Space::with_height(height)))
}

#[no_mangle]
extern "C" fn horizontal_space_new() -> SelfPtr {
    Box::into_raw(Box::new(widget::horizontal_space()))
}

#[no_mangle]
extern "C" fn vertical_space_new() -> SelfPtr {
    Box::into_raw(Box::new(widget::vertical_space()))
}

#[no_mangle]
extern "C" fn space_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let space = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(space.into()))
}
