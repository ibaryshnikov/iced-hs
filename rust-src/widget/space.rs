use iced::widget::{self, Space};
use iced::Length;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::ElementPtr;

type SelfPtr = *mut Space;

#[no_mangle]
extern "C" fn space_new(width_ptr: *mut Length, height_ptr: *mut Length) -> SelfPtr {
    let width = from_raw(width_ptr);
    let height = from_raw(height_ptr);
    into_raw(Space::new(width, height))
}

#[no_mangle]
extern "C" fn space_with_width(width_ptr: *mut Length) -> SelfPtr {
    let width = from_raw(width_ptr);
    into_raw(Space::with_width(width))
}

#[no_mangle]
extern "C" fn space_with_height(height_ptr: *mut Length) -> SelfPtr {
    let height = from_raw(height_ptr);
    into_raw(Space::with_height(height))
}

#[no_mangle]
extern "C" fn horizontal_space_new() -> SelfPtr {
    into_raw(widget::horizontal_space())
}

#[no_mangle]
extern "C" fn vertical_space_new() -> SelfPtr {
    into_raw(widget::vertical_space())
}

#[no_mangle]
extern "C" fn space_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
