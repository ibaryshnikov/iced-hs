use iced::widget::{self, Space};
use iced::Length;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::ElementPtr;

type SelfPtr = *mut Space;

#[no_mangle]
extern "C" fn space_new() -> SelfPtr {
    into_raw(Space::new())
}

#[no_mangle]
extern "C" fn space_width(self_ptr: SelfPtr, width_ptr: *mut Length) -> SelfPtr {
    let space = from_raw(self_ptr);
    let width = from_raw(width_ptr);
    into_raw(space.width(width))
}

#[no_mangle]
extern "C" fn space_height(self_ptr: SelfPtr, height_ptr: *mut Length) -> SelfPtr {
    let space = from_raw(self_ptr);
    let height = from_raw(height_ptr);
    into_raw(space.height(height))
}

#[no_mangle]
extern "C" fn space_horizontal() -> SelfPtr {
    into_raw(widget::space::horizontal())
}

#[no_mangle]
extern "C" fn space_vertical() -> SelfPtr {
    into_raw(widget::space::vertical())
}

#[no_mangle]
extern "C" fn space_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
