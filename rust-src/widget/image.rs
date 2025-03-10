use iced::advanced::image::Handle;
use iced::widget::{image, Image};
use iced::Length;

use super::ElementPtr;
use crate::ffi::{from_raw, into_raw};

type SelfPtr = *mut Image<Handle>;

#[no_mangle]
extern "C" fn image_new(handle_ptr: *mut Handle) -> SelfPtr {
    let handle = from_raw(handle_ptr);
    into_raw(image(handle))
}

#[no_mangle]
extern "C" fn image_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let image = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(image.width(width))
}

#[no_mangle]
extern "C" fn image_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let image = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(image.height(height))
}

#[no_mangle]
extern "C" fn image_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_raw(from_raw(self_ptr).into())
}
