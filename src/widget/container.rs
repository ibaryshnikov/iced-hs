use std::ffi::c_float;

use iced::widget::{container, Container};
use iced::{Length, Padding};

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Container<'static, IcedMessage>;

#[no_mangle]
extern "C" fn container_new(content_ptr: ElementPtr) -> SelfPtr {
    let content = unsafe { *Box::from_raw(content_ptr) };
    Box::into_raw(Box::new(container(content)))
}

#[no_mangle]
extern "C" fn container_center_x(self_ptr: SelfPtr) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(container.center_x()))
}

#[no_mangle]
extern "C" fn container_center_y(self_ptr: SelfPtr) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(container.center_y()))
}

#[no_mangle]
extern "C" fn container_padding(
    self_ptr: SelfPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(container.padding(padding)))
}

#[no_mangle]
extern "C" fn container_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(container.width(width)))
}

#[no_mangle]
extern "C" fn container_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(container.height(height)))
}

#[no_mangle]
extern "C" fn container_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let container = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(container.into()))
}
