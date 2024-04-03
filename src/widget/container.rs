use iced::widget::{container, Container};
use iced::Length;

use super::{ElementPtr, IcedMessage};

type ContainerPtr = *mut Container<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn new_container(content_ptr: ElementPtr) -> ContainerPtr {
    let content = unsafe { *Box::from_raw(content_ptr) };
    Box::into_raw(Box::new(container(content)))
}

#[no_mangle]
pub extern "C" fn container_center_x(pointer: ContainerPtr) -> ContainerPtr {
    let container = unsafe { Box::from_raw(pointer) };
    Box::into_raw(Box::new(container.center_x()))
}

#[no_mangle]
pub extern "C" fn container_center_y(pointer: ContainerPtr) -> ContainerPtr {
    let container = unsafe { Box::from_raw(pointer) };
    Box::into_raw(Box::new(container.center_y()))
}

#[no_mangle]
pub extern "C" fn container_into_element(pointer: ContainerPtr) -> ElementPtr {
    let container = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(container.into()))
}

#[no_mangle]
pub extern "C" fn container_height(pointer: ContainerPtr, height: *mut Length) -> ContainerPtr {
    let container = unsafe { Box::from_raw(pointer) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(container.height(height)))
}

#[no_mangle]
pub extern "C" fn container_width(pointer: ContainerPtr, width: *mut Length) -> ContainerPtr {
    let container = unsafe { Box::from_raw(pointer) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(container.width(width)))
}
