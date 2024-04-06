use iced::widget::{scrollable, Scrollable};
use iced::Length;

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Scrollable<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn scrollable_new(content_ptr: ElementPtr) -> SelfPtr {
    let content = unsafe { *Box::from_raw(content_ptr) };
    Box::into_raw(Box::new(scrollable(content)))
}

#[no_mangle]
pub extern "C" fn scrollable_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let scrollable = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(scrollable.width(width)))
}

#[no_mangle]
pub extern "C" fn scrollable_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let scrollable = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(scrollable.height(height)))
}

#[no_mangle]
pub extern "C" fn scrollable_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let scrollable = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(scrollable.into()))
}
