use iced::widget::{scrollable, Scrollable};
use iced::Length;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Scrollable<'static, IcedMessage>;

#[no_mangle]
extern "C" fn scrollable_new(content_ptr: ElementPtr) -> SelfPtr {
    let content = from_raw(content_ptr);
    into_raw(scrollable(content))
}

#[no_mangle]
extern "C" fn scrollable_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let scrollable = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(scrollable.width(width))
}

#[no_mangle]
extern "C" fn scrollable_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let scrollable = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(scrollable.height(height))
}

#[no_mangle]
extern "C" fn scrollable_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
