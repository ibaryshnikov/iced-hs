use std::ffi::{c_float, c_int};

use iced::widget::Slider;
use iced::Length;

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Slider<'static, c_int, IcedMessage>;
type OnChangeFFI = unsafe extern "C" fn(value: c_int) -> *const u8;

#[no_mangle]
extern "C" fn slider_new(
    range_start: c_int,
    range_end: c_int,
    value: c_int,
    on_change_ffi: OnChangeFFI,
) -> SelfPtr {
    let on_change = move |value| {
        let message_ptr = unsafe { on_change_ffi(value) };
        IcedMessage::ptr(message_ptr)
    };
    let range = range_start..=range_end;
    let slider = Slider::new(range, value, on_change);
    Box::into_raw(Box::new(slider))
}

#[no_mangle]
extern "C" fn slider_on_release(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let slider = unsafe { Box::from_raw(self_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    Box::into_raw(Box::new(slider.on_release(message)))
}

#[no_mangle]
extern "C" fn slider_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let slider = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(slider.width(width)))
}

#[no_mangle]
extern "C" fn slider_height(self_ptr: SelfPtr, height: c_float) -> SelfPtr {
    let slider = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(slider.height(height)))
}

#[no_mangle]
extern "C" fn slider_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let slider = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(slider.into()))
}
