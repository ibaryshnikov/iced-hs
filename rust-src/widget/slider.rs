use std::ffi::{c_float, c_int};

use iced::widget::Slider;
use iced::Length;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Slider<'static, c_int, IcedMessage>;
type OnChangeFFI = extern "C" fn(value: c_int) -> *const u8;

#[no_mangle]
extern "C" fn slider_new(
    range_start: c_int,
    range_end: c_int,
    value: c_int,
    on_change_ffi: OnChangeFFI,
) -> SelfPtr {
    let on_change = move |value| {
        let message_ptr = on_change_ffi(value);
        IcedMessage::ptr(message_ptr)
    };
    let range = range_start..=range_end;
    let slider = Slider::new(range, value, on_change);
    into_raw(slider)
}

#[no_mangle]
extern "C" fn slider_default(self_ptr: SelfPtr, value: c_int) -> SelfPtr {
    let slider = from_raw(self_ptr);
    into_raw(slider.default(value))
}

#[no_mangle]
extern "C" fn slider_on_release(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let slider = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(slider.on_release(message))
}

#[no_mangle]
extern "C" fn slider_step(self_ptr: SelfPtr, value: c_int) -> SelfPtr {
    let slider = from_raw(self_ptr);
    into_raw(slider.step(value))
}

#[no_mangle]
extern "C" fn slider_shift_step(self_ptr: SelfPtr, value: c_int) -> SelfPtr {
    let slider = from_raw(self_ptr);
    into_raw(slider.shift_step(value))
}

#[no_mangle]
extern "C" fn slider_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let slider = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(slider.width(width))
}

#[no_mangle]
extern "C" fn slider_height(self_ptr: SelfPtr, height: c_float) -> SelfPtr {
    let slider = from_raw(self_ptr);
    into_raw(slider.height(height))
}

#[no_mangle]
extern "C" fn slider_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
