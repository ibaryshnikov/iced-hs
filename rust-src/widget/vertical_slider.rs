use std::ffi::{c_float, c_int};
use std::sync::Arc;

use iced::widget::VerticalSlider;
use iced::Length;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::{free_haskell_fun_ptr, ElementPtr, IcedMessage};

type SelfPtr = *mut VerticalSlider<'static, c_int, IcedMessage>;

#[repr(transparent)]
struct OnChangeFFI {
    inner: extern "C" fn(value: c_int) -> *const u8,
}

impl Drop for OnChangeFFI {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

#[no_mangle]
extern "C" fn vertical_slider_new(
    range_start: c_int,
    range_end: c_int,
    value: c_int,
    on_change_ffi: OnChangeFFI,
) -> SelfPtr {
    let on_change_ffi = Arc::new(on_change_ffi);
    let on_change = move |value| {
        let message_ptr = (on_change_ffi.inner)(value);
        IcedMessage::ptr(message_ptr)
    };
    let range = range_start..=range_end;
    let vertical_slider = VerticalSlider::new(range, value, on_change);
    into_raw(vertical_slider)
}

#[no_mangle]
extern "C" fn vertical_slider_default(self_ptr: SelfPtr, value: c_int) -> SelfPtr {
    let vertical_slider = from_raw(self_ptr);
    into_raw(vertical_slider.default(value))
}

#[no_mangle]
extern "C" fn vertical_slider_on_release(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let vertical_slider = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(vertical_slider.on_release(message))
}

#[no_mangle]
extern "C" fn vertical_slider_step(self_ptr: SelfPtr, value: c_int) -> SelfPtr {
    let vertical_slider = from_raw(self_ptr);
    into_raw(vertical_slider.step(value))
}

#[no_mangle]
extern "C" fn vertical_slider_shift_step(self_ptr: SelfPtr, value: c_int) -> SelfPtr {
    let vertical_slider = from_raw(self_ptr);
    into_raw(vertical_slider.shift_step(value))
}

#[no_mangle]
extern "C" fn vertical_slider_width(self_ptr: SelfPtr, width: c_float) -> SelfPtr {
    let vertical_slider = from_raw(self_ptr);
    into_raw(vertical_slider.width(width))
}

#[no_mangle]
extern "C" fn vertical_slider_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let vertical_slider = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(vertical_slider.height(height))
}

#[no_mangle]
extern "C" fn vertical_slider_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
