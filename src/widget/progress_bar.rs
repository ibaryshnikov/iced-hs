use std::ffi::c_float;

use iced::widget::{progress_bar, ProgressBar};
use iced::Length;

use super::ElementPtr;

type SelfPtr = *mut ProgressBar;

#[no_mangle]
pub extern "C" fn progress_bar_new(
    range_from: c_float,
    range_to: c_float,
    value: c_float,
) -> SelfPtr {
    let range = range_from..=range_to;
    Box::into_raw(Box::new(progress_bar(range, value)))
}

#[no_mangle]
pub extern "C" fn progress_bar_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let progress_bar = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(progress_bar.width(width)))
}

#[no_mangle]
pub extern "C" fn progress_bar_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let progress_bar = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(progress_bar.height(height)))
}

#[no_mangle]
pub extern "C" fn progress_bar_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let progress_bar = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(progress_bar.into()))
}
