use std::ffi::c_float;

use iced::widget::responsive;
use iced::Size;

use crate::ffi::{from_raw, into_raw};
use crate::ElementPtr;

type ViewFFI = extern "C" fn(width: c_float, height: c_float) -> ElementPtr;

#[no_mangle]
extern "C" fn responsive_new(view_ffi: ViewFFI) -> ElementPtr {
    let view = move |size: Size| from_raw(view_ffi(size.width, size.height));
    into_raw(responsive(view).into())
}
