use std::ffi::c_float;

use iced::widget::responsive;
use iced::Size;

use super::ElementPtr;

type ViewFFI = extern "C" fn(width: c_float, height: c_float) -> ElementPtr;

#[no_mangle]
extern "C" fn responsive_new(view_ffi: ViewFFI) -> ElementPtr {
    let view = move |size: Size| {
        let element_ptr = view_ffi(size.width, size.height);
        unsafe { *Box::from_raw(element_ptr) }
    };
    Box::into_raw(Box::new(responsive(view).into()))
}
