use std::ffi::c_float;
use std::sync::Arc;

use iced::widget::responsive;
use iced::Size;

use crate::ffi::{from_raw, into_raw};
use crate::free_haskell_fun_ptr;
use crate::ElementPtr;

#[repr(transparent)]
struct ViewFFI {
    inner: extern "C" fn(width: c_float, height: c_float) -> ElementPtr,
}

impl Drop for ViewFFI {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) }
    }
}

#[no_mangle]
extern "C" fn responsive_new(view_ffi: ViewFFI) -> ElementPtr {
    let view_ffi = Arc::new(view_ffi);
    let view = move |size: Size| from_raw((view_ffi.inner)(size.width, size.height));
    into_raw(responsive(view).into())
}
