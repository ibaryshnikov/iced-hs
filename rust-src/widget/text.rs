use std::ffi::{c_char, c_float, c_uchar};
use std::sync::Arc;

use iced::widget::{text, Text};
use iced::{Color, Length};
use text::Style;

use crate::ffi::{from_raw, into_element, into_raw, read_c_string};
use crate::{free_haskell_fun_ptr, ElementPtr};

type SelfPtr = *mut Text<'static>;

#[repr(transparent)]
struct StyleCallback {
    inner: extern "C" fn(style: &mut Style, theme: c_uchar),
}

impl Drop for StyleCallback {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

#[no_mangle]
extern "C" fn text_new(input: *mut c_char) -> SelfPtr {
    let string = read_c_string(input);
    into_raw(text(string))
}

#[no_mangle]
extern "C" fn text_color(self_ptr: SelfPtr, color_ptr: *mut Color) -> SelfPtr {
    let text = from_raw(self_ptr);
    let color = from_raw(color_ptr);
    Box::into_raw(Box::new(text.color(color)))
}

#[no_mangle]
extern "C" fn text_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let text = from_raw(self_ptr);
    into_raw(text.size(size))
}

#[no_mangle]
extern "C" fn text_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let text = from_raw(self_ptr);
    let callback = Arc::new(callback);
    let text = text.style(move |theme| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let mut style = Style::default();
        (callback.inner)(&mut style, theme_raw);
        style
    });
    into_raw(text)
}

#[no_mangle]
extern "C" fn text_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let text = from_raw(self_ptr);
    let width = from_raw(width);
    into_raw(text.width(width))
}

#[no_mangle]
extern "C" fn text_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let text = from_raw(self_ptr);
    let height = from_raw(height);
    into_raw(text.height(height))
}

#[no_mangle]
extern "C" fn text_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}

#[no_mangle]
extern "C" fn text_style_set_color(style: &mut Style, color_ptr: *mut Color) {
    let color = from_raw(color_ptr);
    style.color = Some(color);
}
