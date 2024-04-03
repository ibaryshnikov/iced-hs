use std::borrow::Cow;
use std::slice::from_raw_parts;

use iced::Settings;
use libc::size_t;

#[no_mangle]
pub extern "C" fn new_settings() -> *mut Settings<()> {
    let settings = Settings::default();
    Box::into_raw(Box::new(settings))
}

#[no_mangle]
pub extern "C" fn settings_add_fonts(
    settings_ptr: *mut Settings<()>,
    data: *const u8,
    len: size_t,
) {
    let mut settings = unsafe { Box::from_raw(settings_ptr) };
    let fonts = unsafe { from_raw_parts(data, len) };
    settings.fonts = vec![Cow::from(fonts)];
    std::mem::forget(settings);
}
