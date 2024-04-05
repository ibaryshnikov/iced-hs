use std::borrow::Cow;
use std::slice::from_raw_parts;

use iced::Settings;

#[no_mangle]
pub extern "C" fn settings_new() -> *mut Settings<()> {
    let settings = Settings::default();
    Box::into_raw(Box::new(settings))
}

#[no_mangle]
pub extern "C" fn settings_add_fonts(settings_ptr: *mut Settings<()>, data: *const u8, len: usize) {
    let mut settings = unsafe { Box::from_raw(settings_ptr) };
    let fonts = unsafe { from_raw_parts(data, len) };
    settings.fonts = vec![Cow::from(fonts)];
    std::mem::forget(settings);
}
