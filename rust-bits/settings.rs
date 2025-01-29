use std::borrow::Cow;
use std::slice::from_raw_parts;

use iced::Settings;

#[no_mangle]
extern "C" fn settings_new() -> *mut Settings {
    let settings = Settings::default();
    Box::into_raw(Box::new(settings))
}

#[no_mangle]
extern "C" fn settings_add_font(settings_ptr: *mut Settings, data: *const u8, len: usize) {
    let mut settings = unsafe { Box::from_raw(settings_ptr) };
    let font = unsafe { from_raw_parts(data, len) };
    settings.fonts.push(Cow::from(font));
    std::mem::forget(settings);
}
