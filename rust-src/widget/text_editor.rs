use std::ffi::{c_char, c_float, c_uchar};

use iced::advanced::text::highlighter::PlainText;
use iced::widget::{text_editor, TextEditor};
use iced::{Background, Border, Color, Length, Padding};
use text_editor::{Action, Content, Status, Style};

use super::{ElementPtr, IcedMessage};
use crate::ffi::read_c_string;

type SelfPtr = *mut TextEditor<'static, PlainText, IcedMessage>;

type OnActionFFI = extern "C" fn(action: *mut Action) -> *const u8;

type StyleCallback = extern "C" fn(style: &mut Style, theme: c_uchar, status: c_uchar);

#[no_mangle]
extern "C" fn text_editor_content_new() -> *mut Content {
    Box::into_raw(Box::new(Content::new()))
}

#[no_mangle]
extern "C" fn text_editor_content_with_text(input_ptr: *mut c_char) -> *mut Content {
    let input = read_c_string(input_ptr);
    Box::into_raw(Box::new(Content::with_text(&input)))
}

#[no_mangle]
extern "C" fn text_editor_content_perform(content: &mut Content, action: *mut Action) {
    let action = unsafe { *Box::from_raw(action) };
    content.perform(action);
}

#[no_mangle]
extern "C" fn text_editor_content_free(pointer: *mut Content) {
    let _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
extern "C" fn text_editor_new(content: *mut Content) -> SelfPtr {
    let content = unsafe { Box::from_raw(content) };
    // Content is tracked on the Haskell side
    let editor = text_editor(Box::leak(content));
    Box::into_raw(Box::new(editor))
}

#[no_mangle]
extern "C" fn text_editor_on_action(self_ptr: SelfPtr, on_action_ffi: OnActionFFI) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    let text_editor = text_editor.on_action(move |action| {
        let action_ptr = Box::into_raw(Box::new(action));
        let message_ptr = on_action_ffi(action_ptr);
        IcedMessage::ptr(message_ptr)
    });
    Box::into_raw(Box::new(text_editor))
}

#[no_mangle]
extern "C" fn text_editor_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    let padding = unsafe { *Box::from_raw(padding_ptr) };
    Box::into_raw(Box::new(text_editor.padding(padding)))
}

fn status_to_raw(status: Status) -> c_uchar {
    match status {
        Status::Active => 0,
        Status::Hovered => 1,
        Status::Focused => 2,
        Status::Disabled => 3,
    }
}

#[no_mangle]
extern "C" fn text_editor_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_editor.style(move |theme, status| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let status_raw = status_to_raw(status);
        let mut style = text_editor::default(theme, status);
        callback(&mut style, theme_raw, status_raw);
        style
    })))
}

#[no_mangle]
extern "C" fn text_editor_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(text_editor.height(height)))
}

#[no_mangle]
extern "C" fn text_editor_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text_editor = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_editor.into()))
}

#[no_mangle]
extern "C" fn text_editor_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.background = Background::Color(color);
}

#[no_mangle]
extern "C" fn text_editor_style_set_border(
    style: &mut Style,
    color_ptr: *mut Color,
    width: c_float,
    radius: c_float,
) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.border = Border {
        color,
        width,
        radius: radius.into(),
    }
}

#[no_mangle]
extern "C" fn text_editor_style_set_icon(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.icon = color;
}

#[no_mangle]
extern "C" fn text_editor_style_set_placeholder(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.placeholder = color;
}

#[no_mangle]
extern "C" fn text_editor_style_set_value(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.value = color;
}

#[no_mangle]
extern "C" fn text_editor_style_set_selection(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.selection = color;
}
