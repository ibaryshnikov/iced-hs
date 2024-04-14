use std::ffi::{c_char, c_float};

use iced::advanced::text::highlighter::PlainText;
use iced::widget::{text_editor, TextEditor};
use iced::{Length, Padding};
use text_editor::{Action, Content};

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut TextEditor<'static, PlainText, IcedMessage>;

type OnActionFFI = unsafe extern "C" fn(action: *mut Action) -> *const u8;

#[no_mangle]
pub extern "C" fn text_editor_content_new() -> *mut Content {
    Box::into_raw(Box::new(Content::new()))
}

#[no_mangle]
pub extern "C" fn text_editor_content_with_text(input_ptr: *mut c_char) -> *mut Content {
    let input = read_c_string(input_ptr);
    Box::into_raw(Box::new(Content::with_text(&input)))
}

#[no_mangle]
pub extern "C" fn text_editor_content_perform(content: &mut Content, action: *mut Action) {
    let action = unsafe { *Box::from_raw(action) };
    content.perform(action);
}

#[no_mangle]
pub extern "C" fn text_editor_content_free(pointer: *mut Content) {
    let _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
pub extern "C" fn text_editor_new(content: *mut Content) -> SelfPtr {
    let content = unsafe { Box::from_raw(content) };
    // Content is tracked on the Haskell side
    let editor = text_editor(Box::leak(content));
    Box::into_raw(Box::new(editor))
}

#[no_mangle]
pub extern "C" fn text_editor_on_action(self_ptr: SelfPtr, on_action_ffi: OnActionFFI) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    let text_editor = text_editor.on_action(move |action| {
        let action_ptr = Box::into_raw(Box::new(action));
        let message_ptr = unsafe { on_action_ffi(action_ptr) };
        IcedMessage::ptr(message_ptr)
    });
    Box::into_raw(Box::new(text_editor))
}

#[no_mangle]
pub extern "C" fn text_editor_padding(
    self_ptr: SelfPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(text_editor.padding(padding)))
}

#[no_mangle]
pub extern "C" fn text_editor_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let text_editor = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(text_editor.height(height)))
}

#[no_mangle]
pub extern "C" fn text_editor_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let text_editor = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(text_editor.into()))
}
