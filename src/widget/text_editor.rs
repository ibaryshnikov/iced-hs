use iced::advanced::text::highlighter::PlainText;
use iced::widget::{text_editor, TextEditor};
use text_editor::{Action, Content};

use crate::{HaskellMessage, IcedMessage};

use super::ElementPtr;

type TextEditorPtr = *mut TextEditor<'static, PlainText, IcedMessage>;

type ActionCallback = unsafe extern "C" fn(action: *mut Action) -> *const u8;

#[no_mangle]
pub extern "C" fn new_content() -> *mut Content {
    Box::into_raw(Box::new(Content::new()))
}

#[no_mangle]
pub extern "C" fn free_content(pointer: *mut Content) {
    let _ = unsafe { Box::from_raw(pointer) };
}

#[no_mangle]
pub extern "C" fn content_perform(content: *mut Content, action: *mut Action) {
    let mut content = unsafe { Box::from_raw(content) };
    let action = unsafe { Box::from_raw(action) };
    content.perform(*action);
    // don't drop content yet
    std::mem::forget(content);
}

#[no_mangle]
pub extern "C" fn new_text_editor(content: *mut Content) -> TextEditorPtr {
    let content = unsafe { Box::from_raw(content) };
    // Content is tracked on the Haskell side
    let editor = text_editor(Box::leak(content));
    Box::into_raw(Box::new(editor))
}

#[no_mangle]
pub extern "C" fn text_editor_on_action(
    pointer: TextEditorPtr,
    on_action: ActionCallback,
) -> TextEditorPtr {
    let mut text_editor = unsafe { *Box::from_raw(pointer) };
    text_editor = text_editor.on_action(move |action| {
        let action_ptr = Box::into_raw(Box::new(action));
        let message_ptr = unsafe { on_action(action_ptr) };
        IcedMessage::Ptr(HaskellMessage { ptr: message_ptr })
    });
    Box::into_raw(Box::new(text_editor))
}

#[no_mangle]
pub extern "C" fn text_editor_into_element(pointer: TextEditorPtr) -> ElementPtr {
    let text_editor = unsafe { *Box::from_raw(pointer) };
    Box::into_raw(Box::new(text_editor.into()))
}
