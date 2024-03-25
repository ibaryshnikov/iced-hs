use iced::widget::text_editor;
use text_editor::{Action, Content};

use crate::{HaskellMessage, IcedMessage};

use super::ElementPtr;

type ActionCallback = unsafe extern "C" fn(action: *mut Action) -> *const u8;

#[no_mangle]
pub extern "C" fn new_content() -> *mut Content {
    Box::into_raw(Box::new(Content::new()))
}

#[no_mangle]
pub extern "C" fn apply_action(content: *mut Content, action: *mut Action) {
    let mut content = unsafe { Box::from_raw(content) };
    let action = unsafe { Box::from_raw(action) };
    content.perform(*action);
    // don't drop content yet
    std::mem::forget(content);
}

#[no_mangle]
pub extern "C" fn new_text_editor(
    content: *mut Content,
    maybe_on_action: Option<ActionCallback>,
) -> ElementPtr {
    let content = unsafe { Box::from_raw(content) };
    // Content is tracket on the Haskell side
    let mut editor = text_editor(Box::leak(content));
    if let Some(on_action) = maybe_on_action {
        editor = editor.on_action(move |action| {
            let action_ptr = Box::into_raw(Box::new(action));
            let message_ptr = unsafe { on_action(action_ptr) };
            IcedMessage::Ptr(HaskellMessage { ptr: message_ptr })
        });
    }
    Box::into_raw(Box::new(editor.into()))
}
