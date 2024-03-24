use iced::widget;

use super::ElementPtr;

#[no_mangle]
pub extern "C" fn new_horizontal_space() -> ElementPtr {
    Box::into_raw(Box::new(widget::horizontal_space().into()))
}
