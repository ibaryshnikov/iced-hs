use iced::widget;

use super::ElementPtr;

#[no_mangle]
pub extern "C" fn new_vertical_space() -> ElementPtr {
    Box::into_raw(Box::new(widget::vertical_space().into()))
}
