use std::ffi::c_float;

// use iced::mouse::ScrollDelta;
use iced::widget::{mouse_area, MouseArea};
use iced::Point;

use crate::ffi::{from_raw, into_element, into_raw};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut MouseArea<'static, IcedMessage>;

type OnMoveFFI = extern "C" fn(x: c_float, y: c_float) -> *const u8;

#[no_mangle]
extern "C" fn mouse_area_new(content_ptr: ElementPtr) -> SelfPtr {
    let content = from_raw(content_ptr);
    into_raw(mouse_area(content))
}

#[no_mangle]
extern "C" fn mouse_area_on_double_click(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_double_click(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_enter(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_enter(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_exit(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_exit(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_middle_press(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_middle_press(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_middle_release(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_middle_release(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_move(self_ptr: SelfPtr, on_move_ffi: OnMoveFFI) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let callback = move |point: Point| {
        let message_ptr = on_move_ffi(point.x, point.y);
        IcedMessage::ptr(message_ptr)
    };
    into_raw(mouse_area.on_move(callback))
}

#[no_mangle]
extern "C" fn mouse_area_on_press(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_press(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_release(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_release(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_right_press(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_right_press(message))
}

#[no_mangle]
extern "C" fn mouse_area_on_right_release(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let message = IcedMessage::ptr(message_ptr);
    into_raw(mouse_area.on_right_release(message))
}

/*
 * TODO: figure out ffi type for callback
#[no_mangle]
extern "C" fn mouse_area_on_scroll(self_ptr: SelfPtr, on_scroll_ffi: OnScrollFFI) -> SelfPtr {
    let mouse_area = from_raw(self_ptr);
    let callback = move |delta: ScrollDelta| {
        println!("Delta is {:?}", delta);
        let message_ptr = on_scroll_ffi(0.0, 0.0);
        IcedMessage::ptr(message_ptr)
    };
    Box::into_raw(Box::new(mouse_area.on_scroll(callback)))
}
*/

#[no_mangle]
extern "C" fn mouse_area_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
