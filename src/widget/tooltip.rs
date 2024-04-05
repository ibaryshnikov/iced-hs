use std::ffi::{c_float, c_uchar};

use iced::widget::tooltip::Position;
use iced::widget::{self, Tooltip};

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Tooltip<'static, IcedMessage>;

#[no_mangle]
pub extern "C" fn tooltip_new(
    content_ptr: ElementPtr,
    tooltip_ptr: ElementPtr,
    position_raw: c_uchar,
) -> SelfPtr {
    let content = unsafe { *Box::from_raw(content_ptr) };
    let tooltip = unsafe { *Box::from_raw(tooltip_ptr) };
    let position = match position_raw {
        1 => Position::FollowCursor,
        2 => Position::Top,
        3 => Position::Bottom,
        4 => Position::Left,
        5 => Position::Right,
        other => panic!("Unexpected Position value: {}", other),
    };
    Box::into_raw(Box::new(widget::tooltip(content, tooltip, position)))
}

#[no_mangle]
pub extern "C" fn tooltip_gap(self_ptr: SelfPtr, gap: c_float) -> SelfPtr {
    let tooltip = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(tooltip.gap(gap)))
}

#[no_mangle]
pub extern "C" fn tooltip_padding(self_ptr: SelfPtr, padding: c_float) -> SelfPtr {
    let tooltip = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(tooltip.padding(padding)))
}

#[no_mangle]
pub extern "C" fn tooltip_snap_within_viewport(self_ptr: SelfPtr, snap_raw: c_uchar) -> SelfPtr {
    let tooltip = unsafe { Box::from_raw(self_ptr) };
    let snap = match snap_raw {
        0 => false,
        1 => true,
        other => panic!("Unexpected snap value: {}", other),
    };
    Box::into_raw(Box::new(tooltip.snap_within_viewport(snap)))
}

// todo
// #[no_mangle]
// pub extern "C" fn tooltip_style(
//     self_ptr: SelfPtr,
//     style_ptr: Some style struct,
// ) -> SelfPtr {
//     let tooltip = unsafe { Box::from_raw(self_ptr) };
//     let style = ...
//     Box::into_raw(Box::new(tooltip.style(style)));
// }

#[no_mangle]
pub extern "C" fn tooltip_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let tooltip = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(tooltip.into()))
}
