use std::ffi::{c_float, c_uchar};

use iced::widget::tooltip::Position;
use iced::widget::{self, Tooltip};

use crate::ffi::{from_raw, into_element, into_raw, read_c_bool};
use crate::{ElementPtr, IcedMessage};

type SelfPtr = *mut Tooltip<'static, IcedMessage>;

#[no_mangle]
extern "C" fn tooltip_new(
    content_ptr: ElementPtr,
    tooltip_ptr: ElementPtr,
    position_raw: c_uchar,
) -> SelfPtr {
    let content = from_raw(content_ptr);
    let tooltip = from_raw(tooltip_ptr);
    let position = match position_raw {
        1 => Position::FollowCursor,
        2 => Position::Top,
        3 => Position::Bottom,
        4 => Position::Left,
        5 => Position::Right,
        other => panic!("Unexpected Position value: {other}"),
    };
    into_raw(widget::tooltip(content, tooltip, position))
}

#[no_mangle]
extern "C" fn tooltip_gap(self_ptr: SelfPtr, gap: c_float) -> SelfPtr {
    let tooltip = from_raw(self_ptr);
    into_raw(tooltip.gap(gap))
}

#[no_mangle]
extern "C" fn tooltip_padding(self_ptr: SelfPtr, padding: c_float) -> SelfPtr {
    let tooltip = from_raw(self_ptr);
    into_raw(tooltip.padding(padding))
}

#[no_mangle]
extern "C" fn tooltip_snap_within_viewport(self_ptr: SelfPtr, snap_raw: c_uchar) -> SelfPtr {
    let tooltip = from_raw(self_ptr);
    let snap = read_c_bool(snap_raw);
    into_raw(tooltip.snap_within_viewport(snap))
}

// todo
// #[no_mangle]
// extern "C" fn tooltip_style(
//     self_ptr: SelfPtr,
//     style_ptr: Some style struct,
// ) -> SelfPtr {
//     let tooltip = unsafe { Box::from_raw(self_ptr) };
//     let style = ...
//     Box::into_raw(Box::new(tooltip.style(style)));
// }

#[no_mangle]
extern "C" fn tooltip_into_element(self_ptr: SelfPtr) -> ElementPtr {
    into_element(self_ptr)
}
