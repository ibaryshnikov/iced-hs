use std::ffi::{c_float, c_uchar};

use container::Style;
use iced::widget::{center, container, Container};
use iced::{Background, Border, Color, Length, Padding};

use super::{ElementPtr, IcedMessage};

type SelfPtr = *mut Container<'static, IcedMessage>;

type StyleCallback = extern "C" fn(style: &mut Style, theme: c_uchar);

enum BasicStyle {
    BorderedBox,
    RoundedBox,
    Transparent,
}

use BasicStyle::*;

impl BasicStyle {
    fn from_raw(value: u8) -> Self {
        match value {
            0 => BorderedBox,
            1 => RoundedBox,
            2 => Transparent,
            other => panic!("Unexpected value in container BasicStyle: {other}"),
        }
    }
}

#[no_mangle]
extern "C" fn container_new(content_ptr: ElementPtr) -> SelfPtr {
    let content = unsafe { *Box::from_raw(content_ptr) };
    Box::into_raw(Box::new(container(content)))
}

#[no_mangle]
extern "C" fn container_new_centered(content_ptr: ElementPtr) -> SelfPtr {
    let content = unsafe { *Box::from_raw(content_ptr) };
    Box::into_raw(Box::new(center(content)))
}

#[no_mangle]
extern "C" fn container_center_x(self_ptr: SelfPtr, width_ptr: *mut Length) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width_ptr) };
    Box::into_raw(Box::new(container.center_x(width)))
}

#[no_mangle]
extern "C" fn container_center_y(self_ptr: SelfPtr, height_ptr: *mut Length) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height_ptr) };
    Box::into_raw(Box::new(container.center_y(height)))
}

#[no_mangle]
extern "C" fn container_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let padding = unsafe { *Box::from_raw(padding_ptr) };
    Box::into_raw(Box::new(container.padding(padding)))
}

#[no_mangle]
extern "C" fn container_style_basic(self_ptr: SelfPtr, style_raw: c_uchar) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let style_fn = match BasicStyle::from_raw(style_raw) {
        BorderedBox => container::bordered_box,
        RoundedBox => container::rounded_box,
        Transparent => container::transparent,
    };
    Box::into_raw(Box::new(container.style(style_fn)))
}

#[no_mangle]
extern "C" fn container_style_custom(self_ptr: SelfPtr, callback: StyleCallback) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(container.style(move |theme| {
        let theme_raw = crate::theme::theme_to_raw(theme);
        let mut style = Style::default();
        callback(&mut style, theme_raw);
        style
    })))
}

#[no_mangle]
extern "C" fn container_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(container.width(width)))
}

#[no_mangle]
extern "C" fn container_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let container = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(container.height(height)))
}

#[no_mangle]
extern "C" fn container_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let container = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(container.into()))
}

#[no_mangle]
extern "C" fn container_style_set_background(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.background = Some(Background::Color(color));
}

#[no_mangle]
extern "C" fn container_style_set_border(
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
extern "C" fn container_style_set_text_color(style: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    style.text_color = Some(color);
}
