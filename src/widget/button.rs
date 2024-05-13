use std::ffi::{c_char, c_float};

use button::Style;
use iced::widget::{button, text, Button};
use iced::{Background, Border, Color, Length, Padding, Theme};

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut Button<'static, IcedMessage>;

#[no_mangle]
extern "C" fn button_new(input: *mut c_char) -> SelfPtr {
    let string = read_c_string(input);
    let button = button(text(string));
    Box::into_raw(Box::new(button))
}

#[no_mangle]
extern "C" fn button_on_press(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    Box::into_raw(Box::new(button.on_press(message)))
}

#[no_mangle]
extern "C" fn button_padding(
    self_ptr: SelfPtr,
    top: c_float,
    right: c_float,
    bottom: c_float,
    left: c_float,
) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let padding = Padding {
        top,
        right,
        bottom,
        left,
    };
    Box::into_raw(Box::new(button.padding(padding)))
}

#[no_mangle]
extern "C" fn button_style(self_ptr: SelfPtr, style_ptr: *mut ButtonStyle) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let style = unsafe { *Box::from_raw(style_ptr) };
    let base = style.active_hs;
    Box::into_raw(Box::new(button.style(move |_theme, status| match status {
        button::Status::Active => base,
        button::Status::Hovered => {
            if let Some(hovered) = style.hovered_hs {
                hovered
            } else {
                base
            }
        }
        button::Status::Pressed => {
            if let Some(pressed) = style.pressed_hs {
                pressed
            } else {
                base
            }
        }
        button::Status::Disabled => {
            if let Some(disabled) = style.disabled_hs {
                disabled
            } else {
                base
            }
        }
    })))
}

#[no_mangle]
extern "C" fn button_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(button.width(width)))
}

#[no_mangle]
extern "C" fn button_height(self_ptr: SelfPtr, height: *mut Length) -> SelfPtr {
    let button = unsafe { Box::from_raw(self_ptr) };
    let height = unsafe { *Box::from_raw(height) };
    Box::into_raw(Box::new(button.height(height)))
}

#[no_mangle]
extern "C" fn button_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let button = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(button.into()))
}

#[derive(Default)]
struct ButtonStyle {
    active_hs: Style,
    hovered_hs: Option<Style>,
    pressed_hs: Option<Style>,
    disabled_hs: Option<Style>,
}

#[no_mangle]
extern "C" fn button_appearance_new() -> *mut Style {
    let appearance = button::primary(&Theme::Light, button::Status::Active);
    Box::into_raw(Box::new(appearance))
}

#[no_mangle]
extern "C" fn button_appearance_clone(appearance: &Style) -> *mut Style {
    Box::into_raw(Box::new(*appearance))
}

#[no_mangle]
extern "C" fn button_appearance_free(ptr: *mut Style) {
    let _ = unsafe { Box::from_raw(ptr) };
}

#[no_mangle]
extern "C" fn button_appearance_set_background(appearance: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    appearance.background = Some(Background::Color(color));
}

#[no_mangle]
extern "C" fn button_appearance_set_border(
    appearance: &mut Style,
    color_ptr: *mut Color,
    width: c_float,
    radius: c_float,
) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    appearance.border = Border {
        color,
        width,
        radius: radius.into(),
    }
}

#[no_mangle]
extern "C" fn button_appearance_set_text_color(appearance: &mut Style, color_ptr: *mut Color) {
    let color = unsafe { *Box::from_raw(color_ptr) };
    appearance.text_color = color;
}

#[no_mangle]
extern "C" fn button_stylesheet_new(pointer: *mut Style) -> *mut ButtonStyle {
    let active_hs = unsafe { *Box::from_raw(pointer) };
    let stylesheet = ButtonStyle {
        active_hs,
        ..Default::default()
    };
    Box::into_raw(Box::new(stylesheet))
}

#[no_mangle]
extern "C" fn button_stylesheet_set_hovered(stylesheet: &mut ButtonStyle, pointer: *mut Style) {
    let appearance = unsafe { *Box::from_raw(pointer) };
    stylesheet.hovered_hs = Some(appearance)
}

#[no_mangle]
extern "C" fn button_stylesheet_set_pressed(stylesheet: &mut ButtonStyle, pointer: *mut Style) {
    let appearance = unsafe { *Box::from_raw(pointer) };
    stylesheet.pressed_hs = Some(appearance)
}

#[no_mangle]
extern "C" fn button_stylesheet_set_disabled(stylesheet: &mut ButtonStyle, pointer: *mut Style) {
    let appearance = unsafe { *Box::from_raw(pointer) };
    stylesheet.disabled_hs = Some(appearance)
}
