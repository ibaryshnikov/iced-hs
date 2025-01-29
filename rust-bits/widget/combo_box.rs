use std::ffi::{c_char, c_float};

use combo_box::State;
use iced::widget::text::LineHeight;
use iced::widget::{combo_box, ComboBox};
use iced::{Length, Padding};

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut ComboBox<'static, String, IcedMessage>;
type StatePtr = *mut State<String>;

type OnSelectFFI = extern "C" fn(selected: *mut c_char) -> *const u8;
type OnInputFFI = extern "C" fn(input: *mut c_char) -> *const u8;
type OnOptionHoveredFFI = extern "C" fn(selected: *mut c_char) -> *const u8;

#[no_mangle]
extern "C" fn combo_box_state_new(
    len: usize,
    options_ptr: *const *mut c_char, // array of CString
) -> StatePtr {
    let options = super::read_array_of_c_strings(len, options_ptr);
    let state = State::new(options);
    Box::into_raw(Box::new(state))
}

#[no_mangle]
extern "C" fn combo_box_state_free(state_ptr: StatePtr) {
    let _ = unsafe { Box::from_raw(state_ptr) };
}

#[no_mangle]
extern "C" fn combo_box_new(
    state_ptr: StatePtr,
    placeholder_ptr: *mut c_char, // CString
    selected_ptr: *mut c_char,    // CString
    on_select_ffi: OnSelectFFI,
) -> SelfPtr {
    let state = unsafe { Box::from_raw(state_ptr) };
    let placeholder = read_c_string(placeholder_ptr);
    let selected = super::read_c_string_to_option(selected_ptr);
    let on_select = super::wrap_callback_with_string(on_select_ffi);
    // track State in Haskell
    let combo_box = combo_box(Box::leak(state), &placeholder, selected.as_ref(), on_select);
    Box::into_raw(Box::new(combo_box))
}

#[no_mangle]
extern "C" fn combo_box_line_height(
    self_ptr: SelfPtr,
    line_height_ptr: *mut LineHeight,
) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let line_height = unsafe { *Box::from_raw(line_height_ptr) };
    Box::into_raw(Box::new(combo_box.line_height(line_height)))
}

#[no_mangle]
extern "C" fn combo_box_on_close(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    Box::into_raw(Box::new(combo_box.on_close(message)))
}

#[no_mangle]
extern "C" fn combo_box_on_input(self_ptr: SelfPtr, on_input_ffi: OnInputFFI) -> SelfPtr {
    let combo_box = unsafe { *Box::from_raw(self_ptr) };
    let on_input = super::wrap_callback_with_string(on_input_ffi);
    Box::into_raw(Box::new(combo_box.on_input(on_input)))
}

#[no_mangle]
extern "C" fn combo_box_on_option_hovered(
    self_ptr: SelfPtr,
    callback_ffi: OnOptionHoveredFFI,
) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let callback = super::wrap_callback_with_string(callback_ffi);
    Box::into_raw(Box::new(combo_box.on_option_hovered(callback)))
}

#[no_mangle]
extern "C" fn combo_box_padding(self_ptr: SelfPtr, padding_ptr: *mut Padding) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let padding = unsafe { *Box::from_raw(padding_ptr) };
    Box::into_raw(Box::new(combo_box.padding(padding)))
}

#[no_mangle]
extern "C" fn combo_box_size(self_ptr: SelfPtr, size: c_float) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(combo_box.size(size)))
}

#[no_mangle]
extern "C" fn combo_box_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(combo_box.width(width)))
}

#[no_mangle]
extern "C" fn combo_box_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let combo_box = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(combo_box.into()))
}
