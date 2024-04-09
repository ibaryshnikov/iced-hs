use std::ffi::c_char;

use combo_box::State;
use iced::widget::{combo_box, ComboBox};
use iced::Length;

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut ComboBox<'static, String, IcedMessage>;
type StatePtr = *mut State<String>;

type OnSelect = unsafe extern "C" fn(selected: *mut c_char) -> *const u8;
type OnOptionHovered = unsafe extern "C" fn(selected: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn combo_box_state_new(
    len: usize,
    options_ptr: *const *mut c_char, // array of CString
) -> StatePtr {
    let options = super::read_array_of_c_strings(len, options_ptr);
    let state = State::new(options);
    Box::into_raw(Box::new(state))
}

#[no_mangle]
pub extern "C" fn combo_box_state_free(state_ptr: StatePtr) {
    let _ = unsafe { Box::from_raw(state_ptr) };
}

#[no_mangle]
pub extern "C" fn combo_box_new(
    state_ptr: StatePtr,
    placeholder_ptr: *mut c_char, // CString
    selected_ptr: *mut c_char,    // CString
    on_select_ffi: OnSelect,
) -> SelfPtr {
    let state = unsafe { Box::from_raw(state_ptr) };
    let placeholder = read_c_string(placeholder_ptr);
    let selected = super::read_c_string_to_option(selected_ptr);
    let on_select = super::make_callback_with_string_argument(on_select_ffi);
    // track State in Haskell
    let combo_box = combo_box(Box::leak(state), &placeholder, selected.as_ref(), on_select);
    Box::into_raw(Box::new(combo_box))
}

#[no_mangle]
pub extern "C" fn combo_box_on_close(self_ptr: SelfPtr, message_ptr: *const u8) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    Box::into_raw(Box::new(combo_box.on_close(message)))
}

#[no_mangle]
pub extern "C" fn combo_box_on_option_hovered(
    self_ptr: SelfPtr,
    callback_ffi: OnOptionHovered,
) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let callback = super::make_callback_with_string_argument(callback_ffi);
    Box::into_raw(Box::new(combo_box.on_option_hovered(callback)))
}

#[no_mangle]
pub extern "C" fn combo_box_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let combo_box = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(combo_box.width(width)))
}

#[no_mangle]
pub extern "C" fn combo_box_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let combo_box = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(combo_box.into()))
}
