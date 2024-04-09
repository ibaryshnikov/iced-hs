use std::ffi::c_char;

use iced::widget::{pick_list, PickList};
use iced::Length;

use super::{read_c_string, ElementPtr, IcedMessage};

type SelfPtr = *mut PickList<'static, String, Vec<String>, String, IcedMessage>;

type OnSelect = unsafe extern "C" fn(selected: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn pick_list_new(
    len: usize,
    options_ptr: *const *mut c_char, // array of CString
    selected_ptr: *mut c_char,       // CString
    on_select_ffi: OnSelect,
) -> SelfPtr {
    let selected = super::read_c_string_to_option(selected_ptr);

    let options = super::read_array_of_c_strings(len, options_ptr);
    let on_select = super::make_callback_with_string_argument(on_select_ffi);
    let pick_list = pick_list(options, selected, on_select);
    Box::into_raw(Box::new(pick_list))
}

#[no_mangle]
pub extern "C" fn pick_list_placeholder(
    self_ptr: SelfPtr,
    placeholder_ptr: *mut c_char,
) -> SelfPtr {
    let pick_list = unsafe { Box::from_raw(self_ptr) };
    let placeholder = read_c_string(placeholder_ptr);
    Box::into_raw(Box::new(pick_list.placeholder(placeholder)))
}

#[no_mangle]
pub extern "C" fn pick_list_width(self_ptr: SelfPtr, width: *mut Length) -> SelfPtr {
    let pick_list = unsafe { Box::from_raw(self_ptr) };
    let width = unsafe { *Box::from_raw(width) };
    Box::into_raw(Box::new(pick_list.width(width)))
}

#[no_mangle]
pub extern "C" fn pick_list_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let pick_list = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(pick_list.into()))
}
