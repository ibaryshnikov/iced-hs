use std::ffi::{c_char, CString};

use iced::widget::{pick_list, PickList};

use super::{c_string_to_rust, ElementPtr, IcedMessage};

type SelfPtr = *mut PickList<'static, String, Vec<String>, String, IcedMessage>;

type OnSelect = unsafe extern "C" fn(selected: *mut c_char) -> *const u8;

#[no_mangle]
pub extern "C" fn pick_list_new(
    len: usize,
    options_ptr: *const *mut c_char, // array of CString
    selected_ptr: *mut c_char,       // CString
    on_select: OnSelect,
) -> SelfPtr {
    let selected = c_string_to_rust(selected_ptr);
    let selected_option = if selected.is_empty() {
        None
    } else {
        Some(selected)
    };

    let slice = unsafe { std::slice::from_raw_parts(options_ptr, len) };
    let mut options = vec![];
    for option_ptr in slice {
        let option = c_string_to_rust(*option_ptr);
        options.push(option);
    }
    let on_select_closure = move |selected_value| {
        let c_string = CString::new(selected_value).expect("Should create a CString");
        let string_ptr = c_string.into_raw();
        let message_ptr = unsafe { on_select(string_ptr) };
        // free CString
        let _ = unsafe { CString::from_raw(string_ptr) };
        IcedMessage::ptr(message_ptr)
    };
    let pick_list = pick_list(options, selected_option, on_select_closure);
    Box::into_raw(Box::new(pick_list))
}

#[no_mangle]
pub extern "C" fn pick_list_placeholder(
    self_ptr: SelfPtr,
    placeholder_ptr: *mut c_char,
) -> SelfPtr {
    let pick_list = unsafe { Box::from_raw(self_ptr) };
    let placeholder = c_string_to_rust(placeholder_ptr);
    Box::into_raw(Box::new(pick_list.placeholder(placeholder)))
}

#[no_mangle]
pub extern "C" fn pick_list_into_element(self_ptr: SelfPtr) -> ElementPtr {
    let pick_list = unsafe { *Box::from_raw(self_ptr) };
    Box::into_raw(Box::new(pick_list.into()))
}
