use iced::Alignment;

#[no_mangle]
pub extern "C" fn alignment_start() -> *mut Alignment {
    Box::into_raw(Box::new(Alignment::Start))
}

#[no_mangle]
pub extern "C" fn alignment_center() -> *mut Alignment {
    Box::into_raw(Box::new(Alignment::Center))
}

#[no_mangle]
pub extern "C" fn alignment_end() -> *mut Alignment {
    Box::into_raw(Box::new(Alignment::End))
}
