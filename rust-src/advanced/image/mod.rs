use std::ffi::{c_float, c_uchar};

use iced::advanced::image::{Handle, Image};

use crate::ffi::{from_raw, into_raw, read_c_bool};

mod handle;

type SelfPtr = *mut Image<Handle>;

#[no_mangle]
extern "C" fn advanced_image_new(handle_ptr: *mut Handle) -> SelfPtr {
    let handle = from_raw(handle_ptr);
    into_raw(Image::new(handle))
}

#[no_mangle]
extern "C" fn advanced_image_opacity(self_ptr: SelfPtr, opacity: c_float) -> SelfPtr {
    let image = from_raw(self_ptr);
    into_raw(image.opacity(opacity))
}

#[no_mangle]
extern "C" fn advanced_image_rotation(self_ptr: SelfPtr, radians: c_float) -> SelfPtr {
    let image = from_raw(self_ptr);
    into_raw(image.rotation(radians))
}

#[no_mangle]
extern "C" fn advanced_image_snap(self_ptr: SelfPtr, snap_raw: c_uchar) -> SelfPtr {
    let image = from_raw(self_ptr);
    let snap = read_c_bool(snap_raw);
    into_raw(image.snap(snap))
}
