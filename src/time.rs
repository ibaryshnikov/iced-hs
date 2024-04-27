use std::ffi::c_ulong;
use std::time::Duration;

use crate::future::RawFuture;

#[no_mangle]
extern "C" fn duration_from_secs(value: c_ulong) -> *mut Duration {
    let duration = Duration::from_secs(value);
    Box::into_raw(Box::new(duration))
}

#[no_mangle]
extern "C" fn duration_from_millis(value: c_ulong) -> *mut Duration {
    let duration = Duration::from_millis(value);
    Box::into_raw(Box::new(duration))
}

#[no_mangle]
extern "C" fn tokio_time_sleep(duration_ptr: *mut Duration) -> RawFuture<()> {
    let duration = unsafe { *Box::from_raw(duration_ptr) };
    let pinned = Box::pin(tokio::time::sleep(duration));
    Box::into_raw(Box::new(pinned))
}
