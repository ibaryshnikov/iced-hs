use std::ffi::c_ulong;
use std::time::Duration;

use crate::future::PinnedFuture;
use crate::{IcedMessage, Message};

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
extern "C" fn tokio_time_sleep(
    duration_ptr: *mut Duration,
    message_ptr: Message,
) -> *mut PinnedFuture {
    let duration = unsafe { *Box::from_raw(duration_ptr) };
    let message = IcedMessage::ptr(message_ptr);
    let future = async move {
        tokio::time::sleep(duration).await;
        message
    };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}
