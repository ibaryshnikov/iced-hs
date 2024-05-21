use iced::Subscription;

use crate::{IcedMessage, Model};

pub type SubscriptionFn = extern "C" fn(model: Model) -> SelfPtr;

type SelfPtr = *mut Subscription<IcedMessage>;

#[no_mangle]
extern "C" fn iced_subscription_none() -> SelfPtr {
    Box::into_raw(Box::new(Subscription::none()))
}

#[no_mangle]
extern "C" fn iced_subscription_batch(len: usize, ptr: *const SelfPtr) -> SelfPtr {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let mut items = Vec::new();
    for item in slice {
        let boxed = unsafe { Box::from_raw(*item) };
        items.push(*boxed);
    }
    Box::into_raw(Box::new(Subscription::batch(items)))
}
