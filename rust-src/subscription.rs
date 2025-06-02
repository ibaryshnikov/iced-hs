use iced::Subscription;

use crate::{free_haskell_fun_ptr, IcedMessage, Model};

#[repr(transparent)]
pub struct SubscriptionFn {
    pub inner: extern "C" fn(model: Model) -> SelfPtr,
}

impl Drop for SubscriptionFn {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

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
