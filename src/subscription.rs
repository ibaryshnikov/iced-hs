use iced::Subscription;

use crate::{IcedMessage, Model};

pub type SubscriptionFn = unsafe extern "C" fn(model: Model) -> *mut Subscription<IcedMessage>;

#[no_mangle]
extern "C" fn iced_subscription_none() -> *mut Subscription<IcedMessage> {
    Box::into_raw(Box::new(Subscription::none()))
}
