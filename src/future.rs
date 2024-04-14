use std::future::Future;
use std::pin::Pin;

use crate::IcedMessage;

pub type PinnedFuture = Pin<Box<dyn Future<Output = IcedMessage> + Send>>;
