use std::ffi::c_ulonglong;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::{Duration, Instant};

use iced::Subscription;

use crate::ffi::{from_raw, into_raw};
use crate::future::RawFuture;
use crate::{free_haskell_fun_ptr, IcedMessage, Message};

static STARTED_AT: OnceLock<Instant> = OnceLock::new();

#[repr(transparent)]
struct OnEvery {
    inner: extern "C" fn(micros: c_ulonglong) -> Message,
}

impl Drop for OnEvery {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

#[no_mangle]
extern "C" fn duration_from_secs(value: c_ulonglong) -> *mut Duration {
    into_raw(Duration::from_secs(value))
}

#[no_mangle]
extern "C" fn duration_from_millis(value: c_ulonglong) -> *mut Duration {
    into_raw(Duration::from_millis(value))
}

#[no_mangle]
extern "C" fn duration_from_micros(value: c_ulonglong) -> *mut Duration {
    into_raw(Duration::from_micros(value))
}

#[no_mangle]
extern "C" fn duration_from_nanos(value: c_ulonglong) -> *mut Duration {
    into_raw(Duration::from_nanos(value))
}

#[no_mangle]
extern "C" fn tokio_time_sleep(duration_ptr: *mut Duration) -> RawFuture<()> {
    let duration = from_raw(duration_ptr);
    into_raw(Box::pin(tokio::time::sleep(duration)))
}

fn time_passed_micros(instant: Instant) -> u64 {
    let start = STARTED_AT.get_or_init(Instant::now);
    instant.duration_since(*start).as_micros() as u64
}

#[derive(Clone)]
struct Closure {
    on_every: Arc<OnEvery>,
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        "time_on_every".hash(state);
    }
}

impl Closure {
    fn new(on_every: OnEvery) -> Self {
        Closure {
            on_every: Arc::new(on_every),
        }
    }
    fn call(&self, micros: u64) -> IcedMessage {
        let message_ptr = (self.on_every.inner)(micros);
        IcedMessage::ptr(message_ptr)
    }
}

#[no_mangle]
extern "C" fn iced_time_every(
    duration_ptr: *mut Duration,
    on_every: OnEvery,
) -> *mut Subscription<IcedMessage> {
    let duration = from_raw(duration_ptr);
    let on_every = Closure::new(on_every);
    let every = iced::time::every(duration)
        .with(on_every)
        .map(|(on_every, instant)| {
            let micros = time_passed_micros(instant);
            on_every.call(micros)
        });
    into_raw(every)
}

#[no_mangle]
extern "C" fn time_micros_since_start() -> c_ulonglong {
    time_passed_micros(Instant::now())
}
