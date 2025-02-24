use std::ffi::c_ulong;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::{Duration, Instant};

use iced::Subscription;

use crate::future::RawFuture;
use crate::{free_haskell_fun_ptr, IcedMessage, Message};

static STARTED_AT: OnceLock<Instant> = OnceLock::new();

type OnEvery = extern "C" fn(micros: c_ulong) -> Message;

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
extern "C" fn duration_from_micros(value: c_ulong) -> *mut Duration {
    let duration = Duration::from_micros(value);
    Box::into_raw(Box::new(duration))
}

#[no_mangle]
extern "C" fn duration_from_nanos(value: c_ulong) -> *mut Duration {
    let duration = Duration::from_nanos(value);
    Box::into_raw(Box::new(duration))
}

#[no_mangle]
extern "C" fn tokio_time_sleep(duration_ptr: *mut Duration) -> RawFuture<()> {
    let duration = unsafe { *Box::from_raw(duration_ptr) };
    let pinned = Box::pin(tokio::time::sleep(duration));
    Box::into_raw(Box::new(pinned))
}

fn time_passed_micros(instant: Instant) -> u64 {
    let start = STARTED_AT.get_or_init(Instant::now);
    instant.duration_since(*start).as_micros() as u64
}

struct HaskellClosure {
    on_every: OnEvery,
}

impl Drop for HaskellClosure {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.on_every as usize) };
    }
}

#[derive(Clone)]
struct Closure {
    internal: Arc<HaskellClosure>,
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        "time_on_every".hash(state);
    }
}

impl Closure {
    fn new(on_every: OnEvery) -> Self {
        let internal = Arc::new(HaskellClosure { on_every });
        Closure { internal }
    }
    fn call(&self, micros: u64) -> IcedMessage {
        let message_ptr = (self.internal.on_every)(micros);
        IcedMessage::ptr(message_ptr)
    }
}

#[no_mangle]
extern "C" fn iced_time_every(
    duration_ptr: *mut Duration,
    on_every: OnEvery,
) -> *mut Subscription<IcedMessage> {
    let duration = unsafe { *Box::from_raw(duration_ptr) };
    let on_every = Closure::new(on_every);
    let every = iced::time::every(duration)
        .with(on_every)
        .map(|(on_every, instant)| {
            let micros = time_passed_micros(instant);
            on_every.call(micros)
        });
    Box::into_raw(Box::new(every))
}

#[no_mangle]
extern "C" fn time_micros_since_start() -> c_ulong {
    time_passed_micros(Instant::now())
}
