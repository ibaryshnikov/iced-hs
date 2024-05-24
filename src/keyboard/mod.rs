use std::ffi::c_int;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use iced::{keyboard, Subscription};
use keyboard::{Key, Modifiers, PhysicalKey};

use crate::{free_haskell_fun_ptr, IcedMessage, Message};

mod key;
mod physical;

type OnKey = extern "C" fn(key: c_int, modifiers: c_int) -> Message;

struct HaskellClosure {
    on_key: OnKey,
}

impl Drop for HaskellClosure {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.on_key as usize) };
    }
}

#[derive(Clone)]
struct Closure {
    label: &'static str,
    internal: Arc<HaskellClosure>,
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.label.hash(state);
    }
}

impl Closure {
    fn new(label: &'static str, on_key: OnKey) -> Self {
        let internal = Arc::new(HaskellClosure { on_key });
        Closure { label, internal }
    }
    fn call(&self, event: Event) -> IcedMessage {
        // let logical = match event.logical_key {
        //     Key::Named(named) => key::named_to_int(named),
        //     Key::Character(_) => return IcedMessage::None,
        //     Key::Unidentified => return IcedMessage::None,
        // };
        let physical = match event.physical_key {
            PhysicalKey::Code(code) => physical::key_code_to_int(code),
            PhysicalKey::Unidentified => return IcedMessage::None,
        };

        let message_ptr = (self.internal.on_key)(physical, 0);
        IcedMessage::ptr(message_ptr)
    }
}

struct Event {
    logical_key: Key,
    physical_key: PhysicalKey,
    modifiers: Modifiers,
}

fn make_key_event(
    logical_key: Key,
    physical_key: PhysicalKey,
    modifiers: Modifiers,
) -> Option<Event> {
    Some(Event {
        logical_key,
        physical_key,
        modifiers,
    })
}

#[no_mangle]
extern "C" fn keyboard_on_key_press(on_press: OnKey) -> *mut Subscription<IcedMessage> {
    let on_press = Closure::new("keyboard_on_press", on_press);
    let subscription = keyboard::on_key_press(make_key_event)
        .with(on_press)
        .map(|(on_press, event)| on_press.call(event));
    Box::into_raw(Box::new(subscription))
}

#[no_mangle]
extern "C" fn keyboard_on_key_release(on_release: OnKey) -> *mut Subscription<IcedMessage> {
    let on_release = Closure::new("keyboard_on_release", on_release);
    let subscription = keyboard::on_key_release(make_key_event)
        .with(on_release)
        .map(|(on_release, event)| on_release.call(event));
    Box::into_raw(Box::new(subscription))
}
