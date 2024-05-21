use std::hash::{Hash, Hasher};
use std::sync::Arc;

use iced::{keyboard, Subscription};
use keyboard::{Key, Modifiers};

use crate::{free_haskell_fun_ptr, IcedMessage, Message};

type OnKeyPress = extern "C" fn(key: u16, modifiers: u16) -> Message;

struct HaskellClosure {
    on_press: OnKeyPress,
}

impl Drop for HaskellClosure {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.on_press as usize) };
    }
}

#[derive(Clone)]
struct Closure {
    internal: Arc<HaskellClosure>,
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        "keyboard_on_press".hash(state);
    }
}

impl Closure {
    fn new(on_press: OnKeyPress) -> Self {
        let internal = Arc::new(HaskellClosure { on_press });
        Closure { internal }
    }
    fn call(&self, _event: Event) -> IcedMessage {
        let message_ptr = (self.internal.on_press)(0, 0);
        IcedMessage::ptr(message_ptr)
    }
}

struct Event {
    key: Key,
    modifiers: Modifiers,
}

#[no_mangle]
extern "C" fn keyboard_on_key_press(on_press: OnKeyPress) -> *mut Subscription<IcedMessage> {
    let on_press = Closure::new(on_press);
    let subscription = keyboard::on_key_press(|key, modifiers| Some(Event { key, modifiers }))
        .with(on_press)
        .map(|(on_press, event)| on_press.call(event));
    Box::into_raw(Box::new(subscription))
}
