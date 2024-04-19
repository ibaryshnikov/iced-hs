use iced::Command;

use crate::future::PinnedFuture;
use crate::{IcedMessage, Message, Model};

pub type CommandCallback = unsafe extern "C" fn() -> Message;

pub struct UpdateResult {
    pub model: Model,
    pub command: CommandKind,
}

pub enum CommandKind {
    IO(CommandCallback),
    Future(PinnedFuture),
    None,
}

impl CommandKind {
    pub fn perform(self) -> Command<IcedMessage> {
        match self {
            CommandKind::IO(callback) => perform_io(callback),
            CommandKind::Future(future) => Command::perform(future, |message| message),
            CommandKind::None => Command::none(),
        }
    }
}

fn perform_io(callback: CommandCallback) -> Command<IcedMessage> {
    let (sender, receiver) = tokio::sync::oneshot::channel();
    let _handle = std::thread::spawn(move || {
        let message_ptr = unsafe { callback() };
        let message = IcedMessage::ptr(message_ptr);
        if sender.send(message).is_err() {
            println!("Error writing to channel in Command::perform");
        }
    });
    let future = async move {
        receiver.await.unwrap_or_else(|e| {
            println!("Error reading from channel in Command::perform: {}", e);
            IcedMessage::None
        })
    };
    Command::perform(future, |message| message)
}

#[no_mangle]
extern "C" fn update_result_new(model: Model) -> *mut UpdateResult {
    Box::into_raw(Box::new(UpdateResult {
        model,
        command: CommandKind::None,
    }))
}

#[no_mangle]
extern "C" fn update_result_add_command_io(
    result: &mut UpdateResult,
    maybe_callback: Option<CommandCallback>,
) {
    let Some(callback) = maybe_callback else {
        return;
    };
    result.command = CommandKind::IO(callback);
}

#[no_mangle]
extern "C" fn update_result_add_command_future(
    result: &mut UpdateResult,
    future_ptr: *mut PinnedFuture,
) {
    let future = unsafe { *Box::from_raw(future_ptr) };
    result.command = CommandKind::Future(future);
}
