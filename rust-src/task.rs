use std::sync::Arc;

use iced::Task;

use crate::future::{PinnedFuture, RawFuture, StablePtr};
use crate::{free_haskell_fun_ptr, IcedMessage, Message, ModelInner};

#[repr(transparent)]
pub struct TaskCallback {
    inner: extern "C" fn() -> Message,
}

impl Drop for TaskCallback {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

pub struct UpdateResult {
    pub model: ModelInner,
    pub task: TaskKind,
}

pub enum TaskKind {
    IO(TaskCallback),
    Future(PinnedFuture<StablePtr>),
    None,
}

impl TaskKind {
    pub fn perform(self) -> Task<IcedMessage> {
        match self {
            TaskKind::IO(callback) => perform_io(callback),
            TaskKind::Future(future) => {
                Task::perform(future, |message| IcedMessage::ptr(message.ptr))
            }
            TaskKind::None => Task::none(),
        }
    }
}

fn perform_io(callback: TaskCallback) -> Task<IcedMessage> {
    let callback = Arc::new(callback);
    let handle = tokio::task::spawn_blocking(move || {
        let ptr = (callback.inner)();
        IcedMessage::ptr(ptr)
    });
    let future = async move {
        handle.await.unwrap_or_else(|e| {
            println!("Error joining spawn_blocking handle in Task::perform: {e}");
            IcedMessage::None
        })
    };
    Task::perform(future, |message| message)
}

#[no_mangle]
extern "C" fn update_result_new(model: ModelInner) -> *mut UpdateResult {
    Box::into_raw(Box::new(UpdateResult {
        model,
        task: TaskKind::None,
    }))
}

#[no_mangle]
extern "C" fn update_result_add_task_io(
    result: &mut UpdateResult,
    maybe_callback: Option<TaskCallback>,
) {
    let Some(callback) = maybe_callback else {
        return;
    };
    result.task = TaskKind::IO(callback);
}

#[no_mangle]
extern "C" fn update_result_add_task_future(
    result: &mut UpdateResult,
    future_ptr: RawFuture<StablePtr>,
) {
    let future = unsafe { *Box::from_raw(future_ptr) };
    result.task = TaskKind::Future(future);
}
