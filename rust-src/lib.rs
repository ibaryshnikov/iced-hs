use std::ffi::c_char;
use std::sync::Arc;

use iced::{Element, Settings, Subscription, Task, Theme};

mod advanced;
mod alignment;
mod color;
mod core;
mod ffi;
mod future;
mod keyboard;
mod length;
mod line_height;
mod padding;
mod settings;
mod subscription;
mod task;
mod theme;
mod time;
mod widget;

use core::ElementPtr;
use ffi::{from_raw, into_raw, read_c_string};
use subscription::SubscriptionFn;
use task::UpdateResult;
use theme::{theme_from_raw, ThemeFn};

type ModelInner = *const u8;
type Message = *const u8;

#[repr(transparent)]
struct Model {
    inner: ModelInner,
}

unsafe impl Send for Model {}
unsafe impl Sync for Model {}

impl Drop for Model {
    fn drop(&mut self) {
        unsafe { free_stable_ptr(self.inner) }
    }
}

#[repr(transparent)]
struct Title {
    inner: extern "C" fn(model: ModelInner) -> *mut c_char,
}

impl Drop for Title {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

#[repr(transparent)]
struct Update {
    inner: extern "C" fn(model: ModelInner, message: Message) -> *mut UpdateResult,
}

impl Drop for Update {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

#[repr(transparent)]
struct View {
    inner: extern "C" fn(model: ModelInner) -> ElementPtr,
}

impl Drop for View {
    fn drop(&mut self) {
        unsafe { free_haskell_fun_ptr(self.inner as usize) };
    }
}

extern "C" {
    // Part of HsFFI.h
    #[link_name = "hs_free_fun_ptr"]
    fn free_haskell_fun_ptr(ptr: usize);
    // Part of HsFFI.h
    #[link_name = "hs_free_stable_ptr"]
    fn free_stable_ptr(ptr: *const u8);
}

#[derive(Debug, Clone)]
struct HaskellMessage {
    ptr: *const u8,
}
unsafe impl Send for HaskellMessage {}
unsafe impl Sync for HaskellMessage {}

impl Drop for HaskellMessage {
    fn drop(&mut self) {
        unsafe { free_stable_ptr(self.ptr) }
    }
}

#[derive(Clone, Debug)]
enum IcedMessage {
    Ptr(Arc<HaskellMessage>),
    None,
}

impl IcedMessage {
    // StablePtr to Haskell message
    fn ptr(ptr: *const u8) -> Self {
        let message = HaskellMessage { ptr };
        IcedMessage::Ptr(Arc::new(message))
    }
}

#[derive(Clone)]
struct App {
    title_hs: Arc<Title>,
    model: Arc<Model>,
    update_hs: Arc<Update>,
    view_hs: Arc<View>,
    subscription_hs: Option<Arc<SubscriptionFn>>,
    theme_hs: Option<Arc<ThemeFn>>,
}

impl App {
    fn title(&self) -> String {
        let pointer = (self.title_hs.inner)(self.model.inner);
        read_c_string(pointer)
    }
    fn update(&mut self, message: IcedMessage) -> Task<IcedMessage> {
        match message {
            IcedMessage::Ptr(message) => self.process_update(message),
            IcedMessage::None => Task::none(),
        }
    }
    fn view(&self) -> Element<'_, IcedMessage> {
        unsafe { *Box::from_raw((self.view_hs.inner)(self.model.inner)) }
    }
    fn theme(&self) -> Theme {
        match &self.theme_hs {
            Some(theme_fn) => {
                let value = (theme_fn.inner)(self.model.inner);
                theme_from_raw(value)
            }
            None => Theme::Light,
        }
    }
    fn subscription(&self) -> Subscription<IcedMessage> {
        match &self.subscription_hs {
            Some(subscription) => {
                let pointer = (subscription.inner)(self.model.inner);
                unsafe { *Box::from_raw(pointer) }
            }
            None => Subscription::none(),
        }
    }
}

impl App {
    fn process_update(&mut self, message: Arc<HaskellMessage>) -> Task<IcedMessage> {
        let result_ptr = (self.update_hs.inner)(self.model.inner, message.ptr);
        let result = from_raw(result_ptr);
        // When pointer changes, drop the Arc<Model>,
        // and when the reference count goes to 0 the Model will be freed.
        // In fact, it almost always changes
        if self.model.inner != result.model {
            self.model = Arc::new(Model {
                inner: result.model,
            });
        }
        result.task.perform()
    }
}

#[no_mangle]
extern "C" fn app_new(title_hs: Title, model: Model, update_hs: Update, view_hs: View) -> *mut App {
    let app = App {
        title_hs: Arc::new(title_hs),
        model: Arc::new(model),
        update_hs: Arc::new(update_hs),
        view_hs: Arc::new(view_hs),
        subscription_hs: None,
        theme_hs: None,
    };
    into_raw(app)
}

#[no_mangle]
extern "C" fn app_set_subscription(app: &mut App, subscription: SubscriptionFn) {
    app.subscription_hs = Some(Arc::new(subscription));
}

#[no_mangle]
extern "C" fn app_set_theme(app: &mut App, theme: ThemeFn) {
    app.theme_hs = Some(Arc::new(theme));
}

#[no_mangle]
extern "C" fn app_run(app_ptr: *mut App, settings_ptr: *mut Settings) {
    let app = from_raw(app_ptr);
    let settings = from_raw(settings_ptr);

    iced::application(move || app.clone(), App::update, App::view)
        .settings(settings)
        .title(App::title)
        .theme(App::theme)
        .subscription(App::subscription)
        .run()
        .expect("Should run the app")
}
