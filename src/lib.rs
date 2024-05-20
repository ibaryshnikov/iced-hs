use std::ffi::c_char;
use std::sync::Arc;

use iced::advanced::Application;
use iced::{Command, Element, Renderer, Settings, Subscription, Theme};

mod alignment;
mod color;
mod command;
mod future;
mod length;
mod line_height;
mod settings;
mod subscription;
mod time;
mod widget;

use command::UpdateResult;
use subscription::SubscriptionFn;
use widget::read_c_string;

type Model = *const u8;
type Message = *const u8;
type Title = unsafe extern "C" fn(model: Model) -> *mut c_char;
type Update = unsafe extern "C" fn(model: Model, message: Message) -> *mut UpdateResult;
type View = unsafe extern "C" fn(model: Model) -> widget::ElementPtr;

extern "C" {
    fn free_haskell_fun_ptr(ptr: usize);
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

struct Flags {
    title: Title,
    model: Model,
    update: Update,
    view: View,
    subscription: Option<SubscriptionFn>,
}

struct App {
    title_hs: Title,
    model: Model,
    update_hs: Update,
    view_hs: View,
    subscription_hs: Option<SubscriptionFn>,
}

impl Application for App {
    type Executor = iced::executor::Default;
    type Message = IcedMessage;
    type Theme = Theme;
    type Renderer = Renderer;
    type Flags = Flags;

    fn new(flags: Flags) -> (App, Command<IcedMessage>) {
        (
            App {
                title_hs: flags.title,
                model: flags.model,
                update_hs: flags.update,
                view_hs: flags.view,
                subscription_hs: flags.subscription,
            },
            Command::none(),
        )
    }
    fn title(&self) -> String {
        let title_ptr = unsafe { (self.title_hs)(self.model) };
        read_c_string(title_ptr)
    }
    fn update(&mut self, message: IcedMessage) -> Command<IcedMessage> {
        match message {
            IcedMessage::Ptr(message) => self.process_update(message),
            IcedMessage::None => Command::none(),
        }
    }
    fn view(&self) -> Element<IcedMessage> {
        unsafe { *Box::from_raw((self.view_hs)(self.model)) }
    }
    fn subscription(&self) -> Subscription<IcedMessage> {
        match self.subscription_hs {
            Some(subscription) => {
                let pointer = unsafe { subscription(self.model) };
                unsafe { *Box::from_raw(pointer) }
            }
            None => Subscription::none(),
        }
    }
}

impl App {
    fn process_update(&mut self, message: Arc<HaskellMessage>) -> Command<IcedMessage> {
        let result_ptr = unsafe { (self.update_hs)(self.model, message.ptr) };
        let result = unsafe { Box::from_raw(result_ptr) };
        // when pointer changes, free the old one
        // in fact, it almost always changes
        if self.model != result.model {
            let old_ptr = self.model;
            self.model = result.model;
            unsafe { free_stable_ptr(old_ptr) }
        }
        result.command.perform()
    }
}

fn add_flags_to_settings(flags: Flags, settings: Settings<()>) -> Settings<Flags> {
    Settings {
        flags,
        id: settings.id,
        window: settings.window,
        fonts: settings.fonts,
        default_font: settings.default_font,
        default_text_size: settings.default_text_size,
        antialiasing: settings.antialiasing,
    }
}

#[no_mangle]
extern "C" fn app_flags_new(
    maybe_title: Option<Title>,
    model: Model,
    maybe_update: Option<Update>,
    maybe_view: Option<View>,
) -> *mut Flags {
    let Some(title) = maybe_title else {
        panic!("Title callback is NULL");
    };
    let Some(update) = maybe_update else {
        panic!("Update callback is NULL");
    };
    let Some(view) = maybe_view else {
        panic!("View callback is NULL");
    };
    let flags = Flags {
        title,
        model,
        update,
        view,
        subscription: None,
    };
    Box::into_raw(Box::new(flags))
}

#[no_mangle]
extern "C" fn app_flags_set_subscription(flags: &mut Flags, subscription: SubscriptionFn) {
    flags.subscription = Some(subscription);
}

#[no_mangle]
extern "C" fn app_run(flags_ptr: *mut Flags, settings_ptr: *mut Settings<()>) {
    let flags = unsafe { *Box::from_raw(flags_ptr) };
    let settings = unsafe { *Box::from_raw(settings_ptr) };
    let settings = add_flags_to_settings(flags, settings);
    App::run(settings).expect("Should run the app")
}
