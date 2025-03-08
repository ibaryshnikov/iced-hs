use std::ffi::c_char;
use std::sync::Arc;

use graphics::compositor;
use iced::{window, Element, Renderer, Settings, Subscription, Task, Theme};
use iced_widget::graphics;
use iced_winit::Program;

mod alignment;
mod color;
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

use ffi::{from_raw, into_raw};
use subscription::SubscriptionFn;
use task::UpdateResult;
use theme::{theme_from_raw, ThemeFn};
use widget::read_c_string;

type Model = *const u8;
type Message = *const u8;
type Title = extern "C" fn(model: Model) -> *mut c_char;
type Update = extern "C" fn(model: Model, message: Message) -> *mut UpdateResult;
type View = extern "C" fn(model: Model) -> widget::ElementPtr;

extern "C" {
    #[link_name = "hs_free_fun_ptr"]
    fn free_haskell_fun_ptr(ptr: usize);
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

struct App {
    title_hs: Title,
    model: Model,
    update_hs: Update,
    view_hs: View,
    subscription_hs: Option<SubscriptionFn>,
    theme_hs: Option<ThemeFn>,
}

impl Program for App {
    type Executor = iced::executor::Default;
    type Message = IcedMessage;
    type Theme = Theme;
    type Renderer = Renderer;
    type Flags = Self;

    fn new(app: Self) -> (App, Task<IcedMessage>) {
        (app, Task::none())
    }
    fn title(&self, _window: window::Id) -> String {
        let pointer = (self.title_hs)(self.model);
        read_c_string(pointer)
    }
    fn update(&mut self, message: Self::Message) -> Task<Self::Message> {
        match message {
            IcedMessage::Ptr(message) => self.process_update(message),
            IcedMessage::None => Task::none(),
        }
    }
    fn view(&self, _window: window::Id) -> Element<Self::Message, Self::Theme, Self::Renderer> {
        unsafe { *Box::from_raw((self.view_hs)(self.model)) }
    }
    fn theme(&self, _window: window::Id) -> Theme {
        match self.theme_hs {
            Some(theme_fn) => {
                let value = theme_fn(self.model);
                theme_from_raw(value)
            }
            None => Self::Theme::default(),
        }
    }
    fn subscription(&self) -> Subscription<IcedMessage> {
        match self.subscription_hs {
            Some(subscription) => {
                let pointer = subscription(self.model);
                unsafe { *Box::from_raw(pointer) }
            }
            None => Subscription::none(),
        }
    }
}

impl App {
    fn process_update(&mut self, message: Arc<HaskellMessage>) -> Task<IcedMessage> {
        let result_ptr = (self.update_hs)(self.model, message.ptr);
        let result = from_raw(result_ptr);
        // when pointer changes, free the old one
        // in fact, it almost always changes
        if self.model != result.model {
            let old_ptr = self.model;
            self.model = result.model;
            unsafe { free_stable_ptr(old_ptr) }
        }
        result.task.perform()
    }
}

#[no_mangle]
extern "C" fn app_new(title_hs: Title, model: Model, update_hs: Update, view_hs: View) -> *mut App {
    let app = App {
        title_hs,
        model,
        update_hs,
        view_hs,
        subscription_hs: None,
        theme_hs: None,
    };
    into_raw(app)
}

#[no_mangle]
extern "C" fn app_set_subscription(app: &mut App, subscription: SubscriptionFn) {
    app.subscription_hs = Some(subscription);
}

#[no_mangle]
extern "C" fn app_set_theme(app: &mut App, theme: ThemeFn) {
    app.theme_hs = Some(theme);
}

#[no_mangle]
extern "C" fn app_run(app_ptr: *mut App, settings_ptr: *mut Settings) {
    let app = from_raw(app_ptr);
    let settings = from_raw(settings_ptr);

    let renderer_settings = graphics::Settings {
        default_font: settings.default_font,
        default_text_size: settings.default_text_size,
        antialiasing: if settings.antialiasing {
            Some(graphics::Antialiasing::MSAAx4)
        } else {
            None
        },
    };

    let window_settings = Some(window::Settings::default());

    iced_winit::program::run::<App, <Renderer as compositor::Default>::Compositor>(
        settings.into(),
        renderer_settings,
        window_settings,
        app,
    )
    .expect("Should run the app")
}
