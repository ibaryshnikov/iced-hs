use std::ffi::c_char;
use std::sync::Arc;

use iced::{Application, Command, Element, Settings, Theme};

mod alignment;
mod command;
mod future;
mod length;
mod line_height;
mod settings;
mod time;
mod widget;

use command::UpdateResult;
use widget::read_c_string;

type Model = *const u8;
type Message = *const u8;
type Title = unsafe extern "C" fn(model: Model) -> *mut c_char;
type Update = unsafe extern "C" fn(model: Model, message: Message) -> *mut UpdateResult;
type View = unsafe extern "C" fn(model: Model) -> widget::ElementPtr;

extern "C" {
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
}

struct App {
    title_hs: Title,
    model: Model,
    update_hs: Update,
    view_hs: View,
}

impl Application for App {
    type Executor = iced::executor::Default;
    type Message = IcedMessage;
    type Theme = Theme;
    type Flags = Flags;

    fn new(flags: Flags) -> (App, Command<IcedMessage>) {
        (
            App {
                title_hs: flags.title,
                model: flags.model,
                update_hs: flags.update,
                view_hs: flags.view,
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
            IcedMessage::Ptr(message) => {
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
            IcedMessage::None => Command::none(),
        }
    }
    fn view(&self) -> Element<IcedMessage> {
        unsafe { *Box::from_raw((self.view_hs)(self.model)) }
    }
}

fn make_settings(settings: Settings<()>, flags: Flags) -> Settings<Flags> {
    Settings {
        flags,
        id: settings.id,
        window: settings.window,
        fonts: settings.fonts,
        default_font: settings.default_font,
        default_text_size: settings.default_text_size,
        antialiasing: true,
    }
}

#[no_mangle]
extern "C" fn run_app(
    settings_ptr: *mut Settings<()>,
    maybe_title: Option<Title>,
    model: Model,
    maybe_update: Option<Update>,
    maybe_view: Option<View>,
) {
    let Some(title) = maybe_title else {
        panic!("Title callback is NULL");
    };
    let Some(update) = maybe_update else {
        panic!("Update callback is NULL");
    };
    let Some(view) = maybe_view else {
        panic!("View callback is NULL");
    };
    let settings = unsafe { *Box::from_raw(settings_ptr) };
    let flags = Flags {
        title,
        model,
        update,
        view,
    };
    let settings = make_settings(settings, flags);
    App::run(settings).expect("Should run the app")
}
