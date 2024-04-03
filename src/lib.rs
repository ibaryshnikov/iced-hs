use std::ffi::c_char;

use iced::{Application, Command, Element, Settings, Theme};

mod alignment;
mod length;
mod settings;
mod widget;

type Model = *const u8;
type Message = *const u8;
type Update = unsafe extern "C" fn(model: Model, message: Message) -> Model;
type View = unsafe extern "C" fn(model: Model) -> widget::ElementPtr;

#[derive(Debug, Clone)]
pub struct HaskellMessage {
    ptr: *const u8,
}
unsafe impl Send for HaskellMessage {}
unsafe impl Sync for HaskellMessage {}

#[derive(Clone, Debug)]
pub enum IcedMessage {
    Ptr(HaskellMessage),
}

struct Flags {
    title: String,
    model: Model,
    update: Update,
    view: View,
}

struct App {
    title: String,
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
                title: flags.title,
                model: flags.model,
                update_hs: flags.update,
                view_hs: flags.view,
            },
            Command::none(),
        )
    }
    fn title(&self) -> String {
        self.title.clone()
    }
    fn update(&mut self, message: IcedMessage) -> Command<IcedMessage> {
        match message {
            IcedMessage::Ptr(message) => {
                self.model = unsafe { (self.update_hs)(self.model, message.ptr) };
            }
        }

        Command::none()
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

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn run_app(
    settings_ptr: *mut Settings<()>,
    title_ptr: *mut c_char,
    model: Model,
    maybe_update: Option<Update>,
    maybe_view: Option<View>,
) {
    let Some(update) = maybe_update else {
        panic!("Update callback is NULL");
    };
    let Some(view) = maybe_view else {
        panic!("View callback is NULL");
    };
    let settings = unsafe { *Box::from_raw(settings_ptr) };
    let title = widget::c_string_to_rust(title_ptr);
    let flags = Flags {
        title,
        model,
        update,
        view,
    };
    let settings = make_settings(settings, flags);
    App::run(settings).expect("Should run the app")
}
