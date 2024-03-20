use iced::{Application, Command, Element, Settings, Theme};

mod widgets;

type Model = *const u8;
type Message = *const u8;
type Update = unsafe extern "C" fn(model: Model, message: Message) -> Model;
type View = unsafe extern "C" fn(model: Model) -> widgets::ElementPtr;

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
    model: Model,
    update: Update,
    view: View,
}

struct App {
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
                model: flags.model,
                update_hs: flags.update,
                view_hs: flags.view,
            },
            Command::none(),
        )
    }
    fn title(&self) -> String {
        "Hello from Iced".to_owned()
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

#[no_mangle]
pub extern "C" fn run_app(model: Model, maybe_update: Option<Update>, maybe_view: Option<View>) {
    let Some(update) = maybe_update else {
        panic!("Update callback is NULL");
    };
    let Some(view) = maybe_view else {
        panic!("View callback is NULL");
    };
    let flags = Flags {
        model,
        update,
        view,
    };
    let mut settings = Settings::with_flags(flags);
    settings.antialiasing = true;
    App::run(settings).expect("Should run the app")
}
