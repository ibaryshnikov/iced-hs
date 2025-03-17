use crate::IcedMessage;

pub(crate) type Element = iced::Element<'static, IcedMessage>;
pub(crate) type ElementPtr = *mut Element;
